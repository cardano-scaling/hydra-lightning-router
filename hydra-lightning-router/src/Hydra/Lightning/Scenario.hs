{-# LANGUAGE OverloadedStrings #-}

module Hydra.Lightning.Scenario (main) where

import Cardano.Api qualified as C
import CardanoClient (QueryPoint (QueryTip))
import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, catch, finally)
import Control.Lens (contramap, (^..), (^?))
import Control.Monad (guard)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse, commitTx))
import Hydra.Cardano.Api (Tx, mkScriptAddress, mkTxOutAutoBalance, mkTxOutDatumInline, signTx, toPlutusKeyHash)
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Backend (ChainBackend)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Cluster.Faucet (seedFromFaucet)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (Alice, AliceFunds, Bob, BobFunds, Carol, Faucet), alice, aliceSk, aliceVk, bob, bobSk, bobVk, carol, carolSk, carolVk)
import Hydra.Cluster.Scenarios (EndToEndLog (FromCardanoNode, FromFaucet, FromHydraNode), headIsInitializingWith, refuelIfNeeded, returnFundsToFaucet)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.HTLC.Data (Datum (Datum, hash, receiver, sender, timeout))
import Hydra.HTLC.Embed (htlcValidatorScript)
import Hydra.Invoice qualified as I
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx ()
import Hydra.Tx.ContestationPeriod qualified as CP
import HydraNode
  ( getProtocolParameters,
    input,
    requestCommitTx,
    send,
    waitMatch,
    withHydraNode,
  )
import Network.HTTP.Req (POST (POST), defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Network.HTTP.Req qualified as Req
import Network.Socket (withSocketsDo)
import Network.WebSockets
  ( Connection,
    receiveData,
    runClient,
    withPingThread,
  )
import PlutusTx.Builtins (toBuiltin)
import System.Timeout qualified as Timeout
import Test.Hydra.Prelude (around, describe, hspec, it, shouldBe, withTempDir)

-- | Single hydra-node where the commit is done using some wallet UTxO.
singlePartyCommitsFromExternal ::
  (ChainBackend backend) =>
  Tracer IO EndToEndLog ->
  FilePath ->
  FilePath ->
  backend ->
  [C.TxId] ->
  IO ()
singlePartyCommitsFromExternal tracer workDir workDir2 backend hydraScriptsTxId =
  ( `finally`
      do
        returnFundsToFaucet tracer backend Alice
        returnFundsToFaucet tracer backend AliceFunds
  )
    $ do
      refuelIfNeeded tracer backend Alice 25_000_000
      refuelIfNeeded tracer backend Bob 25_000_000
      refuelIfNeeded tracer backend Carol 25_000_000

      let contestationPeriod :: CP.ContestationPeriod = 100

      aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [Carol] contestationPeriod

      carolChainConfig <- chainConfigFor Carol workDir backend hydraScriptsTxId [Alice] contestationPeriod

      bobChainConfig <-
        chainConfigFor Bob workDir2 backend hydraScriptsTxId [Carol] contestationPeriod

      carolChainConfig2 <- chainConfigFor Carol workDir2 backend hydraScriptsTxId [Bob] contestationPeriod

      let hydraTracer = contramap FromHydraNode tracer

      blockTime <- Backend.getBlockTime backend

      (aliceWalletVk, aliceWalletSk) <- keysFor AliceFunds
      (bobWalletVk, bobWalletSk) <- keysFor BobFunds
      let port1 :: Int = 4002
      let port2 :: Int = 4004
      let timeoutVal :: Int = 100
      let path :: String = "/?history=no"
      let wsServer1 = connectWs timeoutVal port1 path
      let wsServer2 = connectWs timeoutVal port2 path
      I.UnsafePreImage preImage <- I.generatePreImage

      let hydraHead1 = withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [carolVk] [1, 2] $ \n1 ->
            withHydraNode hydraTracer carolChainConfig workDir 2 carolSk [aliceVk] [1, 2] $ \n2 -> do
              utxoToCommit <- seedFromFaucet backend aliceWalletVk (C.lovelaceToValue 12_000_000) (contramap FromFaucet tracer)
              wsServer1
              send n1 $ input "Init" []
              headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice, carol])

              res <-
                runReq defaultHttpConfig $
                  req
                    POST
                    (http "127.0.0.1" /: "commit")
                    (Req.ReqBodyJson utxoToCommit)
                    (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                    (port $ 4000 + 1)

              let DraftCommitTxResponse {commitTx} = responseBody res
              Backend.submitTransaction backend $ signTx aliceWalletSk commitTx

              requestCommitTx n2 mempty >>= Backend.submitTransaction backend

              aliceHeadUTxO <- waitMatch (20 * blockTime) n1 $ \v -> do
                guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
                guard $ v ^? key "tag" == Just "HeadIsOpen"
                pure $ v ^? key "utxo"
              aliceHeadUTxO `shouldBe` Just (Aeson.toJSON utxoToCommit)
              pparams <- getProtocolParameters n1
              lockTx <- buildLockTx pparams utxoToCommit preImage aliceWalletVk bobWalletVk
              let signedL2tx = signTx aliceWalletSk lockTx
              putStrLn $ renderTxWithUTxO utxoToCommit signedL2tx
              send n1 $ input "NewTx" ["transaction" Aeson..= signedL2tx]

              waitMatch 10 n2 $ \v -> do
                guard $ v ^? key "tag" == Just "SnapshotConfirmed"
                guard $
                  Aeson.toJSON signedL2tx
                    `elem` (v ^.. key "snapshot" . key "confirmed" . values)

      let hydraHead2 = withHydraNode hydraTracer bobChainConfig workDir2 3 bobSk [carolVk] [3, 4] $ \n3 ->
            withHydraNode hydraTracer carolChainConfig2 workDir2 4 carolSk [bobVk] [3, 4] $ \n4 -> do
              utxoToCommit2 <- seedFromFaucet backend bobWalletVk (C.lovelaceToValue 13_000_000) (contramap FromFaucet tracer)
              wsServer2
              send n3 $ input "Init" []
              headId <- waitMatch (20 * blockTime) n3 $ headIsInitializingWith (Set.fromList [bob, carol])
              res <-
                runReq defaultHttpConfig $
                  req
                    POST
                    (http "127.0.0.1" /: "commit")
                    (Req.ReqBodyJson utxoToCommit2)
                    (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                    (port $ 4000 + 3)

              let DraftCommitTxResponse {commitTx = commitTx2} = responseBody res
              Backend.submitTransaction backend $ signTx bobWalletSk commitTx2

              requestCommitTx n4 mempty >>= Backend.submitTransaction backend

              bobHeadUTxO <- waitMatch (20 * blockTime) n3 $ \v -> do
                guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
                guard $ v ^? key "tag" == Just "HeadIsOpen"
                pure $ v ^? key "utxo"
              bobHeadUTxO `shouldBe` Just (Aeson.toJSON utxoToCommit2)

      _ <- concurrently hydraHead1 hydraHead2
      pure ()
  where
    buildLockTx pparams utxo preImage sender' recipient' = do
      networkId <- Backend.queryNetworkId backend
      let scriptAddress = mkScriptAddress networkId htlcValidatorScript
      timeout <- posixFromUTCTime . addUTCTime 100 <$> getCurrentTime
      let sender = toPlutusKeyHash $ C.verificationKeyHash sender'
      let receiver = toPlutusKeyHash $ C.verificationKeyHash recipient'
      let d =
            Datum
              { hash = toBuiltin preImage,
                timeout,
                sender,
                receiver
              }

      let scriptOutput =
            mkTxOutAutoBalance
              pparams
              scriptAddress
              (C.lovelaceToValue 0)
              (mkTxOutDatumInline d)
              C.ReferenceScriptNone

      systemStart <- Backend.querySystemStart backend QueryTip
      eraHistory <- Backend.queryEraHistory backend QueryTip
      stakePools <- Backend.queryStakePools backend QueryTip
      case Backend.buildTransactionWithPParams' pparams systemStart eraHistory stakePools scriptAddress utxo [] [scriptOutput] Nothing of
        Left e -> error $ show e
        Right tx -> pure tx

    application con = do
      withPingThread con 30 (return ()) $ do
        _ <- Timeout.timeout 100 $ receiveInputs con
        pure ()
    -- sendClose con ("Bye!" :: Text)

    connectWs :: Int -> Int -> String -> IO ()
    connectWs n portNo path =
      if n < 0
        then error "Could not connect"
        else withSocketsDo $ runClient "127.0.0.1" portNo path $ \con ->
          application con
            `catch` \(_ :: IOException) -> do
              threadDelay 1
              connectWs (n - 1) portNo path

    receiveInputs :: Connection -> IO ()
    receiveInputs con = do
      msg <- receiveData con
      print msg
      case Aeson.eitherDecode msg :: Either String Aeson.Value of
        Right i -> print i
        Left _ -> pure ()

main :: IO ()
main = hspec $ around (showLogsOnFailure "spec") $ do
  describe "HTLC" $ do
    it "hydra lightning router" $ \tracer ->
      withTempDir "hydra-head-1" $ \tmpDir ->
        withTempDir "hydra-head-2" $ \tmpDir2 -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \_ backend -> do
            x <- Faucet.publishHydraScriptsAs backend Faucet
            singlePartyCommitsFromExternal tracer tmpDir tmpDir2 backend x

{--
main :: IO ()
main = do
  runClient "127.0.0.1" 4001 "/?history=no" $ \aliceConn -> do
    runClient "127.0.0.1" 4002 "/?history=no" $ \bobConn -> do
      runClient "127.0.0.1" 4003 "/?history=no" $ \idaConn1 -> do
        runClient "127.0.0.1" 4004 "/?history=no" $ \idaConn2 -> do
          send aliceConn $ input "Init" []

          -- Alice Init
          -- Alice Commit Something
          -- Ida1 Commit 0
          -- Bob Init
          -- Ida2 Commit Something
          -- 0: Bob generates an invoice and keeps the secret.
          -- 1: Alice pays an HTLC on Head 1.
          -- 2: Ida observes HTLC on Head 1.
          -- 3: Ida constructs HTLC on Head 2 using the same hash.
          -- 4: Bob claims the money in Head 2 using the Claim redeemer.
          -- 5: Ida observes HTLC Preimage on Head 2.
          -- 6: Ida claims the money in Head 1 using the Preimage.
          receiveDataMessage aliceConn >>= print
          receiveDataMessage bobConn >>= print
          receiveDataMessage idaConn1 >>= print
          receiveDataMessage idaConn2 >>= print
--}
