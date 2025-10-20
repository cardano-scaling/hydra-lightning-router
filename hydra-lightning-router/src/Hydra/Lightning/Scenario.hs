{-# LANGUAGE OverloadedStrings #-}

module Hydra.Lightning.Scenario where

import Cardano.Api (Coin, PolicyAssets, PolicyId)
import Cardano.Api qualified as C
import CardanoClient (buildAddress)
import CardanoNode (withCardanoNodeDevnet)
import System.Timeout (timeout)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, catch, finally)
import Control.Lens (contramap, (^.), (^?))
import Control.Monad (forever, guard, unless)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Debug.Trace (traceShow, traceShowM)
import GHC.Generics (Generic)
import Hydra.API.HTTPServer (DraftCommitTxResponse (..))
import Hydra.API.ServerOutput (ServerOutput)
import Hydra.Cardano.Api (Tx, UTxO, signTx)
import Hydra.Chain (PostTxError (..))
import Hydra.Chain.Backend (ChainBackend, buildTransaction, buildTransactionWithPParams, buildTransactionWithPParams')
import Hydra.Chain.Backend qualified as Backend
import Hydra.Cluster.Faucet (FaucetLog, createOutputAtAddress, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (..), actorName, alice, aliceSk, aliceVk, bob, bobSk, bobVk, carol, carolSk, carolVk)
import Hydra.Cluster.Mithril (MithrilLog)
import Hydra.Cluster.Options (Options)
import Hydra.Cluster.Scenarios (EndToEndLog (..), headIsInitializingWith, refuelIfNeeded, returnFundsToFaucet)
import Hydra.Cluster.Util (chainConfigFor, chainConfigFor', keysFor, modifyConfig, setNetworkId)
import Hydra.Contract (hydraScriptCatalogue)
import Hydra.Contract.Dummy (dummyRewardingScript)
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx, unsafeBuildTransaction)
import Hydra.Ledger.Cardano.Evaluate (maxTxExecutionUnits)
import Hydra.Logging (Tracer, showLogsOnFailure, traceWith)
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), DirectOptions (..), RunOptions (..), startChainFrom)
import Hydra.Tx (HeadId, IsTx (balance), Party, txId)
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Utils (dummyValidatorScript, verificationKeyToOnChainId)
import HydraNode
  ( HydraClient (..),
    HydraNodeLog,
    getProtocolParameters,
    getSnapshotConfirmed,
    getSnapshotLastSeen,
    getSnapshotUTxO,
    input,
    output,
    postDecommit,
    prepareHydraNode,
    requestCommitTx,
    send,
    waitFor,
    waitForAllMatch,
    waitForNodesConnected,
    waitForNodesDisconnected,
    waitMatch,
    withHydraCluster,
    withHydraNode,
    withPreparedHydraNode,
  )
import Network.HTTP.Req
import Network.HTTP.Req qualified as Req
import Network.Socket (withSocketsDo)
import Network.WebSockets
  ( Connection,
    ConnectionException,
    ServerApp,
    acceptRequest,
    defaultConnectionOptions,
    receiveData,
    receiveDataMessage,
    runClient,
    runServer,
    sendClose,
    sendTextData,
    withPingThread,
  )
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

      let contestationPeriod = 100

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
      let timeoutVal = 100
      let path = "/"
      let wsServer1 = connectWs timeoutVal port1 path
      let wsServer2 = connectWs timeoutVal port2 path

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
                    (ReqBodyJson utxoToCommit)
                    (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                    (port $ 4000 + 1)

              let DraftCommitTxResponse {commitTx} = responseBody res
              Backend.submitTransaction backend $ signTx aliceWalletSk commitTx

              requestCommitTx n2 mempty >>= Backend.submitTransaction backend

              lockedUTxO <- waitMatch (20 * blockTime) n1 $ \v -> do
                guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
                guard $ v ^? key "tag" == Just "HeadIsOpen"
                pure $ v ^? key "utxo"
              lockedUTxO `shouldBe` Just (Aeson.toJSON utxoToCommit)

      let hydraHead2 = withHydraNode hydraTracer bobChainConfig workDir2 3 bobSk [carolVk] [3, 4] $ \n3 ->
            withHydraNode hydraTracer carolChainConfig2 workDir2 4 carolSk [bobVk] [3, 4] $ \n4 -> do
              utxoToCommit2 <- seedFromFaucet backend bobWalletVk (C.lovelaceToValue 13_000_000) (contramap FromFaucet tracer)
              wsServer2
              send n3 $ input "Init" []
              headId <- waitMatch (10 * blockTime) n3 $ headIsInitializingWith (Set.fromList [bob, carol])
              res <-
                runReq defaultHttpConfig $
                  req
                    POST
                    (http "127.0.0.1" /: "commit")
                    (ReqBodyJson utxoToCommit2)
                    (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                    (port $ 4000 + 3)

              let DraftCommitTxResponse {commitTx = commitTx2} = responseBody res
              Backend.submitTransaction backend $ signTx bobWalletSk commitTx2

              requestCommitTx n4 mempty >>= Backend.submitTransaction backend

              lockedUTxO <- waitMatch (20 * blockTime) n3 $ \v -> do
                guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
                guard $ v ^? key "tag" == Just "HeadIsOpen"
                pure $ v ^? key "utxo"
              lockedUTxO `shouldBe` Just (Aeson.toJSON utxoToCommit2)

      (a, b) <- concurrently hydraHead1 hydraHead2
      pure ()
  where
    application con = do
      withPingThread con 30 (return ()) $ do
        receiveInputs con
        sendClose con ("Bye!" :: Text)

    connectWs n portNo path =
      if n < 0
        then error "Could not connect"
        else withSocketsDo $ runClient "127.0.0.1" portNo path $ \con ->
          application con
            `catch` \(_ :: IOException) -> do
              threadDelay 1
              connectWs (n - 1) portNo path

    receiveInputs :: Connection -> IO (Maybe ())
    receiveInputs con = timeout 100 $ do
      msg <- receiveData con
      print msg
      case Aeson.eitherDecode msg :: Either String Aeson.Value of
        Right i -> print i
        Left e -> error e

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
