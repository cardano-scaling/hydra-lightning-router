{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Hydra.Lightning.Scenario where

import CardanoNode (withBackend)
import Hydra.Cardano.Api (signTx)
import Control.Monad (guard)
import HydraNode (
  HydraClient (..),
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
  requestCommitTx',
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

import Test.Hydra.Prelude (shouldBe, withTempDir, it, describe, hspec, around)
import Hydra.API.HTTPServer (DraftCommitTxResponse(..))
import Cardano.Api (UTxO, Coin, Tx, PolicyId, PolicyAssets)
import Cardano.Api qualified as C
import Network.WebSockets (receiveDataMessage, runClient, sendTextData, Connection)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Network.HTTP.Req qualified as Req
import Network.HTTP.Req
import Data.Proxy (Proxy(Proxy))
import Data.Map (Map)
import Control.Lens ((^.), contramap, (^?))
import Control.Exception (finally)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Hydra.Chain (PostTxError (..))
import Hydra.Chain.Backend (ChainBackend, buildTransaction, buildTransactionWithPParams, buildTransactionWithPParams')
import Hydra.Chain.Backend qualified as Backend
import Hydra.Cluster.Faucet (FaucetLog, createOutputAtAddress, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (..), actorName, alice, aliceSk, aliceVk, bob, bobSk, bobVk, carol, carolSk, carolVk)
import Hydra.Cluster.Mithril (MithrilLog)
import Hydra.Cluster.Options (Options)
import Hydra.Cluster.Util (chainConfigFor, chainConfigFor', keysFor, modifyConfig, setNetworkId)
import Hydra.Contract.Dummy (dummyRewardingScript)
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx, unsafeBuildTransaction)
import Hydra.Cluster.Scenarios (EndToEndLog(..), returnFundsToFaucet, refuelIfNeeded, headIsInitializingWith)
import Data.Set qualified as Set
import Hydra.Ledger.Cardano.Evaluate (maxTxExecutionUnits)
import Hydra.Logging (Tracer, traceWith, showLogsOnFailure)
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), DirectOptions (..), RunOptions (..), startChainFrom)
import Hydra.Tx (HeadId, IsTx (balance), Party, txId)
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Deposit (capUTxO)
import Hydra.Tx.Utils (dummyValidatorScript, verificationKeyToOnChainId)

-- | Single hydra-node where the commit is done using some wallet UTxO.
singlePartyCommitsFromExternal ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [C.TxId] ->
  IO ()
singlePartyCommitsFromExternal tracer workDir backend hydraScriptsTxId =
  ( `finally`
      do
        returnFundsToFaucet tracer backend Alice
        returnFundsToFaucet tracer backend AliceFunds
  )
    $ do
      refuelIfNeeded tracer backend Alice 25_000_000
      let contestationPeriod = 100
      aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
      let hydraNodeId = 1
      let hydraTracer = contramap FromHydraNode tracer
      blockTime <- Backend.getBlockTime backend
      withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

        (walletVk, walletSk) <- keysFor AliceFunds
        utxoToCommit <- seedFromFaucet backend walletVk 5_000_000 (contramap FromFaucet tracer)

        res <-
          runReq defaultHttpConfig $
            req
              POST
              (http "127.0.0.1" /: "commit")
              (ReqBodyJson utxoToCommit)
              (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse (Tx C.ConwayEra))))
              (port $ 4000 + hydraNodeId)

        let DraftCommitTxResponse{commitTx} = responseBody res
        Backend.submitTransaction backend $ signTx walletSk commitTx

        lockedUTxO <- waitMatch (10 * blockTime) n1 $ \v -> do
          guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
          guard $ v ^? key "tag" == Just "HeadIsOpen"
          pure $ v ^? key "utxo"
        lockedUTxO `shouldBe` Just (Aeson.toJSON utxoToCommit)

main :: IO ()
main = hspec $ around (showLogsOnFailure "spec")$ do
 describe "foo" $ do
  it "foo" $ \tracer -> do
   withTempDir "hydra-cluster" $ \tmpDir -> do
     print "fff"
     withBackend (contramap FromCardanoNode tracer) tmpDir $ \_ backend -> do
          print "aa"
          x <- Faucet.publishHydraScriptsAs backend Faucet
          print "bb"
          singlePartyCommitsFromExternal tracer tmpDir backend x

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
