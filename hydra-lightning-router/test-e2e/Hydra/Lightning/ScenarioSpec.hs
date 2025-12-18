{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Hydra.Lightning.ScenarioSpec (spec) where

import Cardano.Api qualified as C
import Cardano.Api.UTxO qualified as C
import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM.TVar (newTVarIO, writeTVar)
import Control.Lens (contramap, (^..), (^?))
import Control.Monad (guard)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _JSON)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Time (diffUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import GHC.Conc (atomically)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse, commitTx))
import Hydra.Cardano.Api
  ( Tx,
    mkScriptAddress,
    mkVkAddress,
    signTx,
    pattern PlutusScript,
    pattern TxOut,
  )
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Direct (DirectBackend (DirectBackend))
import Hydra.Cluster.Faucet (seedFromFaucet, seedFromFaucetWithMinting)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture
  ( Actor (Faucet),
  )
import Hydra.Cluster.Scenarios
  ( EndToEndLog (FromFaucet, FromHydraNode),
    headIsInitializingWith,
  )
import Hydra.Contract.Dummy (dummyMintingScript)
import Hydra.HTLC.Embed (htlcValidatorScript)
import Hydra.Invoice qualified as I
import Hydra.Lightning.Helper
  ( alice,
    aliceWalletSk,
    aliceWalletVk,
    bob,
    bobWalletSk,
    bobWalletVk,
    buildClaimTX,
    buildLockTx,
    buildRefundTx,
    carol,
    carolWalletSk,
    carolWalletVk,
    genFungibleAsset,
    generateInvoice,
    generateInvoiceWithTime,
    vkAddress,
    waitLockInHead,
    waitToReceiveFunds,
  )
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options (DirectOptions (DirectOptions, networkId, nodeSocket))
import HydraNode
  ( getProtocolParameters,
    getSnapshotUTxO,
    input,
    output,
    send,
    waitFor,
    waitMatch,
    withConnectionToNode,
  )
import Network.HTTP.Req (POST (POST), defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Network.HTTP.Req qualified as Req
import Test.Hydra.Prelude (Spec, around, describe, it, shouldBe)
import Test.QuickCheck (generate)

spec :: Spec
spec = around (showLogsOnFailure "spec") $ do
  let backend = DirectBackend $ DirectOptions {networkId = C.Testnet $ C.NetworkMagic 42, nodeSocket = "./../devnet/node.socket"}
  describe "HTLC" $ do
    it "hydra lightning router" $ \tracer -> do
      _ <- Faucet.publishHydraScriptsAs backend Faucet
      aliceBobIdaTransferAcrossHeads tracer backend
    it "can refund from HTLC after timelock" $ \tracer -> do
      _ <- Faucet.publishHydraScriptsAs backend Faucet
      refundFromHTLC tracer backend

aliceBobIdaTransferAcrossHeads ::
  Tracer IO EndToEndLog ->
  DirectBackend ->
  IO ()
aliceBobIdaTransferAcrossHeads tracer backend = do
  let hydraTracer = contramap FromHydraNode tracer

  blockTime <- Backend.getBlockTime backend
  networkId <- Backend.queryNetworkId backend

  -- Used to signal when the claiming process has finished in the opposite Head
  head1Var <- newTVarIO Nothing
  head2Var <- newTVarIO Nothing

  tokensUTxO <- generate (genFungibleAsset (C.Quantity 5) (Just $ C.PolicyId $ C.hashScript $ PlutusScript dummyMintingScript))
  let totalTokenValue = UTxO.totalValue tokensUTxO
  let tokenAssets = C.valueToPolicyAssets totalTokenValue
  let assetsToValue = foldMap ((mempty <>) . uncurry C.policyAssetsToValue) . Map.toList
  let tokenAssetValue = assetsToValue tokenAssets
  let lockedVal = C.lovelaceToValue 5_000_000 <> tokenAssetValue
  utxoToCommitAlice <-
    seedFromFaucetWithMinting
      backend
      aliceWalletVk
      (C.lovelaceToValue 12_000_000 <> tokenAssetValue)
      (contramap FromFaucet tracer)
      (Just dummyMintingScript)
  carolUTxO <- seedFromFaucetWithMinting backend carolWalletVk (C.lovelaceToValue 14_000_000 <> tokenAssetValue) (contramap FromFaucet tracer) (Just dummyMintingScript)

  let head1 = withConnectionToNode hydraTracer 1 $ \n1 ->
        withConnectionToNode hydraTracer 2 $ \n2 -> do
          utxoToCommitCarol <- seedFromFaucet backend carolWalletVk (C.lovelaceToValue 10_000_000) (contramap FromFaucet tracer)
          send n1 $ input "Init" []
          headId <- waitMatch (20 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice, carol])

          res <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson utxoToCommitAlice)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 1)

          let DraftCommitTxResponse {commitTx} = responseBody res
          Backend.submitTransaction backend $ signTx aliceWalletSk commitTx
          res2 <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson utxoToCommitCarol)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 2)

          let DraftCommitTxResponse {commitTx = commitTxCarol} = responseBody res2
          Backend.submitTransaction backend $ signTx carolWalletSk commitTxCarol

          headUTxO <- waitMatch (20 * blockTime) n1 $ \v -> do
            guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
            guard $ v ^? key "tag" == Just "HeadIsOpen"
            pure $ v ^? key "utxo"
          headUTxO `shouldBe` Just (Aeson.toJSON $ utxoToCommitAlice <> utxoToCommitCarol)

          pparams <- getProtocolParameters n1
          let sender = vkAddress networkId aliceWalletVk
          let recipient = vkAddress networkId bobWalletVk
          let changeAddress = mkVkAddress networkId aliceWalletVk
          (invoice, preImage) <- generateInvoice recipient lockedVal
          let updatedInvoice =
                invoice {I.recipient = vkAddress networkId carolWalletVk}

          lockTx <- buildLockTx backend pparams networkId updatedInvoice utxoToCommitAlice sender changeAddress lockedVal
          let signedL2tx = signTx aliceWalletSk lockTx

          send n1 $ input "NewTx" ["transaction" Aeson..= signedL2tx]

          waitMatch 10 n2 $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $
              Aeson.toJSON signedL2tx
                `elem` (v ^.. key "snapshot" . key "confirmed" . values)

          -- NOTIFY Head2 to start the claim
          atomically $ writeTVar head1Var (Just (invoice, preImage))

          -- WAITING on Claim tx in Head 2
          (_invoice', preImage') <- waitLockInHead head2Var

          -- CLAIM TX
          headUTxO' <- getSnapshotUTxO n2
          let claimUTxO = C.filter (\(TxOut a _ _ _) -> a == mkScriptAddress networkId htlcValidatorScript) headUTxO'

          let claimRecipient =
                C.shelleyAddressInEra C.ShelleyBasedEraConway (vkAddress networkId carolWalletVk)
          let headCollateral = C.filter (\(TxOut a _ _ _) -> a == claimRecipient) headUTxO'
          let expectedKeyHashes = [C.verificationKeyHash carolWalletVk]
          let collateral = head $ Set.toList $ C.inputSet headCollateral

          claimTx <- buildClaimTX backend pparams preImage' claimRecipient expectedKeyHashes lockedVal claimUTxO headCollateral collateral claimRecipient
          -- We only need Alice sig for collateral input
          let signedClaimTx = signTx carolWalletSk claimTx

          send n2 $ input "NewTx" ["transaction" Aeson..= signedClaimTx]

          waitMatch 20 n2 $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $
              Aeson.toJSON signedClaimTx
                `elem` (v ^.. key "snapshot" . key "confirmed" . values)

          send n1 $ input "Close" []
          deadline <- waitMatch (20 * blockTime) n1 $ \v -> do
            guard $ v ^? key "tag" == Just "HeadIsClosed"
            v ^? key "contestationDeadline" . _JSON
          remainingTime <- diffUTCTime deadline <$> getCurrentTime

          waitFor hydraTracer (remainingTime + 20 * blockTime) [n1] $
            output "ReadyToFanout" ["headId" Aeson..= headId]

          send n1 $ input "Fanout" []

          waitMatch (20 * blockTime) n1 $ \v ->
            guard $ v ^? key "tag" == Just "HeadIsFinalized"

  let head2 = withConnectionToNode hydraTracer 3 $ \n3 ->
        withConnectionToNode hydraTracer 4 $ \n4 -> do
          bobUTxO <- seedFromFaucet backend bobWalletVk (C.lovelaceToValue 10_000_000) (contramap FromFaucet tracer)
          send n3 $ input "Init" []
          headId <- waitMatch (20 * blockTime) n3 $ headIsInitializingWith (Set.fromList [bob, carol])

          res <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson bobUTxO)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 3)

          let DraftCommitTxResponse {commitTx = commitTxBob} = responseBody res
          Backend.submitTransaction backend $ signTx bobWalletSk commitTxBob

          res2 <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson carolUTxO)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 4)

          let DraftCommitTxResponse {commitTx = commitTxCarol} = responseBody res2
          Backend.submitTransaction backend $ signTx carolWalletSk commitTxCarol

          headUTxO <- waitMatch (20 * blockTime) n3 $ \v -> do
            guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
            guard $ v ^? key "tag" == Just "HeadIsOpen"
            pure $ v ^? key "utxo"
          headUTxO `shouldBe` Just (Aeson.toJSON $ bobUTxO <> carolUTxO)

          -- WAITING on Lock tx in Head 1
          (invoice, preImage) <- waitLockInHead head1Var

          -- LOCK TX
          pparams <- getProtocolParameters n4
          let sender = vkAddress networkId carolWalletVk
          let changeAddress = mkVkAddress networkId carolWalletVk
          lockTx <- buildLockTx backend pparams networkId invoice carolUTxO sender changeAddress lockedVal
          let signedL2tx = signTx carolWalletSk lockTx

          send n4 $ input "NewTx" ["transaction" Aeson..= signedL2tx]

          waitMatch 10 n4 $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $
              Aeson.toJSON signedL2tx
                `elem` (v ^.. key "snapshot" . key "confirmed" . values)

          threadDelay 5_000_000

          -- CLAIM TX
          headUTxO' <- getSnapshotUTxO n3
          let claimUTxO = C.filter (\(TxOut a _ _ _) -> a == mkScriptAddress networkId htlcValidatorScript) headUTxO'
          let claimRecipient =
                C.shelleyAddressInEra C.ShelleyBasedEraConway (vkAddress networkId bobWalletVk)
          let headCollateral = C.filter (\(TxOut a _ _ _) -> a == claimRecipient) headUTxO'
          let expectedKeyHashes = [C.verificationKeyHash bobWalletVk]
          let collateral = head $ Set.toList $ C.inputSet headCollateral

          claimTx <- buildClaimTX backend pparams preImage claimRecipient expectedKeyHashes lockedVal claimUTxO headCollateral collateral claimRecipient
          let signedClaimTx = signTx bobWalletSk claimTx

          send n3 $ input "NewTx" ["transaction" Aeson..= signedClaimTx]

          waitMatch 20 n3 $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $
              Aeson.toJSON signedClaimTx
                `elem` (v ^.. key "snapshot" . key "confirmed" . values)

          -- NOTIFY Head1 to start the claim
          atomically $ writeTVar head2Var (Just (invoice, preImage))

          send n3 $ input "Close" []

          deadline <- waitMatch (20 * blockTime) n3 $ \v -> do
            guard $ v ^? key "tag" == Just "HeadIsClosed"
            v ^? key "contestationDeadline" . _JSON
          remainingTime <- diffUTCTime deadline <$> getCurrentTime
          waitFor hydraTracer (remainingTime + 20 * blockTime) [n3] $
            output "ReadyToFanout" ["headId" Aeson..= headId]

          send n3 $ input "Fanout" []

          waitMatch (20 * blockTime) n3 $ \v ->
            guard $ v ^? key "tag" == Just "HeadIsFinalized"

  -- Run two heads in parallel
  _ <- concurrently head1 head2
  pure ()

refundFromHTLC ::
  Tracer IO EndToEndLog ->
  DirectBackend ->
  IO ()
refundFromHTLC tracer backend = do
  let hydraTracer = contramap FromHydraNode tracer

  blockTime <- Backend.getBlockTime backend
  networkId <- Backend.queryNetworkId backend

  head1Var <- newTVarIO Nothing

  tokensUTxO <- generate (genFungibleAsset (C.Quantity 5) (Just $ C.PolicyId $ C.hashScript $ PlutusScript dummyMintingScript))
  let totalTokenValue = UTxO.totalValue tokensUTxO
  let tokenAssets = C.valueToPolicyAssets totalTokenValue
  let assetsToValue = foldMap ((mempty <>) . uncurry C.policyAssetsToValue) . Map.toList
  let tokenAssetValue = assetsToValue tokenAssets
  let lockedLovelace :: C.Lovelace = 5_000_000
  let lockedVal = C.lovelaceToValue lockedLovelace <> tokenAssetValue
  utxoToCommitAlice <-
    seedFromFaucetWithMinting
      backend
      aliceWalletVk
      (C.lovelaceToValue 12_000_000 <> tokenAssetValue)
      (contramap FromFaucet tracer)
      (Just dummyMintingScript)
  carolUTxO <- seedFromFaucetWithMinting backend carolWalletVk (C.lovelaceToValue 14_000_000 <> tokenAssetValue) (contramap FromFaucet tracer) (Just dummyMintingScript)

  let head1 = withConnectionToNode hydraTracer 1 $ \n1 ->
        withConnectionToNode hydraTracer 2 $ \n2 -> do
          utxoToCommitCarol <- seedFromFaucet backend carolWalletVk (C.lovelaceToValue 10_000_000) (contramap FromFaucet tracer)
          send n1 $ input "Init" []
          headId <- waitMatch (20 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice, carol])

          res <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson utxoToCommitAlice)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 1)

          let DraftCommitTxResponse {commitTx} = responseBody res
          Backend.submitTransaction backend $ signTx aliceWalletSk commitTx
          res2 <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson utxoToCommitCarol)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 2)

          let DraftCommitTxResponse {commitTx = commitTxCarol} = responseBody res2
          Backend.submitTransaction backend $ signTx carolWalletSk commitTxCarol

          headUTxO <- waitMatch (20 * blockTime) n1 $ \v -> do
            guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
            guard $ v ^? key "tag" == Just "HeadIsOpen"
            pure $ v ^? key "utxo"
          headUTxO `shouldBe` Just (Aeson.toJSON $ utxoToCommitAlice <> utxoToCommitCarol)

          pparams <- getProtocolParameters n1
          let sender = vkAddress networkId aliceWalletVk
          let recipient = vkAddress networkId bobWalletVk
          let changeAddress = mkVkAddress networkId aliceWalletVk
          now <- getCurrentTime
          (invoice, _) <- generateInvoiceWithTime now recipient lockedVal
          let updatedInvoice =
                invoice {I.recipient = vkAddress networkId carolWalletVk}

          lockTx <- buildLockTx backend pparams networkId updatedInvoice utxoToCommitAlice sender changeAddress lockedVal
          let signedL2tx = signTx aliceWalletSk lockTx

          send n1 $ input "NewTx" ["transaction" Aeson..= signedL2tx]

          waitMatch 10 n2 $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $
              Aeson.toJSON signedL2tx
                `elem` (v ^.. key "snapshot" . key "confirmed" . values)

          send n1 $ input "Close" []
          deadline <- waitMatch (20 * blockTime) n1 $ \v -> do
            guard $ v ^? key "tag" == Just "HeadIsClosed"
            v ^? key "contestationDeadline" . _JSON
          remainingTime <- diffUTCTime deadline <$> getCurrentTime

          waitFor hydraTracer (remainingTime + 20 * blockTime) [n1] $
            output "ReadyToFanout" ["headId" Aeson..= headId]

          send n1 $ input "Fanout" []

          waitMatch (20 * blockTime) n1 $ \v ->
            guard $ v ^? key "tag" == Just "HeadIsFinalized"

          -- NOTIFY Head2 to also close and then try to refund
          atomically $ writeTVar head1Var (Just ())

  let head2 = withConnectionToNode hydraTracer 3 $ \n3 ->
        withConnectionToNode hydraTracer 4 $ \_ -> do
          bobUTxO <- seedFromFaucet backend bobWalletVk (C.lovelaceToValue 10_000_000) (contramap FromFaucet tracer)
          send n3 $ input "Init" []
          headId <- waitMatch (20 * blockTime) n3 $ headIsInitializingWith (Set.fromList [bob, carol])

          res <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson bobUTxO)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 3)

          let DraftCommitTxResponse {commitTx = commitTxBob} = responseBody res
          Backend.submitTransaction backend $ signTx bobWalletSk commitTxBob

          res2 <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (Req.ReqBodyJson carolUTxO)
                (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse Tx)))
                (port $ 4000 + 4)

          let DraftCommitTxResponse {commitTx = commitTxCarol} = responseBody res2
          Backend.submitTransaction backend $ signTx carolWalletSk commitTxCarol

          headUTxO <- waitMatch (20 * blockTime) n3 $ \v -> do
            guard $ v ^? key "headId" == Just (Aeson.toJSON headId)
            guard $ v ^? key "tag" == Just "HeadIsOpen"
            pure $ v ^? key "utxo"
          headUTxO `shouldBe` Just (Aeson.toJSON $ bobUTxO <> carolUTxO)

          -- WAITING for Head 1 to close after locking the funds in HTLC script
          _ <- waitLockInHead head1Var

          send n3 $ input "Close" []
          deadline <- waitMatch (20 * blockTime) n3 $ \v -> do
            guard $ v ^? key "tag" == Just "HeadIsClosed"
            v ^? key "contestationDeadline" . _JSON
          remainingTime <- diffUTCTime deadline <$> getCurrentTime

          waitFor hydraTracer (remainingTime + 20 * blockTime) [n3] $
            output "ReadyToFanout" ["headId" Aeson..= headId]

          send n3 $ input "Fanout" []

          waitMatch (20 * blockTime) n3 $ \v ->
            guard $ v ^? key "tag" == Just "HeadIsFinalized"

          pure ()

  -- Run two heads in parallel until they close/fanout
  _ <- concurrently head1 head2

  let htlcAddress = C.makeShelleyAddress networkId (C.PaymentCredentialByScript $ C.hashScript $ PlutusScript htlcValidatorScript) C.NoStakeAddress
  htlcUTxO <- Backend.queryUTxO backend [htlcAddress]
  aliceL1UTxO <- Backend.queryUTxO backend [vkAddress networkId aliceWalletVk]
  let aliceBeforeRefund = UTxO.totalLovelace aliceL1UTxO
  let changeAddress = mkVkAddress networkId aliceWalletVk

  -- Alice was locking funds into HTLC but now is also a recipient of a refund
  let recipient = C.shelleyAddressInEra C.ShelleyBasedEraConway (vkAddress networkId aliceWalletVk)
  refundTx <- buildRefundTx backend htlcUTxO recipient changeAddress lockedVal aliceL1UTxO
  let signedL1tx = signTx aliceWalletSk refundTx
  let expectedTxId = C.getTxId $ C.getTxBody signedL1tx
  let C.TxFeeExplicit _ fee = C.txFee $ C.getTxBodyContent $ C.getTxBody signedL1tx
  -- SUBMIT L1 refund tx
  Backend.submitTransaction backend signedL1tx
  putStrLn "Waiting on funds..."
  aliceL1UTxO' <- waitToReceiveFunds backend networkId aliceWalletVk expectedTxId
  let aliceAfterRefund = UTxO.totalLovelace aliceL1UTxO'

  -- ALICE should have whatever she had before refund + the value locked in HTLC contract and minus the fees she paid for refund tx.
  if aliceAfterRefund == aliceBeforeRefund + lockedLovelace - fee
    then pure ()
    else error "Refund not detected"
