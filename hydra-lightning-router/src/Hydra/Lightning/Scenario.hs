{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Lightning.Scenario (main) where

import Cardano.Api qualified as C
import Cardano.Api.UTxO qualified as C
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import CardanoClient (QueryPoint (QueryTip))
import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar)
import Control.Exception (finally)
import Control.Lens (contramap, (&), (^..), (^?))
import Control.Monad (guard)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _JSON)
import Data.Bifunctor (second)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Time (addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import GHC.Conc (TVar, atomically)
import GHC.IsList (toList)
import Hydra.API.HTTPServer (DraftCommitTxResponse (DraftCommitTxResponse, commitTx))
import Hydra.Cardano.Api
  ( Era,
    Tx,
    TxOut,
    UTxO,
    balancedTxBody,
    defaultTxBodyContent,
    fromLedgerTx,
    fromLedgerTxOut,
    mkScriptAddress,
    mkTxOutDatumInline,
    mkVkAddress,
    signTx,
    toLedgerTx,
    toScriptData,
    txOutReferenceScript,
    txOutValue,
    pattern ByronAddressInEra,
    pattern PlutusScript,
    pattern PlutusScriptWitness,
    pattern ShelleyAddress,
    pattern ShelleyAddressInEra,
    pattern Tx,
    pattern TxOut,
  )
import Hydra.Chain.Backend (ChainBackend)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Cluster.Faucet (seedFromFaucet, seedFromFaucetWithMinting)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture
  ( Actor (Faucet),
  )
import Hydra.Cluster.Scenarios
  ( EndToEndLog (FromCardanoNode, FromFaucet, FromHydraNode),
    headIsInitializingWith,
    recomputeIntegrityHash,
  )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Contract.Dummy (dummyMintingScript)
import Hydra.HTLC.Conversions (standardInvoiceToHTLCDatum)
import Hydra.HTLC.Data (Redeemer (Claim))
import Hydra.HTLC.Embed (htlcValidatorScript)
import Hydra.Invoice qualified as I
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Node.Util (readFileTextEnvelopeThrow)
import Hydra.Options (DirectOptions (..))
import Hydra.Tx (Party (..))
import Hydra.Tx.ContestationPeriod qualified as CP
import HydraNode
  ( getProtocolParameters,
    getSnapshotUTxO,
    input,
    output,
    send,
    waitFor,
    waitMatch,
    withConnectionToNode,
    withHydraNode,
  )
import Network.HTTP.Req (POST (POST), defaultHttpConfig, http, port, req, responseBody, runReq, (/:))
import Network.HTTP.Req qualified as Req
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Prelude (around, describe, hspec, it, shouldBe, withTempDir)
import Test.QuickCheck (Gen, arbitrary, generate, suchThat)

instance C.HasTypeProxy BS.ByteString where
  data AsType BS.ByteString = AsByteString
  proxyToAsType _ = AsByteString

instance C.SerialiseAsRawBytes BS.ByteString where
  serialiseToRawBytes = id
  deserialiseFromRawBytes AsByteString = pure

aliceBobIdaTransferAcrossHeads ::
  Tracer IO EndToEndLog ->
  DirectBackend ->
  [C.TxId] ->
  IO ()
aliceBobIdaTransferAcrossHeads tracer backend hydraScriptsTxId = do
  let contestationPeriod :: CP.ContestationPeriod = 100

  let hydraTracer = contramap FromHydraNode tracer

  blockTime <- Backend.getBlockTime backend
  networkId <- Backend.queryNetworkId backend

  let alice = Party {vkey = "b37aabd81024c043f53a069c91e51a5b52e4ea399ae17ee1fe3cb9c44db707eb"}
  let bob = Party {vkey = "f68e5624f885d521d2f43c3959a0de70496d5464bd3171aba8248f50d5d72b41"}
  let carol = Party {vkey = "7abcda7de6d883e7570118c1ccc8ee2e911f2e628a41ab0685ffee15f39bba96"}

  aliceWalletSk <- readFileTextEnvelopeThrow "devnet/credentials/alice.sk"
  bobWalletSk <- readFileTextEnvelopeThrow "devnet/credentials/bob.sk"
  carolWalletSk <- readFileTextEnvelopeThrow "devnet/credentials/carol.sk"

  aliceWalletVk <- readFileTextEnvelopeThrow "devnet/credentials/alice.vk"
  bobWalletVk <- readFileTextEnvelopeThrow "devnet/credentials/bob.vk"
  carolWalletVk <- readFileTextEnvelopeThrow "devnet/credentials/carol.vk"

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

          lockTx <- buildLockTx pparams networkId updatedInvoice utxoToCommitAlice sender changeAddress lockedVal
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

          claimTx <- buildClaimTX pparams preImage' claimRecipient expectedKeyHashes lockedVal claimUTxO headCollateral collateral claimRecipient
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
          lockTx <- buildLockTx pparams networkId invoice carolUTxO sender changeAddress lockedVal
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

          claimTx <- buildClaimTX pparams preImage claimRecipient expectedKeyHashes lockedVal claimUTxO headCollateral collateral claimRecipient
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
  where
    waitLockInHead :: TVar (Maybe b) -> IO b
    waitLockInHead var = do
      lockDatum <- readTVarIO var
      case lockDatum of
        Nothing -> threadDelay 1 >> waitLockInHead var
        Just d -> pure d

    vkAddress :: C.NetworkId -> C.VerificationKey C.PaymentKey -> C.Address C.ShelleyAddr
    vkAddress networkId vk =
      C.makeShelleyAddress networkId (C.PaymentCredentialByKey $ C.verificationKeyHash vk) C.NoStakeAddress

    generateInvoice recipient value = do
      date <- addUTCTime (60 * 60 * 24 * 30) <$> getCurrentTime
      (invoice, preImage) <- I.generateStandardInvoice recipient value date
      pure (invoice, preImage)

    buildLockTx pparams networkId invoice utxo sender changeAddress val = do
      let scriptAddress = mkScriptAddress networkId htlcValidatorScript
      case standardInvoiceToHTLCDatum invoice sender of
        Nothing -> error "Failed to generate Datum for Invoice"
        Just dat -> do
          let scriptOutput =
                TxOut
                  scriptAddress
                  val
                  (mkTxOutDatumInline dat)
                  C.ReferenceScriptNone

          systemStart <- Backend.querySystemStart backend QueryTip
          eraHistory <- Backend.queryEraHistory backend QueryTip
          stakePools <- Backend.queryStakePools backend QueryTip

          let bodyContent =
                defaultTxBodyContent
                  & C.addTxIns (map (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) (Set.toList $ C.inputSet utxo))
                  & C.addTxOuts [scriptOutput]

          case C.makeTransactionBodyAutoBalance
            C.shelleyBasedEra
            systemStart
            (C.toLedgerEpochInfo eraHistory)
            (C.LedgerProtocolParameters pparams)
            stakePools
            mempty
            mempty
            utxo
            bodyContent
            changeAddress
            Nothing of
            Left e -> error $ show e
            Right tx -> pure $ flip Tx [] $ balancedTxBody tx

    buildClaimTX pparams preImage recipient expectedKeyHashes val claimUTxO collateralUTxO collateral changeAddress = do
      let maxTxExecutionUnits =
            C.ExecutionUnits
              { C.executionMemory = 0,
                C.executionSteps = 0
              }

      let secret = toBuiltin $ C.serialiseToRawBytes $ I.fromPreImage preImage

      let scriptWitness =
            C.BuildTxWith $
              C.ScriptWitness C.scriptWitnessInCtx $
                PlutusScriptWitness
                  htlcValidatorScript
                  (C.ScriptDatumForTxIn Nothing)
                  (toScriptData (Claim secret))
                  maxTxExecutionUnits

      let txIn = head $ Set.toList $ C.inputSet claimUTxO

      C.ChainPoint tip _ <- Backend.queryTip backend

      let body =
            defaultTxBodyContent
              & C.addTxIns [(txIn, scriptWitness), (collateral, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
              & C.addTxInsCollateral [collateral]
              & C.addTxOuts [TxOut recipient val C.TxOutDatumNone C.ReferenceScriptNone]
              & C.setTxExtraKeyWits (C.TxExtraKeyWitnesses C.AlonzoEraOnwardsConway expectedKeyHashes)
              & C.setTxValidityLowerBound (C.TxValidityLowerBound C.AllegraEraOnwardsConway tip)
              & C.setTxValidityUpperBound (C.TxValidityUpperBound C.ShelleyBasedEraConway $ Just $ tip + 5000)

      systemStart <- Backend.querySystemStart backend CardanoClient.QueryTip
      eraHistory <- Backend.queryEraHistory backend CardanoClient.QueryTip
      stakePools <- Backend.queryStakePools backend CardanoClient.QueryTip

      let eBody =
            second (flip Tx [] . balancedTxBody) $
              C.makeTransactionBodyAutoBalance
                C.shelleyBasedEra
                systemStart
                (C.toLedgerEpochInfo eraHistory)
                (C.LedgerProtocolParameters pparams)
                stakePools
                mempty
                mempty
                (claimUTxO <> collateralUTxO)
                body
                changeAddress
                Nothing
      case eBody of
        Left e -> error $ show e
        Right tx ->
          pure $ fromLedgerTx @Era $ recomputeIntegrityHash pparams [PlutusV3] (toLedgerTx tx)

-- ASSET GENERATION
genFungibleAsset :: C.Quantity -> Maybe C.PolicyId -> Gen UTxO
genFungibleAsset quantity pid =
  UTxO.singleton <$> arbitrary <*> genTxOutWithAssets quantity pid

genTxOutWithAssets :: C.Quantity -> Maybe C.PolicyId -> Gen (TxOut ctx)
genTxOutWithAssets q pid =
  ((fromLedgerTxOut <$> arbitrary) `suchThat` notByronAddress)
    <&> singleAsset q . noNegativeAssetsWithPotentialPolicy pid . noRefScripts . noStakeRefPtr

singleAsset :: C.Quantity -> TxOut ctx -> TxOut ctx
singleAsset wantedQuantity (TxOut addr val dat refScript) =
  let (pid, C.PolicyAssets assets) = head $ Map.assocs $ C.valueToPolicyAssets val
      (name, _) = head $ Map.assocs assets
      singleEntry = Map.singleton name wantedQuantity
      newVal = C.policyAssetsToValue pid (C.PolicyAssets singleEntry)
   in TxOut addr newVal dat refScript

noStakeRefPtr :: TxOut ctx -> TxOut ctx
noStakeRefPtr out@(TxOut addr val dat refScript) = case addr of
  ShelleyAddressInEra (ShelleyAddress _ cre sr) ->
    case sr of
      Ledger.StakeRefPtr _ ->
        TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre Ledger.StakeRefNull)) val dat refScript
      _ ->
        TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre sr)) val dat refScript
  _ -> out

notByronAddress :: TxOut ctx -> Bool
notByronAddress (C.TxOut addr _ _ _) = case addr of
  ByronAddressInEra {} -> False
  _ -> True

noRefScripts :: TxOut ctx -> TxOut ctx
noRefScripts out =
  out {txOutReferenceScript = C.ReferenceScriptNone}

noNegativeAssetsWithPotentialPolicy :: Maybe C.PolicyId -> TxOut ctx -> TxOut ctx
noNegativeAssetsWithPotentialPolicy mpid out =
  let val = txOutValue out
      nonAdaAssets =
        Map.foldrWithKey
          (\pid policyAssets def -> C.policyAssetsToValue (fromMaybe pid mpid) policyAssets <> def)
          mempty
          (filterNegativeVals (C.valueToPolicyAssets val))
      ada = C.selectLovelace val
   in out {txOutValue = C.lovelaceToValue ada <> nonAdaAssets}
  where
    filterNegativeVals =
      Map.filterWithKey
        ( \pid passets ->
            all
              (\(_, C.Quantity n) -> n > 0)
              (toList $ C.policyAssetsToValue pid passets)
        )

main :: IO ()
main = hspec $ around (showLogsOnFailure "spec") $ do
  describe "HTLC" $ do
    it "hydra lightning router" $ \tracer -> do
      let backend = DirectBackend $ DirectOptions {networkId = C.Testnet $ C.NetworkMagic 42, nodeSocket = "devnet/node.socket"}
      x <- Faucet.publishHydraScriptsAs backend Faucet
      aliceBobIdaTransferAcrossHeads tracer backend x
