{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Lightning.Helper
  ( aliceWalletVk,
    waitToReceiveFunds,
    aliceWalletSk,
    buildRefundTx,
    vkAddress,
    waitLockInHead,
    carolWalletVk,
    carolWalletSk,
    bobWalletSk,
    bobWalletVk,
    carol,
    bob,
    buildLockTx,
    generateInvoiceWithTime,
    genFungibleAsset,
    alice,
    buildClaimTX,
    generateInvoice,
  )
where

import Cardano.Api qualified as C
import Cardano.Api.UTxO qualified as C
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import CardanoClient (QueryPoint (QueryTip))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Lens ((&))
import Data.Bifunctor (second)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import GHC.Conc (TVar)
import GHC.IsList (toList)
import Hydra.Cardano.Api
  ( AddressInEra,
    Era,
    LedgerEra,
    PParams,
    SlotNo,
    Tx,
    TxOut,
    UTxO,
    balancedTxBody,
    defaultTxBodyContent,
    fromLedgerTx,
    fromLedgerTxOut,
    mkScriptAddress,
    mkTxOutDatumInline,
    scriptWitnessInCtx,
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
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Direct (DirectBackend)
import Hydra.Cluster.Scenarios
  ( recomputeIntegrityHash,
  )
import Hydra.HTLC.Conversions (standardInvoiceToHTLCDatum)
import Hydra.HTLC.Data (Redeemer (Claim, Refund))
import Hydra.HTLC.Embed (htlcValidatorScript)
import Hydra.Invoice qualified as I
import Hydra.Ledger.Cardano.Evaluate (maxTxExecutionUnits)
import Hydra.Node.Util (readFileTextEnvelopeThrow)
import Hydra.Tx (Party (Party, vkey))
import PlutusTx.Builtins (toBuiltin)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (Gen, arbitrary, suchThat)

instance C.HasTypeProxy BS.ByteString where
  data AsType BS.ByteString = AsByteString
  proxyToAsType _ = AsByteString

instance C.SerialiseAsRawBytes BS.ByteString where
  serialiseToRawBytes = id
  deserialiseFromRawBytes AsByteString = pure

alice :: Party
alice = Party {vkey = "b37aabd81024c043f53a069c91e51a5b52e4ea399ae17ee1fe3cb9c44db707eb"}

bob :: Party
bob = Party {vkey = "f68e5624f885d521d2f43c3959a0de70496d5464bd3171aba8248f50d5d72b41"}

carol :: Party
carol = Party {vkey = "7abcda7de6d883e7570118c1ccc8ee2e911f2e628a41ab0685ffee15f39bba96"}

aliceWalletSk :: C.SigningKey C.PaymentKey
aliceWalletSk = unsafePerformIO $ readFileTextEnvelopeThrow "./../devnet/credentials/alice.sk"
{-# NOINLINE aliceWalletSk #-}

bobWalletSk :: C.SigningKey C.PaymentKey
bobWalletSk = unsafePerformIO $ readFileTextEnvelopeThrow "./../devnet/credentials/bob.sk"
{-# NOINLINE bobWalletSk #-}

carolWalletSk :: C.SigningKey C.PaymentKey
carolWalletSk = unsafePerformIO $ readFileTextEnvelopeThrow "./../devnet/credentials/carol.sk"
{-# NOINLINE carolWalletSk #-}

aliceWalletVk :: C.VerificationKey C.PaymentKey
aliceWalletVk = unsafePerformIO $ readFileTextEnvelopeThrow "./../devnet/credentials/alice.vk"
{-# NOINLINE aliceWalletVk #-}

bobWalletVk :: C.VerificationKey C.PaymentKey
bobWalletVk = unsafePerformIO $ readFileTextEnvelopeThrow "./../devnet/credentials/bob.vk"
{-# NOINLINE bobWalletVk #-}

carolWalletVk :: C.VerificationKey C.PaymentKey
carolWalletVk = unsafePerformIO $ readFileTextEnvelopeThrow "./../devnet/credentials/carol.vk"
{-# NOINLINE carolWalletVk #-}

waitToReceiveFunds :: DirectBackend -> C.NetworkId -> C.VerificationKey C.PaymentKey -> C.TxId -> IO (UTxO.UTxO Era)
waitToReceiveFunds backend networkId userkey expectedTxId = do
  utxo <- Backend.queryUTxO backend [vkAddress networkId userkey]
  let inputs = filter (\(C.TxIn txid _) -> txid == expectedTxId) (toList $ UTxO.inputSet utxo)
  if null inputs
    then threadDelay 1 >> waitToReceiveFunds backend networkId userkey expectedTxId
    else pure utxo

waitLockInHead :: TVar (Maybe b) -> IO b
waitLockInHead var = do
  lockDatum <- readTVarIO var
  case lockDatum of
    Nothing -> threadDelay 1 >> waitLockInHead var
    Just d -> pure d

vkAddress :: C.NetworkId -> C.VerificationKey C.PaymentKey -> C.Address C.ShelleyAddr
vkAddress networkId vk =
  C.makeShelleyAddress networkId (C.PaymentCredentialByKey $ C.verificationKeyHash vk) C.NoStakeAddress

generateInvoice :: C.Address C.ShelleyAddr -> C.Value -> IO (I.StandardInvoice, I.PreImage)
generateInvoice recipient value = do
  date <- addUTCTime (60 * 60 * 24 * 30) <$> getCurrentTime
  (invoice, preImage) <- I.generateStandardInvoice recipient value date
  pure (invoice, preImage)

generateInvoiceWithTime :: UTCTime -> C.Address C.ShelleyAddr -> C.Value -> IO (I.StandardInvoice, I.PreImage)
generateInvoiceWithTime now recipient value = do
  (invoice, preImage) <- I.generateStandardInvoice recipient value now
  pure (invoice, preImage)

buildRefundTx ::
  DirectBackend ->
  UTxO.UTxO Era ->
  AddressInEra ->
  AddressInEra ->
  C.Value ->
  UTxO.UTxO Era ->
  IO Tx
buildRefundTx backend utxo recipient changeAddress val collateralUTxO = do
  let out =
        TxOut
          recipient
          val
          C.TxOutDatumNone
          C.ReferenceScriptNone

  systemStart <- Backend.querySystemStart backend QueryTip
  eraHistory <- Backend.queryEraHistory backend QueryTip
  stakePools <- Backend.queryStakePools backend QueryTip

  pparams <- Backend.queryProtocolParameters backend QueryTip
  let scriptWitness =
        C.BuildTxWith $
          C.ScriptWitness scriptWitnessInCtx $
            PlutusScriptWitness
              htlcValidatorScript
              C.InlineScriptDatum
              (toScriptData Refund)
              maxTxExecutionUnits
  let collateral = toList $ UTxO.inputSet collateralUTxO
  let collateralLvlc = UTxO.totalLovelace collateralUTxO
  tip <- Backend.queryTip backend
  let currentSlot = case C.chainPointToSlotNo tip of
        Nothing -> error "Failed to convert ChainPoint to SlotNo"
        Just s -> s
  let upperSlot :: SlotNo = currentSlot
  let bodyContent =
        defaultTxBodyContent
          & C.addTxIns (map (,scriptWitness) (Set.toList $ C.inputSet utxo) <> map (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) collateral)
          & C.addTxInsCollateral collateral
          & C.setTxTotalCollateral (C.TxTotalCollateral C.BabbageEraOnwardsConway collateralLvlc)
          & C.addTxOuts [out]
          & C.setTxAuxScripts (C.TxAuxScripts C.AllegraEraOnwardsConway [C.ScriptInEra C.PlutusScriptV3InConway (PlutusScript htlcValidatorScript)])
          & C.setTxValidityLowerBound (C.TxValidityLowerBound C.AllegraEraOnwardsConway upperSlot)
          & C.setTxExtraKeyWits (C.TxExtraKeyWitnesses C.AlonzoEraOnwardsConway [C.verificationKeyHash aliceWalletVk])
          & C.setTxProtocolParams (C.BuildTxWith $ Just $ C.LedgerProtocolParameters pparams)

  case C.makeTransactionBodyAutoBalance
    C.shelleyBasedEra
    systemStart
    (C.toLedgerEpochInfo eraHistory)
    (C.LedgerProtocolParameters pparams)
    stakePools
    mempty
    mempty
    (utxo <> collateralUTxO)
    bodyContent
    changeAddress
    Nothing of
    Left e -> error $ show e
    Right tx -> pure $ flip Tx [] $ balancedTxBody tx

buildLockTx ::
  DirectBackend ->
  PParams LedgerEra ->
  C.NetworkId ->
  I.StandardInvoice ->
  UTxO.UTxO Era ->
  C.Address C.ShelleyAddr ->
  AddressInEra ->
  C.Value ->
  IO Tx
buildLockTx backend pparams networkId invoice utxo sender changeAddress val = do
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

buildClaimTX ::
  DirectBackend ->
  PParams LedgerEra ->
  I.PreImage ->
  AddressInEra ->
  [C.Hash C.PaymentKey] ->
  C.Value ->
  UTxO.UTxO Era ->
  UTxO.UTxO Era ->
  C.TxIn ->
  C.AddressInEra Era ->
  IO (C.Tx Era)
buildClaimTX backend pparams preImage recipient expectedKeyHashes val claimUTxO collateralUTxO collateral changeAddress = do
  let maxTxExecutionUnits' =
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
              maxTxExecutionUnits'

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
