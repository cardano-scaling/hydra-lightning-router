module Hydra.HTLC.Tx (
    makeHTLCLockupTxOut,
    constructBalancedHTLCLockupTx,
) where

import Cardano.Api qualified as Api
import Cardano.Api.ProtocolParameters (LedgerProtocolParameters)
import Cardano.Api.Tx qualified as Tx
import Data.Function ((&))
import Hydra.HTLC.Address (makeHtlcScriptAddress)
import Hydra.HTLC.Data qualified as HTLC
import PlutusTx qualified as Plutus

makeHTLCLockupTxOut ::
    Api.BabbageEraOnwards era ->
    Api.NetworkId ->
    HTLC.Datum ->
    Tx.TxOutValue era ->
    Tx.TxOut ctx era
makeHTLCLockupTxOut
    beo
    networkId
    datum
    value =
        Api.TxOut
            (makeHtlcScriptAddress (Api.convert beo) networkId)
            value
            (Api.TxOutDatumInline beo $ Api.unsafeHashableScriptData $ Api.fromPlutusData $ Plutus.toData datum)
            Api.ReferenceScriptNone

constructBalancedHTLCLockupTx ::
    Api.BabbageEraOnwards era ->
    Api.NetworkId ->
    Api.AddressInEra era ->
    Api.UTxO era ->
    LedgerProtocolParameters era ->
    Api.LedgerEpochInfo ->
    Api.SystemStart ->
    [Api.ShelleyWitnessSigningKey] ->
    HTLC.Datum ->
    Tx.TxOutValue era ->
    Either (Api.TxBodyErrorAutoBalance era) (Api.Tx era)
constructBalancedHTLCLockupTx
    beo
    networkId
    changeAddr
    utxo
    lpp
    ledgerEpochInfo
    systemStart
    shelleyWitSigningKeys
    datum
    value = do
        let txBodyContent =
                Api.defaultTxBodyContent (Api.convert beo)
                    & Tx.addTxOuts [makeHTLCLockupTxOut beo networkId datum value]
        Api.constructBalancedTx
            (Api.convert beo)
            txBodyContent
            changeAddr
            Nothing
            utxo
            lpp
            ledgerEpochInfo
            systemStart
            mempty
            mempty
            mempty
            shelleyWitSigningKeys
