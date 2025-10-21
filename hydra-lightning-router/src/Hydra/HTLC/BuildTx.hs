module Hydra.HTLC.BuildTx
  ( payHTLC,
    claimHTLC,
    refundHTLC,
  )
where

import Cardano.Api qualified as C
import Convex.BuildTx (MonadBuildTx, addRequiredSignature, payToScriptDatumHash, spendPlutus, addBtx)
import Hydra.HTLC.Data qualified as HTLC
import Hydra.HTLC.Embed (htlcValidatorScript)
import PlutusTx.Prelude (BuiltinByteString)

mustSpendAfter :: (MonadBuildTx era m) => C.TxValidityLowerBound era -> m ()
mustSpendAfter lb = do
  addBtx (C.setTxValidityLowerBound lb)

mustSpendBefore :: (MonadBuildTx era m) => C.TxValidityUpperBound era -> m ()
mustSpendBefore ub = do
  addBtx (C.setTxValidityUpperBound ub)

payHTLC ::
  (C.IsBabbageBasedEra era) =>
  (MonadBuildTx era m) =>
  C.NetworkId ->
  HTLC.Datum ->
  C.Value ->
  m ()
payHTLC networkId datum =
  payToScriptDatumHash networkId (C.PlutusScript C.plutusScriptVersion htlcValidatorScript) datum C.NoStakeAddress

claimHTLC ::
  (C.HasScriptLanguageInEra C.PlutusScriptV3 era) =>
  (C.IsAlonzoBasedEra era) =>
  (MonadBuildTx era m) =>
  C.TxIn ->
  HTLC.Datum ->
  C.TxValidityUpperBound era ->
  C.Hash C.PaymentKey ->
  BuiltinByteString ->
  m ()
claimHTLC ref datum ub pkh secret = do
  spendPlutus ref htlcValidatorScript datum $ HTLC.Claim secret
  addRequiredSignature pkh
  mustSpendBefore ub

refundHTLC ::
  (C.HasScriptLanguageInEra C.PlutusScriptV3 era) =>
  (C.IsAlonzoBasedEra era) =>
  (MonadBuildTx era m) =>
  C.TxIn ->
  HTLC.Datum ->
  C.TxValidityLowerBound era ->
  C.Hash C.PaymentKey ->
  m ()
refundHTLC ref datum lb pkh = do
  spendPlutus ref htlcValidatorScript datum HTLC.Refund
  addRequiredSignature pkh
  mustSpendAfter lb
