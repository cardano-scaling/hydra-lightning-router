module Hydra.HTLC.BuildTx
  ( payHTLC,
    claimHTLC,
    refundHTLC,
  )
where

import Cardano.Api qualified as C
import Convex.BuildTx (MonadBuildTx, addRequiredSignature, payToScriptDatumHash, spendPlutus)
import Hydra.HTLC.Data qualified as HTLC
import Hydra.HTLC.Embed (htlcValidatorScript)
import PlutusTx.Prelude (BuiltinByteString)

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
  C.Hash C.PaymentKey ->
  BuiltinByteString ->
  m ()
claimHTLC ref datum pkh secret = do
  spendPlutus ref htlcValidatorScript datum $ HTLC.Claim secret
  addRequiredSignature pkh

refundHTLC ::
  (C.HasScriptLanguageInEra C.PlutusScriptV3 era) =>
  (C.IsAlonzoBasedEra era) =>
  (MonadBuildTx era m) =>
  C.TxIn ->
  HTLC.Datum ->
  C.Hash C.PaymentKey ->
  m ()
refundHTLC ref datum pkh = do
  spendPlutus ref htlcValidatorScript datum HTLC.Refund
  addRequiredSignature pkh
