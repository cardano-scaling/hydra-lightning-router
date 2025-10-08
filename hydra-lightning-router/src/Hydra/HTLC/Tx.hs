module Hydra.HTLC.Tx where

import Cardano.Api (Tx, ConwayEra)
import Hydra.HTLC.Embed

buildHtlcTx :: UTxO -> Tx ConwayEra
buildHtlcTx = do
  let scriptAddress = mkScriptAddress networkId serializedScript
  let scriptOutput =
         mkTxOutAutoBalance
            pparams
                scriptAddress
                (lovelaceToValue 0)
                (mkTxOutDatumHash ())
                ReferenceScriptNone


  
