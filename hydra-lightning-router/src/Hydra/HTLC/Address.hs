module Hydra.HTLC.Address (
  makeHtlcScriptAddress
  ) where

import Cardano.Api (NetworkId, ShelleyBasedEra, AddressInEra, PaymentCredential(PaymentCredentialByScript), makeShelleyAddressInEra, Script(PlutusScript), hashScript, StakeAddressReference(NoStakeAddress), plutusScriptVersion)
import Hydra.HTLC.Embed (htlcValidatorScript)


makeHtlcScriptAddress
  :: ShelleyBasedEra era
  -> NetworkId
  -> AddressInEra era
makeHtlcScriptAddress sbe networkId =
  makeShelleyAddressInEra
    sbe
    networkId
    (PaymentCredentialByScript $ hashScript $ PlutusScript plutusScriptVersion htlcValidatorScript)
    NoStakeAddress
