module Hydra.HTLC.Address (
    makeHtlcScriptAddress,
) where

import Cardano.Api (
    AddressInEra,
    NetworkId,
    PaymentCredential (PaymentCredentialByScript),
    Script (PlutusScript),
    ShelleyBasedEra,
    StakeAddressReference (NoStakeAddress),
    hashScript,
    makeShelleyAddressInEra,
    plutusScriptVersion,
 )
import Hydra.HTLC.Embed (htlcValidatorScript)

makeHtlcScriptAddress ::
    ShelleyBasedEra era ->
    NetworkId ->
    AddressInEra era
makeHtlcScriptAddress sbe networkId =
    makeShelleyAddressInEra
        sbe
        networkId
        (PaymentCredentialByScript $ hashScript $ PlutusScript plutusScriptVersion htlcValidatorScript)
        NoStakeAddress
