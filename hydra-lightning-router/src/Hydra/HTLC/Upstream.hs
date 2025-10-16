module Hydra.HTLC.Upstream
  ( shelleyPayAddrToPaymentKey,
  )
where

import Cardano.Api qualified as C

-- Upstreamed here: https://github.com/IntersectMBO/cardano-api/pull/976
shelleyPayAddrToPaymentKey :: C.Address C.ShelleyAddr -> Maybe (C.Hash C.PaymentKey)
shelleyPayAddrToPaymentKey (C.ShelleyAddress _ pCred _) =
  case C.fromShelleyPaymentCredential pCred of
    C.PaymentCredentialByKey key -> Just key
    C.PaymentCredentialByScript _ -> Nothing
