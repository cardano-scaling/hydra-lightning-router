module Hydra.HTLC.Conversions
  ( standardInvoiceToHTLCDatum,
  )
where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Hydra.HTLC.Data qualified as HTLC
import Hydra.Invoice qualified as I
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Prelude (toBuiltin)

standardInvoiceToHTLCDatum :: I.StandardInvoice -> C.Address C.ShelleyAddr -> Maybe HTLC.Datum
standardInvoiceToHTLCDatum invoice sender = do
  b <- C.shelleyPayAddrToPlutusPubKHash $ I.recipient invoice
  c <- C.shelleyPayAddrToPlutusPubKHash sender
  pure $
    HTLC.Datum
      { HTLC.hash = toBuiltin $ Crypto.hashToBytes $ I.fromPaymentId $ I.paymentId invoice,
        HTLC.receiver = b,
        HTLC.timeout = V3.POSIXTime $ floor $ utcTimeToPOSIXSeconds (I.date invoice) * 1000,
        HTLC.sender = c
      }
