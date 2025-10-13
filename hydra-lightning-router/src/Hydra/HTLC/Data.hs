{-# LANGUAGE TemplateHaskell #-}

module Hydra.HTLC.Data
  ( Datum (Datum),
    Redeemer (Claim, Refund),
  )
where

import Data.Kind (Type)
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinByteString)
import Prelude qualified as Haskell

type Datum :: Type
data Datum
  = Datum
  { hash :: BuiltinByteString,
    timeout :: V3.POSIXTime,
    sender :: BuiltinByteString,
    receiver :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Ord)

PlutusTx.unstableMakeIsData ''Datum

type Redeemer :: Type
data Redeemer
  = Claim BuiltinByteString
  | Refund

PlutusTx.unstableMakeIsData ''Redeemer
