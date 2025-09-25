{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.HTLC.Embed (plutusJSON, htlcValidatorScript) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, nth, _String)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short (toShort)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text.Encoding (encodeUtf8)

plutusJSON :: Aeson.Value
plutusJSON =
    case Aeson.decodeStrict $(makeRelativeToProject "./plutus.json" >>= embedFile) of
        Nothing -> error "Invalid blueprint: plutus.json"
        Just value -> value

htlcValidatorScript :: PlutusScript PlutusScriptV3
htlcValidatorScript =
    PlutusScriptSerialised . toShort . Base16.decodeLenient . encodeUtf8 $
        plutusJSON ^. key "validators" . nth 0 . key "compiledCode" . _String
