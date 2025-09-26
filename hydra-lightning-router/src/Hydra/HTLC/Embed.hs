{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.HTLC.Embed (plutusJSON, htlcValidatorScript) where

import Cardano.Api (PlutusScript, PlutusScriptV3)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, nth, _String)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short (toShort)
import Data.FileEmbed (makeRelativeToProject)
import Data.Text.Encoding (encodeUtf8)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)

plutusJSON :: Aeson.Value
plutusJSON =
    $( do
        path <- makeRelativeToProject "./plutus.json"
        bs <- runIO $ BS.readFile path
        case Aeson.decodeStrict bs of
          Nothing -> fail "Invalid plutus.json file."
          Just (value :: Aeson.Value) -> lift value
     )

htlcValidatorScript :: PlutusScript PlutusScriptV3
htlcValidatorScript =
    PlutusScriptSerialised . toShort . Base16.decodeLenient . encodeUtf8 $
        plutusJSON ^. key "validators" . nth 0 . key "compiledCode" . _String
