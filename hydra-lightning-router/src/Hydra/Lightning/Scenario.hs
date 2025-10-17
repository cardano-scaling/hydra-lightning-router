{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Hydra.Lightning.Scenario
  ( main,
  )
where

import Cardano.Api (UTxO, Coin, Tx, PolicyId, PolicyAssets)
import Cardano.Api qualified as C
import Network.WebSockets (receiveDataMessage, runClient, sendTextData, Connection)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Aeson.Lens (key)
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Network.HTTP.Req qualified as Req
import Data.Proxy (Proxy(Proxy))
import Data.Map (Map)
import Control.Lens ((^.))
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)


-- | Helper to make it easy to obtain a commit tx using some wallet utxo.
-- Create a commit tx using the hydra-node for later submission.
requestCommitTx :: C.IsCardanoEra era => Text -> Int -> UTxO era -> IO (Tx era)
requestCommitTx host port utxos =
  requestCommitTx' host port utxos Nothing Nothing


-- >>> decodeBase16 "dflkgjdjgdh"
-- Left "Not base 16"
decodeBase16 :: MonadFail f => Text -> f ByteString
decodeBase16 =
  either fail pure . Base16.decode . encodeUtf8

instance C.IsShelleyBasedEra era => Aeson.ToJSON (Tx era) where
  toJSON tx =
    -- XXX: This is a deprecated function, but the only one that produces the
    -- right 'Tx ConwayEra' in the envelope type. Cardano-api will be
    -- fixing the 'HasTextEnvelope' instance for 'Tx era' and then we can use
    -- 'serialiseToTextEnvelope' here.
    case Aeson.toJSON $ C.serialiseToTextEnvelope Nothing tx of
      Aeson.Object km ->
        Aeson.Object $ KeyMap.insert "txId" (Aeson.toJSON $ C.getTxId $ C.getTxBody tx) km
      v -> v

instance Aeson.FromJSON (Tx C.ConwayEra) where
  parseJSON =
    Aeson.withObject "Tx" $ \o -> do
      hexText <- o Aeson..: "cborHex"
      -- NOTE: We deliberately ignore the "type" to be backwards compatible
      bytes <- decodeBase16 hexText
      case C.deserialiseFromCBOR (C.proxyToAsType (Proxy @(Tx C.ConwayEra))) bytes of
        Left e -> fail $ show e
        Right tx -> do
          -- NOTE: Check txId equivalence only if present.
          (o Aeson..:? "txId") >>= \case
            Just txid'
              | txid' /= C.txId tx -> fail "txId not matching"
            _ -> pure tx



data DraftCommitTxResponse = DraftCommitTxResponse {
    commitTx :: Tx C.ConwayEra
  }
   deriving stock (Show, Eq, Generic)
   deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | Helper to make it easy to obtain a commit tx using some wallet utxo
-- optional amount of lovelace and optional map of assets.
-- Create a commit tx using the hydra-node for later submission.
requestCommitTx' :: Text -> Int -> UTxO C.ConwayEra -> Maybe Coin -> Maybe (Map PolicyId PolicyAssets) -> IO (Tx C.ConwayEra)
requestCommitTx' hostname port utxos amount tokens =
  Req.runReq Req.defaultHttpConfig request <&> commitTx . Req.responseBody
 where
  request =
    Req.req
      Req.POST
      (Req.http hostname Req./: "commit")
      (Req.ReqBodyJson $ Aeson.object $ ["utxos" .= utxos, "amount" .= amount])
      (Proxy :: Proxy (Req.JsonResponse (DraftCommitTxResponse)))
      (Req.port (fromInteger . toInteger $ port))


-- TODO: Actually use CNT.
-- | Create an input as expected by 'send'.
tagged :: Text -> [Pair] -> Aeson.Value
tagged tag pairs = Aeson.object $ ("tag" Aeson..= tag) : pairs

send :: Connection -> Aeson.Value -> IO ()
send conn = sendTextData conn . Aeson.encode

main :: IO ()
main = do
  runClient "127.0.0.1" 4001 "/?history=no" $ \aliceConn -> do
    runClient "127.0.0.1" 4002 "/?history=no" $ \bobConn -> do
      runClient "127.0.0.1" 4003 "/?history=no" $ \idaConn1 -> do
        runClient "127.0.0.1" 4004 "/?history=no" $ \idaConn2 -> do
          send aliceConn $ tagged "Init" []


          -- Alice Init
          -- Alice Commit Something
          -- Ida1 Commit 0
          -- Bob Init
          -- Ida2 Commit Something
          -- 0: Bob generates an invoice and keeps the secret.
          -- 1: Alice pays an HTLC on Head 1.
          -- 2: Ida observes HTLC on Head 1.
          -- 3: Ida constructs HTLC on Head 2 using the same hash.
          -- 4: Bob claims the money in Head 2 using the Claim redeemer.
          -- 5: Ida observes HTLC Preimage on Head 2.
          -- 6: Ida claims the money in Head 1 using the Preimage.
          receiveDataMessage aliceConn >>= print
          receiveDataMessage bobConn >>= print
          receiveDataMessage idaConn1 >>= print
          receiveDataMessage idaConn2 >>= print
