{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Cardano.Api qualified as C
import Convex.BuildTx (execBuildTx, payToScriptDatumHash, spendPlutus, TxBuilder)
import Convex.Class (MonadMockchain)
import Convex.CoinSelection (BalanceTxError, ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection qualified as CoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError, inBabbage)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Hydra.HTLC.Data (Datum (Datum))
import Hydra.HTLC.Data qualified as HTLC
import Hydra.HTLC.Embed (htlcValidatorScript)
import PlutusTx (ToData(..), toData)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, Assertion)
import Hydra.Invoice qualified as I

import Cardano.Api (Address, ShelleyAddr, serialiseAddress, serialiseToRawBytes)
import Cardano.Crypto.Hash qualified as Crypto
import Data.ByteString qualified as BS
import Data.Time (UTCTime(..), diffUTCTime, secondsToNominalDiffTime, fromGregorian, secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Hydra.Invoice (PaymentId (..), StandardInvoice (..))
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Prelude (BuiltinByteString, toBuiltin)

plutusScript :: (C.IsPlutusScriptLanguage lang) => C.PlutusScript lang -> C.Script lang
plutusScript = C.PlutusScript C.plutusScriptVersion

alwaysSucceedsScript :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedsScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

-- Helper function to convert UTCTime to POSIXTime
utcTimeToPOSIXTime :: UTCTime -> V3.POSIXTime
utcTimeToPOSIXTime utc =
  let posixSeconds = floor $ utcTimeToPOSIXSeconds utc
  in V3.POSIXTime posixSeconds

standardInvoiceToHTLCDatum :: I.StandardInvoice -> C.Address C.ShelleyAddr -> Datum
standardInvoiceToHTLCDatum invoice sender
  = Datum {
      HTLC.hash = toBuiltin $ Crypto.hashToBytes $ fromPaymentId $ I.paymentId invoice,
      HTLC.receiver = toBuiltin $ serialiseToRawBytes $ I.recipient invoice,
      HTLC.timeout = utcTimeToPOSIXTime $ I.date invoice,
      HTLC.sender = toBuiltin $ serialiseToRawBytes sender
    }

payToPlutusScript ::
  forall era lang m a.
  (MonadFail m) =>
  (C.MonadError (BalanceTxError era) m) =>
  (C.IsPlutusScriptLanguage lang) =>
  (MonadMockchain era m) =>
  (C.IsBabbageBasedEra era) =>
  (ToData a) =>
  Wallet.Wallet ->
  C.PlutusScript lang ->
  a ->
  C.Value ->
  m C.TxIn
payToPlutusScript wallet script datum value = inBabbage @era $ do
  let tx = execBuildTx $ payToScriptDatumHash Defaults.networkId (plutusScript script) datum C.NoStakeAddress value
  i <- C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty wallet tx TrailingChange []
  pure (C.TxIn i (C.TxIx 0))

spendFromPlutusScript ::
  forall era lang m dat red.
  (MonadFail m) =>
  (MonadMockchain era m) =>
  (C.MonadError (BalanceTxError era) m) =>
  (C.HasScriptLanguageInEra lang era) =>
  (C.IsPlutusScriptLanguage lang) =>
  (C.IsBabbageBasedEra era) =>
  ToData dat =>
  ToData red =>
  Wallet.Wallet ->
  C.PlutusScript lang ->
  C.TxIn ->
  dat ->
  red ->
  m C.TxId
spendFromPlutusScript wallet script ref datum redeemer = inBabbage @era $ do
  let tx = execBuildTx $ spendPlutus ref script datum redeemer
  C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty wallet tx TrailingChange []

main :: IO ()
main = defaultMain tests

canSpendToAlwaysSucceedsScript :: Assertion
canSpendToAlwaysSucceedsScript = mockchainSucceeds $ failOnError $ payToPlutusScript Wallet.w1 alwaysSucceedsScript () (C.lovelaceToValue 1_000_000)

canSpendFromAlwaysSucceedsScript :: Assertion
canSpendFromAlwaysSucceedsScript = mockchainSucceeds $ failOnError $ do
  ref <- payToPlutusScript Wallet.w1 alwaysSucceedsScript () (C.lovelaceToValue 1_000_000)
  spendFromPlutusScript Wallet.w1 alwaysSucceedsScript ref () ()

canSpendToHTLCScript :: Assertion
canSpendToHTLCScript = do
  let sender = Wallet.address Defaults.networkId Wallet.w1
  let recipient = Wallet.address Defaults.networkId Wallet.w2
  let date = UTCTime (fromGregorian 2040 01 01) (secondsToDiffTime 0)
  (invoice, k) <- I.generateStandardInvoice sender (C.lovelaceToValue 1_000_000) date
  let dat = standardInvoiceToHTLCDatum invoice sender
  mockchainSucceeds $ failOnError $ payToPlutusScript Wallet.w1 htlcValidatorScript dat (I.amount invoice)

canClaimFromHTLCScript :: Assertion
canClaimFromHTLCScript = do
  let sender = Wallet.address Defaults.networkId Wallet.w1
  let recipient = Wallet.address Defaults.networkId Wallet.w2
  let date = UTCTime (fromGregorian 2040 01 01) (secondsToDiffTime 0)
  (invoice, k) <- I.generateStandardInvoice sender (C.lovelaceToValue 1_000_000) date
  let dat = standardInvoiceToHTLCDatum invoice sender
  mockchainSucceeds $ failOnError $ do
    ref <- payToPlutusScript Wallet.w1 htlcValidatorScript dat (I.amount invoice)
    spendFromPlutusScript Wallet.w1 htlcValidatorScript ref () ()

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testGroup
        "payments"
        [ testCase "spending a public key output to alwaysSucceeds script" canSpendToAlwaysSucceedsScript,
          testCase "spending a script output from alwaysSucceeds script" canSpendFromAlwaysSucceedsScript,
          testCase "spending a public key output to htlc script" canSpendToHTLCScript,
          testCase "spending a script output from htlc script" canClaimFromHTLCScript
        ]
    ]
