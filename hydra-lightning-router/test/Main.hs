{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Convex.BuildTx (TxBuilder, execBuildTx, payToScriptDatumHash, spendPlutus)
import Convex.Class (MonadMockchain, setPOSIXTime, sendTx)
import Convex.CoinSelection (BalanceTxError, ChangeOutputPosition (TrailingChange), balanceForWallet)
import Control.Monad (forM_, void)
import Convex.MockChain.CoinSelection qualified as CoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError, inBabbage)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.ByteString qualified as BS
import Data.Time (UTCTime (..), diffUTCTime, fromGregorian, secondsToDiffTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Hydra.HTLC.Data (Datum (Datum))
import Hydra.HTLC.Data qualified as HTLC
import Hydra.HTLC.Embed (htlcValidatorScript)
import Hydra.Invoice (PaymentId (..), StandardInvoice (..))
import Hydra.Invoice qualified as I
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx (ToData (..), toData)
import PlutusTx.Prelude (BuiltinByteString, toBuiltin)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

plutusScript :: (C.IsPlutusScriptLanguage lang) => C.PlutusScript lang -> C.Script lang
plutusScript = C.PlutusScript C.plutusScriptVersion

alwaysSucceedsScript :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedsScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

standardInvoiceToHTLCDatum :: I.StandardInvoice -> C.Address C.ShelleyAddr -> Maybe Datum
standardInvoiceToHTLCDatum invoice sender = do
  b <- C.shelleyPayAddrToPlutusPubKHash $ I.recipient invoice
  c <- C.shelleyPayAddrToPlutusPubKHash $ I.recipient invoice
  pure $ Datum
          { HTLC.hash = toBuiltin $ Crypto.hashToBytes $ fromPaymentId $ I.paymentId invoice,
            HTLC.receiver = b,
            HTLC.timeout = V3.POSIXTime $ utcTimeToPOSIXSeconds $ I.date invoice,
            HTLC.sender = c
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
  (ToData dat) =>
  (ToData red) =>
  Wallet.Wallet ->
  C.PlutusScript lang ->
  C.TxIn ->
  dat ->
  red ->
  m C.TxId
spendFromPlutusScript wallet script ref datum redeemer = inBabbage @era $ do
  let tx = execBuildTx $ spendPlutus ref script datum redeemer
  C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty wallet tx TrailingChange []

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
  let Just dat = standardInvoiceToHTLCDatum invoice sender
  mockchainSucceeds $ failOnError $ payToPlutusScript Wallet.w1 htlcValidatorScript dat (I.amount invoice)

canClaimFromHTLCScript :: Assertion
canClaimFromHTLCScript = do
  let sender = Wallet.address Defaults.networkId Wallet.w1
  let recipient = Wallet.address Defaults.networkId Wallet.w2
  let date = UTCTime (fromGregorian 2026 01 01) (secondsToDiffTime 0)
  (invoice, k) <- I.generateStandardInvoice sender (C.lovelaceToValue 1_000_000) date
  let Just dat = standardInvoiceToHTLCDatum invoice sender
  let red = HTLC.Claim $ toBuiltin $ C.serialiseToRawBytes $ I.fromPreImage k
  mockchainSucceeds $ failOnError $ do
    ref <- payToPlutusScript Wallet.w1 htlcValidatorScript dat (I.amount invoice)
    spendFromPlutusScript Wallet.w2 htlcValidatorScript ref dat red

canRefundFromHTLCScript :: Assertion
canRefundFromHTLCScript = do
  let sender = Wallet.address Defaults.networkId Wallet.w1
  let recipient = Wallet.address Defaults.networkId Wallet.w2
  let date = UTCTime (fromGregorian 2026 01 01) (secondsToDiffTime 0)
  (invoice, k) <- I.generateStandardInvoice sender (C.lovelaceToValue 1_000_000) date
  let Just dat = standardInvoiceToHTLCDatum invoice sender
  mockchainSucceeds $ failOnError $ do
    ref <- payToPlutusScript Wallet.w1 htlcValidatorScript dat (I.amount invoice)
    spendFromPlutusScript Wallet.w1 htlcValidatorScript ref dat HTLC.Refund

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testGroup
        "payments"
        [ testCase "can spend to alwaysSucceeds script" canSpendToAlwaysSucceedsScript,
          testCase "can spend from alwaysSucceeds script" canSpendFromAlwaysSucceedsScript,
          testCase "can spend to htlc script" canSpendToHTLCScript,
          testCase "can claim script output from htlc script" canClaimFromHTLCScript,
          testCase "can refund script output from htlc script" canRefundFromHTLCScript
        ]
    ]

main :: IO ()
main = defaultMain tests
