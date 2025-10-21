{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class (liftIO)
import Cardano.Api qualified as C
import Convex.BuildTx (execBuildTx)
import Convex.Class (MonadMockchain, querySlotNo, setSlot)
import Convex.CoinSelection (BalanceTxError, ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection qualified as CoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError, inBabbage, utcTimeToSlot)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Hydra.HTLC.BuildTx (claimHTLC, payHTLC, refundHTLC)
import Hydra.HTLC.Conversions (standardInvoiceToHTLCDatum)
import Hydra.HTLC.Data qualified as HTLC
import Hydra.HTLC.Upstream (shelleyPayAddrToPaymentKey)
import Hydra.Invoice qualified as I
import PlutusTx.Prelude (BuiltinByteString, toBuiltin)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

balanceAndPayHTLC ::
  forall era m.
  (MonadFail m) =>
  (C.MonadError (BalanceTxError era) m) =>
  (MonadMockchain era m) =>
  (C.IsBabbageBasedEra era) =>
  Wallet.Wallet ->
  HTLC.Datum ->
  C.Value ->
  m C.TxIn
balanceAndPayHTLC wallet datum value = inBabbage @era $ do
  let tx = execBuildTx $ payHTLC Defaults.networkId datum value
  i <- C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty wallet tx TrailingChange []
  pure (C.TxIn i (C.TxIx 0))

balanceAndClaimHTLC ::
  forall era m.
  (MonadFail m) =>
  (MonadMockchain era m) =>
  (C.MonadError (BalanceTxError era) m) =>
  (C.IsBabbageBasedEra era) =>
  (C.HasScriptLanguageInEra C.PlutusScriptV3 era) =>
  Wallet.Wallet ->
  C.TxIn ->
  HTLC.Datum ->
  C.TxValidityUpperBound era ->
  C.Hash C.PaymentKey ->
  BuiltinByteString ->
  m C.TxId
balanceAndClaimHTLC wallet ref datum ub pkh secret = inBabbage @era $ do
  let tx = execBuildTx $ claimHTLC ref datum ub pkh secret
  C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty wallet tx TrailingChange []

balanceAndRefundHTLC ::
  forall era m.
  (MonadFail m) =>
  (MonadMockchain era m) =>
  (C.MonadError (BalanceTxError era) m) =>
  (C.IsBabbageBasedEra era) =>
  (C.HasScriptLanguageInEra C.PlutusScriptV3 era) =>
  Wallet.Wallet ->
  C.TxIn ->
  HTLC.Datum ->
  C.TxValidityLowerBound era ->
  C.Hash C.PaymentKey ->
  m C.TxId
balanceAndRefundHTLC wallet ref datum lb pkh = inBabbage @era $ do
  let tx = execBuildTx $ refundHTLC ref datum lb pkh
  C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty wallet tx TrailingChange []

canSpendToHTLCScript :: Assertion
canSpendToHTLCScript = do
  let sender = Wallet.address Defaults.networkId Wallet.w1
  let recipient = Wallet.address Defaults.networkId Wallet.w2
  let date = UTCTime (fromGregorian 2040 01 01) (secondsToDiffTime 0)
  (invoice, k) <- I.generateStandardInvoice recipient (C.lovelaceToValue 1_000_000) date
  let Just dat = standardInvoiceToHTLCDatum invoice sender
  mockchainSucceeds $ failOnError $ balanceAndPayHTLC Wallet.w1 dat (I.amount invoice)

canClaimFromHTLCScript :: Assertion
canClaimFromHTLCScript = do
  let sender = Wallet.address Defaults.networkId Wallet.w1
  let recipient = Wallet.address Defaults.networkId Wallet.w2
  let date = UTCTime (fromGregorian 2026 01 01) (secondsToDiffTime 0)
  (invoice, k) <- I.generateStandardInvoice recipient (C.lovelaceToValue 1_000_000) date
  let Just dat = standardInvoiceToHTLCDatum invoice sender
  let Just pkh = shelleyPayAddrToPaymentKey recipient
  let Right (sn, _, _) = utcTimeToSlot Defaults.eraHistory Defaults.systemStart date
  let ub = C.TxValidityUpperBound C.shelleyBasedEra $ Just sn
  let secret = toBuiltin $ C.serialiseToRawBytes $ I.fromPreImage k
  mockchainSucceeds $ failOnError $ do
    ref <- balanceAndPayHTLC Wallet.w1 dat (I.amount invoice)
    balanceAndClaimHTLC Wallet.w2 ref dat ub pkh secret

canRefundFromHTLCScript :: Assertion
canRefundFromHTLCScript = do
  let sender = Wallet.address Defaults.networkId Wallet.w1
  let recipient = Wallet.address Defaults.networkId Wallet.w2
  let date = UTCTime (fromGregorian 2026 01 01) (secondsToDiffTime 0)
  (invoice, k) <- I.generateStandardInvoice recipient (C.lovelaceToValue 1_000_000) date
  let Just dat = standardInvoiceToHTLCDatum invoice sender
  let Just pkh = shelleyPayAddrToPaymentKey sender
  let Right (sn, _, _) = utcTimeToSlot Defaults.eraHistory Defaults.systemStart date
  let lb = C.TxValidityLowerBound C.allegraBasedEra (sn + 1)
  mockchainSucceeds $ failOnError $ do
    setSlot $ sn + 1
    ref <- balanceAndPayHTLC Wallet.w1 dat (I.amount invoice)
    balanceAndRefundHTLC Wallet.w1 ref dat lb pkh

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testGroup
        "payments"
        [ testCase "can spend to htlc script" canSpendToHTLCScript,
          testCase "can claim script output from htlc script" canClaimFromHTLCScript,
          testCase "can refund script output from htlc script" canRefundFromHTLCScript
        ]
    ]

main :: IO ()
main = defaultMain tests
