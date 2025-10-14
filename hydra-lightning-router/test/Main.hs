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
import Convex.Wallet.MockWallet qualified as Wallet
import Hydra.HTLC.Data (Datum (Datum))
import Hydra.HTLC.Data qualified as HTLC
import Hydra.HTLC.Embed (htlcValidatorScript)
import PlutusTx (ToData)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Hydra.Invoice qualified as I

plutusScript :: (C.IsPlutusScriptLanguage lang) => C.PlutusScript lang -> C.Script lang
plutusScript = C.PlutusScript C.plutusScriptVersion

alwaysSucceedsScript :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedsScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

standardInvoiceToHTLCDatum :: I.StandardInvoice -> C.Address C.ShelleyAddr -> Datum
standardInvoiceToHTLCDatum invoice sender
  = Datum {
      HTLC.hash = _ $ I.paymentId invoice,
      HTLC.receiver = _ (I.recipient invoice),
      HTLC.timeout = _ (I.date invoice),
      HTLC.sender = _ sender
    }

standardInvoiceToHTLCTx :: I.StandardInvoice -> TxBuilder era
standardInvoiceToHTLCTx invoice = execBuildTx $
  payToScriptDatumHash
    Defaults.networkId
    (plutusScript htlcValidatorScript)
    (standardInvoiceToHTLCDatum invoice)
    C.NoStakeAddress
    (I.amount invoice)

payToPlutusScript ::
  forall era lang m a.
  (MonadFail m) =>
  (C.MonadError (BalanceTxError era) m) =>
  (C.IsPlutusScriptLanguage lang) =>
  (MonadMockchain era m) =>
  (C.IsBabbageBasedEra era) =>
  (ToData a) =>
  C.PlutusScript lang ->
  a ->
  m C.TxIn
payToPlutusScript script datum = inBabbage @era $ do
  let tx = execBuildTx $ payToScriptDatumHash Defaults.networkId (plutusScript script) datum C.NoStakeAddress (C.lovelaceToValue 10_000_000)
  i <- C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []
  pure (C.TxIn i (C.TxIx 0))

spendFromPlutusScript ::
  forall era lang m.
  (MonadFail m) =>
  (MonadMockchain era m) =>
  (C.MonadError (BalanceTxError era) m) =>
  (C.HasScriptLanguageInEra lang era) =>
  (C.IsPlutusScriptLanguage lang) =>
  (C.IsBabbageBasedEra era) =>
  C.PlutusScript lang ->
  C.TxIn ->
  m C.TxId
spendFromPlutusScript script ref = inBabbage @era $ do
  let tx = execBuildTx $ spendPlutus ref script () ()
  C.getTxId . C.getTxBody <$> CoinSelection.tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []

main :: IO ()
main = defaultMain tests

mockDatum :: Datum
mockDatum = Datum "0000000000" 0 "foo" "bar"

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ testGroup
        "payments"
        [ testCase "spending a public key output to alwaysSucceeds script" (mockchainSucceeds $ failOnError $ payToPlutusScript alwaysSucceedsScript ()),
          testCase "spending a script output from alwaysSucceeds script" (mockchainSucceeds $ failOnError (payToPlutusScript alwaysSucceedsScript () >>= spendFromPlutusScript alwaysSucceedsScript)),
          testCase "spending a public key output to htlc script" (mockchainSucceeds $ failOnError $ payToPlutusScript htlcValidatorScript ()),
          testCase "spending a script output from htlc script" (mockchainSucceeds $ failOnError (payToPlutusScript htlcValidatorScript mockDatum >>= spendFromPlutusScript htlcValidatorScript))
        ]
    ]
