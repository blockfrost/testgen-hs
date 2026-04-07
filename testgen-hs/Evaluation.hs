{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Evaluation
  ( WrappedTransactionScriptFailure (..),
    writeJson,
    eval'Conway,
  )
where

import CLI (GenSize (..), NumCases (..), Seed (..))
import Cardano.Api.Internal.Orphans ()
import Cardano.Ledger.Alonzo.Plutus.Evaluate (evalTxExUnits)
import Cardano.Ledger.Api (ConwayEra, PParams, TransactionScriptFailure)
import qualified Cardano.Ledger.Api as Ledger
import Cardano.Ledger.Api.Tx (RedeemerReport, Tx)
import Cardano.Ledger.Api.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Slot ()
import Cardano.Slotting.Time (SystemStart (..))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import Data.Proxy (Proxy)
import Data.Text (Text)
import Encoder (ogmiosSuccess, serializeTransactionScriptFailure)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import Test.Consensus.Cardano.Generators ()
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QGen
import qualified Test.QuickCheck.Random as QCRandom

newtype WrappedTransactionScriptFailure era = WrappedTransactionScriptFailure
  { unWrappedTransactionScriptFailure ::
      TransactionScriptFailure era
  }

instance ToJSON (WrappedTransactionScriptFailure ConwayEra) where
  toEncoding (WrappedTransactionScriptFailure x) =
    serializeTransactionScriptFailure x
  toJSON wrapped =
    case J.decode (AesonEncoding.encodingToLazyByteString (J.toEncoding wrapped)) :: Maybe J.Value of
      Just v -> v
      Nothing ->
        error "serializeTransactionScriptFailure produced invalid JSON"

instance QC.Arbitrary (Ledger.TransactionScriptFailure Ledger.ConwayEra) where
  arbitrary =
    QC.oneof
      [ Ledger.RedeemerPointsToUnknownScriptHash <$> QC.arbitrary,
        (`Ledger.MissingScript` Map.empty) <$> QC.arbitrary,
        Ledger.MissingDatum <$> QC.arbitrary,
        Ledger.UnknownTxIn <$> QC.arbitrary,
        Ledger.InvalidTxIn <$> QC.arbitrary,
        pure $ Ledger.IncompatibleBudget (ExBudget (ExCPU 999) (ExMemory 888)),
        Ledger.NoCostModelInLedgerState <$> QC.arbitrary,
        Ledger.ContextError <$> QC.arbitrary
      ]

writeJson ::
  Proxy ConwayEra ->
  Seed ->
  GenSize ->
  NumCases ->
  IO ()
writeJson _ (Seed seed) (GenSize size) (NumCases numCases) = do
  let gen :: QGen.Gen [WrappedTransactionScriptFailure ConwayEra]
      gen =
        QC.vectorOf numCases $
          WrappedTransactionScriptFailure <$> (QC.arbitrary :: QC.Gen (TransactionScriptFailure ConwayEra))
      xs :: [WrappedTransactionScriptFailure ConwayEra]
      xs = QGen.unGen gen (QCRandom.mkQCGen seed) size
  BL8.putStrLn (J.encode xs)

eval'Conway ::
  PParams ConwayEra ->
  Tx ConwayEra ->
  UTxO ConwayEra ->
  EpochInfo (Either Text) ->
  SystemStart ->
  J.Value
eval'Conway pparams tx utxo epochInfo systemStart =
  case J.decode (AesonEncoding.encodingToLazyByteString (ogmiosSuccess report)) of
    Just v -> v
    Nothing -> error "ogmiosSuccess produced invalid JSON"
  where
    report :: RedeemerReport ConwayEra
    report = evalTxExUnits pparams tx utxo epochInfo systemStart

