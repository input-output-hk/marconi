{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides several type aliases and utility functions to deal with them.
module Marconi.ChainIndex.Types (
  -- * Addresses alias used to query marconi
  TargetAddresses,

  -- * Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  UtxoIndexerConfig (..),

  -- * Aliases for the current Cardano era
  CurrentEra,
  pattern AsCurrentEra,
  pattern CurrentEra,
  TxOut,

  -- * Aliases to ease concept mapping between plutus types and cardano types
  TxOutRef,
  txOutRef,

  -- * Database file names
  utxoDbName,
  addressDatumDbName,
  scriptTxDbName,
  epochStateDbName,
  mintBurnDbName,
  SecurityParam (SecurityParam),
  IndexingDepth (MinIndexingDepth, MaxIndexingDepth),
  TxIndexInBlock (TxIndexInBlock),
) where

import Cardano.Api qualified as C
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word64)
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL

-- | Type represents non empty list of Bech32 Shelley compatable addresses
type TargetAddresses = NonEmpty (C.Address C.ShelleyAddr)

data UtxoIndexerConfig = UtxoIndexerConfig
  { ucTargetAddresses :: Maybe TargetAddresses
  -- ^ List of address utxo indexer to follow
  , ucEnableUtxoTxOutRef :: Bool
  -- ^ enable utxo indexer to store txOut refScript
  }

-- | An alias for the current era, to ease the transition from one era to the next one
type CurrentEra = C.BabbageEra

pattern CurrentEra :: C.CardanoEra CurrentEra
pattern CurrentEra = C.BabbageEra

pattern AsCurrentEra :: C.AsType CurrentEra
pattern AsCurrentEra = C.AsBabbageEra

-- | A Cardano TxOut of the current Era
type TxOut = C.TxOut C.CtxTx CurrentEra

{- | A reference to a transaction output. This is a
 pair of a transaction reference, and an index indicating which of the outputs
 of that transaction we are referring to.
-}
type TxOutRef = C.TxIn

txOutRef :: C.TxId -> C.TxIx -> C.TxIn
txOutRef = C.TxIn

data IndexingDepth = MinIndexingDepth !Word64 | MaxIndexingDepth
  deriving (Show, Eq)

newtype SecurityParam = SecurityParam Word64
  deriving newtype (Eq, Ord, Bounded, Enum, Real, Num, Read, Integral, Show)

newtype TxIndexInBlock = TxIndexInBlock Word64
  deriving newtype (Eq, Ord, Bounded, Enum, Real, Num, Read, Integral, Show, Aeson.FromJSON, Aeson.ToJSON, SQL.ToField, SQL.FromField)

-- * Database file names

utxoDbName :: FilePath
utxoDbName = "utxo.db"

addressDatumDbName :: FilePath
addressDatumDbName = "addressdatum.db"

scriptTxDbName :: FilePath
scriptTxDbName = "scripttx.db"

epochStateDbName :: FilePath
epochStateDbName = "epochstate.db"

mintBurnDbName :: FilePath
mintBurnDbName = "mintburn.db"
