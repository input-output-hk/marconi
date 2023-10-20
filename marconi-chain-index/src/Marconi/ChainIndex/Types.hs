{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides several type aliases and utility functions to deal with them.
module Marconi.ChainIndex.Types (
  -- * Config for retrying
  RetryConfig (..),

  -- * Application tracer
  MarconiTrace,

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

  -- * Reexport from cardano-api-extended
  BlockEvent (..),

  -- * Database file names
  utxoDbName,
  addressDatumDbName,
  scriptTxDbName,
  epochStateDbName,
  mintBurnDbName,
  SecurityParam (SecurityParam),
  IndexingDepth (MinIndexingDepth, MaxIndexingDepth),
  TxIndexInBlock (TxIndexInBlock),
  ShouldFailIfResync (ShouldFailIfResync),

  -- * Configuration for index runners
  RunIndexerConfig (RunIndexerConfig),
  runIndexerConfigTrace,
  runIndexerConfigRetryConfig,
  runIndexerConfigSecurityParam,
  runIndexerConfigNetworkId,
  runIndexerConfigChainPoint,
  runIndexerConfigSocketPath,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent, blockInMode, blockTime, epochNo))
import Cardano.BM.Data.Trace (Trace)
import Control.Lens.TH qualified as Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Set.NonEmpty (NESet)
import Data.Word (Word64)
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Prettyprinter (Doc)

-- | Config type for node retries
data RetryConfig = RetryConfig
  { baseTimeBeforeNextRetry :: !Word64
  -- ^ Initial time before next retry (in seconds)
  , maybeMaxWaitTime :: !(Maybe Word64)
  -- ^ Max time before stopping retries (in seconds)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Alias for a 'Doc' tracer, it is the 'Trace' used throughout the Marconi application
type MarconiTrace m ann = Trace m (Doc ann)

-- | Type represents non empty set of Bech32 Shelley compatible addresses
type TargetAddresses = NESet (C.Address C.ShelleyAddr)

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

newtype ShouldFailIfResync = ShouldFailIfResync Bool
  deriving newtype
    ( Eq
    , Ord
    , Show
    , Aeson.FromJSON
    , Aeson.ToJSON
    )

data IndexingDepth = MinIndexingDepth !Word64 | MaxIndexingDepth
  deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

newtype SecurityParam = SecurityParam Word64
  deriving newtype (Eq, Ord, Bounded, Enum, Real, Num, Read, Integral, Show)

newtype TxIndexInBlock = TxIndexInBlock Word64
  deriving newtype
    ( Eq
    , Ord
    , Bounded
    , Enum
    , Real
    , Num
    , Read
    , Integral
    , Show
    , Aeson.FromJSON
    , Aeson.ToJSON
    , SQL.ToField
    , SQL.FromField
    )

-- | Common configuration required to run indexers
data RunIndexerConfig ann = RunIndexerConfig
  { _runIndexerConfigTrace :: MarconiTrace IO ann
  , _runIndexerConfigRetryConfig :: RetryConfig
  , _runIndexerConfigSecurityParam :: SecurityParam
  , _runIndexerConfigNetworkId :: C.NetworkId
  , _runIndexerConfigChainPoint :: C.ChainPoint
  , _runIndexerConfigSocketPath :: FilePath
  }

Lens.makeLenses ''RunIndexerConfig

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
