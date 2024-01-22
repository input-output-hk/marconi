{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides several type aliases and utility functions to deal with them.
module Marconi.Cardano.Core.Types (
  -- * Config for retrying
  RetryConfig (..),

  -- * Application tracer
  MarconiTrace,

  -- * Addresses alias used to query marconi
  TargetAddresses,

  -- * Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  UtxoIndexerConfig (..),

  -- * A type representing either a @ChainTip@ or a @Block@, with an attached distance to the tip
  TipAndBlock (..),

  -- * Aliases for the current Cardano era
  TxOut,

  -- * Aliases to ease concept mapping between plutus types and cardano types
  TxOutRef,
  txOutRef,
  AnyTxBody (AnyTxBody),

  -- * Reexport from cardano-api-extended
  BlockEvent (..),
  SecurityParam (SecurityParam),
  TxIndexInBlock (TxIndexInBlock),
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent, blockInMode, blockTime, epochNo))
import Cardano.BM.Data.Trace (Trace)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Set.NonEmpty (NESet)
import Data.Word (Word64)
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Core qualified as Core
import Ouroboros.Consensus.Config (SecurityParam (SecurityParam))
import Prettyprinter (Doc)

-- Point type instances for types of this module
-- or ones from Cardano.Api used throughout.
type instance Core.Point BlockEvent = C.ChainPoint
type instance Core.Point C.ChainTip = C.ChainPoint
type instance Core.Point [AnyTxBody] = C.ChainPoint

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
type MarconiTrace m = Trace m (Doc ())

-- | Type represents non empty set of Bech32 Shelley compatible addresses
type TargetAddresses = NESet (C.Address C.ShelleyAddr)

data UtxoIndexerConfig = UtxoIndexerConfig
  { ucTargetAddresses :: Maybe TargetAddresses
  -- ^ List of address utxo indexer to follow
  , ucEnableUtxoTxOutRef :: Bool
  -- ^ enable utxo indexer to store txOut refScript
  }

-- | A type representing a @ChainTip@ and maybe a @Block@, with an attached distance to the tip
data TipAndBlock = TipAndBlock C.ChainTip (Maybe (WithDistance BlockEvent))

type instance Core.Point TipAndBlock = C.ChainPoint

-- | A Cardano TxOut of the current Era
type TxOut = C.TxOut C.CtxTx C.ConwayEra

{- | A reference to a transaction output. This is a
 pair of a transaction reference, and an index indicating which of the outputs
 of that transaction we are referring to.
-}
type TxOutRef = C.TxIn

txOutRef :: C.TxId -> C.TxIx -> C.TxIn
txOutRef = C.TxIn

deriving newtype instance Ord SecurityParam
deriving newtype instance Enum SecurityParam
deriving newtype instance Real SecurityParam
deriving newtype instance Num SecurityParam
deriving newtype instance Integral SecurityParam
deriving newtype instance SQL.ToField SecurityParam
deriving newtype instance SQL.FromField SecurityParam

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

-- | An existential type representing a transaction with @C.'TxBody' era@ for any Cardano era.
data AnyTxBody = forall era. (C.IsCardanoEra era) => AnyTxBody C.BlockNo TxIndexInBlock (C.TxBody era)
