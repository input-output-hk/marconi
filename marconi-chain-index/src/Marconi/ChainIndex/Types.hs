{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | This module provides several type aliases and utility functions to deal with them.
module Marconi.ChainIndex.Types
       (
         Interval(..)
       , interval
       , lowerBound
       , upperBound
       , isInInterval
       -- * Addresses alias used to query marconi
       -- * Aliases for the current Cardano era
       , TargetAddresses
       , CurrentEra
       , pattern AsCurrentEra
       , pattern CurrentEra
       , TxOut
       -- * Aliases to ease concept mapping between plutus types and cardano types
       , TxOutRef
       , txOutRef
       -- * Database file names
       , utxoDbName
       , addressDatumDbName
       , datumDbName
       , scriptTxDbName
       , epochStateDbName
       , mintBurnDbName
       , SecurityParam(SecurityParam)
       , IndexingDepth(MinIndexingDepth, MaxIndexingDepth)
       , TxIndexInBlock
       ) where

import Cardano.Api qualified as C
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (pack)
import Data.Word (Word64)
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import Marconi.ChainIndex.Error (IndexerError (InvalidQueryInterval))

-- | Typre represents non empty list of Bech32 Shelley compatable addresses
type TargetAddresses = NonEmpty (C.Address C.ShelleyAddr)

-- | An alias for the current era, to ease the transition from one era to the next one
type CurrentEra = C.BabbageEra

pattern CurrentEra :: C.CardanoEra CurrentEra
pattern CurrentEra = C.BabbageEra

pattern AsCurrentEra :: C.AsType CurrentEra
pattern AsCurrentEra = C.AsBabbageEra

-- | A Cardano TxOut of the current Era
type TxOut = C.TxOut C.CtxTx CurrentEra

-- | A reference to a transaction output. This is a
-- pair of a transaction reference, and an index indicating which of the outputs
-- of that transaction we are referring to.
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

datumDbName :: FilePath
datumDbName = "datum.db"

scriptTxDbName :: FilePath
scriptTxDbName = "scripttx.db"

epochStateDbName :: FilePath
epochStateDbName = "epochstate.db"

mintBurnDbName :: FilePath
mintBurnDbName = "mintburn.db"

-- | Not comprehensive, only supports ChainPoint interval as outlines in <https://github.com/input-output-hk/marconi/blob/main/marconi-sidechain/doc/API.adoc#getutxosfromaddress>
data Interval r
  = LessThanOrEqual !r
  | InRange !r !r
  deriving (Eq, Show)

lowerBound :: Interval r -> Maybe r
lowerBound = \case
    LessThanOrEqual _ -> Nothing
    InRange x _       -> Just x

upperBound :: Interval r -> Maybe r
upperBound = \case
    LessThanOrEqual x -> Just x
    InRange _ x       -> Just x

-- | Smart constructor for 'Interval ', return an error if the lower bound is greater than the upper bound
interval
  :: (Ord r, Show r)
  => Maybe r -- ^ lower bound
  -> r  -- ^ upper bound
  -> Either IndexerError (Interval r)
interval Nothing p = Right $ LessThanOrEqual p
interval (Just p) p' =
  let
--  Enforce the internal invariant
-- 'InRange'.
    wrap
      :: (Ord r, Show r)
      => (r -> r -> Interval r)
      -> r -> r -> Either IndexerError (Interval r)
    wrap f x y
      | x <= y = Right $ f x y
      | otherwise = Left . InvalidQueryInterval . pack
          $ "Invalid Interval. LowerBound, "
          <> show x
          <> " is not less than or equal to upperBound "
          <> show y

  in wrap InRange p p'

-- | Check if a given chainpoint is in the given interval
isInInterval :: Interval C.SlotNo -> C.ChainPoint -> Bool
isInInterval slotNoInterval = \case
  C.ChainPointAtGenesis -> case slotNoInterval of
    LessThanOrEqual _ -> True
    InRange _ _       -> False

  C.ChainPoint slotNo _  -> case slotNoInterval of
    LessThanOrEqual slotNo' -> slotNo' >= slotNo
    InRange l h             -> l <= slotNo && h >= slotNo
