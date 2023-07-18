{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- + This module will create the SQL tables:
--
-- + table: unspent_transactions
--
-- @
--      |---------+------+------+-----------+-------+--------------+------------------+----------------+--------+-----------------+---------+----------------+---------|
--      | Address | TxId | TxIx | DatumHash | Value | InlineScript | InlineScriptHash | TxIndexInBlock | SlotNo | BlockHeaderHash | BlockNo | BlockTimestamp | EpochNo
--      |---------+------+------+-----------+-------+--------------+------------------+----------------+--------+-----------------+---------+----------------+---------|
-- @
--
-- + table: spent
--
-- @
--      |------+------+--------+-----------------+---------+----------------+---------+-----------|
--      | TxId | TxIx | SlotNo | blockHeaderHash | BlockNo | BlockTimestamp | EpochNo | SpentTxId |
--      |------+------+--------+-----------------+---------+----------------+---------+-----------|
-- @
--
-- + table: datumhash_datum
--
-- @
--      |------------+-------|
--      | datum_hash | datum |
--      |------------+-------|
-- @
-- To create these tables, we extract all transactions outputs from each transactions fetched with
-- the chain-sync protocol of the local node.

-- \| Module for indexing the Utxos in the Cardano blockchain

-- | Module for indexing the Utxos in the Cardano blockchain
module Marconi.ChainIndex.Indexers.Utxo where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Concurrent.Async (concurrently_)
import Control.Exception (Exception, bracket_)
import Control.Lens.Combinators (
  Lens',
  Traversal',
  imap,
  preview,
  view,
  _Just,
 )
import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (guard, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Object),
  object,
  (.:),
  (.:?),
  (.=),
 )
import Data.Either (fromRight, rights)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import Database.SQLite.Simple (
  NamedParam ((:=)),
  ResultError (UnexpectedNull),
 )
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (returnError)
import Database.SQLite.Simple.FromRow (
  FromRow (fromRow),
  field,
  fieldWith,
 )
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (toField), toField)
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Marconi.ChainIndex.Error (
  IndexerError (
    CantInsertEvent,
    CantQueryIndexer,
    CantRollback,
    CantStartIndexer,
    QueryError
  ),
  liftSQLError,
 )
import Marconi.ChainIndex.Extract.Datum qualified as Datum
import Marconi.ChainIndex.Indexers.LastSync (
  addLastSyncPoints,
  createLastSyncTable,
  queryLastSyncPoint,
  rollbackLastSyncPoints,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (
  TargetAddresses,
  TxIndexInBlock,
  TxOut,
  UtxoIndexerConfig (UtxoIndexerConfig),
  pattern CurrentEra,
 )
import Marconi.ChainIndex.Utils (
  addressesToPredicate,
 )
import Marconi.Core.Storable (
  Buffered (getStoredEvents, persistToStorage),
  HasPoint,
  Queryable (queryStorage),
  Resumable (resumeFromStorage),
  Rewindable (rewindStorage),
  StorableEvent,
  StorableMonad,
  StorablePoint,
  StorableQuery,
  StorableResult,
  emptyState,
 )
import Marconi.Core.Storable qualified as Storable
import System.Random.MWC (
  createSystemRandom,
  uniformR,
 )
import Text.Read (readMaybe)

{- Note [Last synced block]
 -
 - The 'LastSyncedBlockInfoQuery' query doesn't return the last indexed block info, but the one before.
 - The reason is that we want to use this query to find a sync point that is common to all the indexers
 - that are under the same coordinator.
 - Unfortunately, while the coordinator ensures that all the indexer move at the same speed, it can't
 - monitor if the last submitted block was indexed by all the indexers or not.
 -
 - As a consequence, if the last block info of the utxo indexer can, at most, be ahead of one block compared to other
 - indexers. Taking the block info before ensures that we have consistent information across all the indexers.
 -}

-- | Not comprehensive, only supports ChainPoint interval as outlines in <https://github.com/input-output-hk/marconi/blob/main/marconi-sidechain/doc/API.adoc#getutxosfromaddress>
data Interval r
  = LessThanOrEqual !r
  | InRange !r !r
  deriving (Eq, Show)

lowerBound :: Interval r -> Maybe r
lowerBound = \case
  LessThanOrEqual _ -> Nothing
  InRange x _ -> Just x

upperBound :: Interval r -> Maybe r
upperBound = \case
  LessThanOrEqual x -> Just x
  InRange _ x -> Just x

-- | Smart constructor for 'Interval ', return an error if the lower bound is greater than the upper bound.
interval
  :: Maybe C.SlotNo
  -- ^ lower bound
  -> C.SlotNo
  -- ^ upper bound
  -> Either (IndexerError UtxoIndexerError) (Interval C.SlotNo)
interval Nothing p = Right $ LessThanOrEqual p
interval (Just p) p' =
  let
    --  Enforce the internal invariant
    -- 'InRange'.
    wrap f x y
      | x <= y = Right $ f x y
      | otherwise =
          Left . QueryError $ InvalidInterval x y
   in
    wrap InRange p p'

-- | Check if a given chainpoint is in the given interval
isInInterval :: Interval C.SlotNo -> C.SlotNo -> Bool
isInInterval slotNoInterval slotNo =
  case slotNoInterval of
    LessThanOrEqual slotNo' -> slotNo' >= slotNo
    InRange l h -> l <= slotNo && h >= slotNo

type UtxoIndexer = Storable.State UtxoHandle

data UtxoHandle = UtxoHandle
  { hdlConnection :: !SQL.Connection
  -- ^ SQLite connection
  , hdlDepth :: !Int
  -- ^ depth before flushing to disk storage
  , toVacuume :: !Bool
  -- ^ weather to perform SQLite vacuum to release space
  }

data QueryUtxoByAddress = QueryUtxoByAddress !C.AddressAny !(Interval C.SlotNo)
  deriving (Show, Eq)

data instance StorableQuery UtxoHandle
  = QueryUtxoByAddressWrapper QueryUtxoByAddress
  | LastSyncedBlockInfoQuery
  deriving (Show, Eq)

data UtxoIndexerError = InvalidInterval C.SlotNo C.SlotNo
  deriving (Show, Eq)

instance Exception UtxoIndexerError

type QueryableAddresses = NonEmpty (StorableQuery UtxoHandle)

type instance StorableMonad UtxoHandle = ExceptT (IndexerError UtxoIndexerError) IO

type instance StorablePoint UtxoHandle = C.ChainPoint

newtype Depth = Depth Int
  deriving newtype (Eq, Ord, Num, Show)

data Utxo = Utxo
  { _address :: !C.AddressAny
  , _txIn :: !C.TxIn
  , _datumHash :: !(Maybe (C.Hash C.ScriptData))
  , _value :: !C.Value
  , _inlineScript :: !(Maybe C.ScriptInAnyLang)
  , _inlineScriptHash :: !(Maybe C.ScriptHash)
  , _txIndexInBlock :: !TxIndexInBlock
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''Utxo)

instance Ord Utxo where
  compare u1 u2 = compare (u1 ^. txIn) (u2 ^. txIn)

instance FromJSON Utxo where
  parseJSON (Object v) =
    Utxo
      <$> v .: "address"
      <*> (C.TxIn <$> v .: "txId" <*> v .: "txIx")
      <*> v .: "datumHash"
      <*> v .: "value"
      <*> v .: "inlineScript"
      <*> v .: "inlineScriptHash"
      <*> v .: "txIndexInBlock"
  parseJSON _ = mempty

instance ToJSON Utxo where
  toJSON u =
    let C.TxIn txid txix = u ^. txIn
     in object
          [ "address" .= (u ^. address)
          , "txId" .= txid
          , "txIx" .= txix
          , "datumHash" .= (u ^. datumHash)
          , "value" .= (u ^. value)
          , -- Uses ToJSON instance of cardano-api which serialises using the 'C.HasTextEnvelope' typeclass.
            "inlineScript" .= (u ^. inlineScript)
          , "inlineScriptHash" .= (u ^. inlineScriptHash)
          , "txIndexInBlock" .= (u ^. txIndexInBlock)
          ]

data BlockInfo = BlockInfo
  { _blockInfoSlotNo :: !C.SlotNo
  , _blockInfoBlockHeaderHash :: !(C.Hash C.BlockHeader)
  , _blockInfoBlockNo :: !C.BlockNo
  , _blockInfoTimestamp :: !Word64
  , _blockInfoEpochNo :: !C.EpochNo
  }
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (FromRow, ToJSON, FromJSON)

instance FromRow (Maybe BlockInfo) where
  fromRow = do
    maybeSlotNo <- field
    maybeBhh <- field
    maybeBlockNo <- field
    maybeTimestamp <- field
    maybeEpochNo <- field
    if
      | Just slotNo <- maybeSlotNo
      , Just bhh <- maybeBhh
      , Just blockNo <- maybeBlockNo
      , Just timestamp <- maybeTimestamp
      , Just epochNo <- maybeEpochNo ->
          pure $ Just $ BlockInfo slotNo bhh blockNo timestamp epochNo
      | otherwise -> pure Nothing

$(makeLenses ''BlockInfo)

blockInfoToChainPointRow :: BlockInfo -> ChainPointRow
blockInfoToChainPointRow BlockInfo{_blockInfoSlotNo, _blockInfoBlockHeaderHash} =
  ChainPointRow _blockInfoSlotNo _blockInfoBlockHeaderHash

data ChainPointRow = ChainPointRow {_cpSlotNo :: C.SlotNo, _cpBlockHeaderHash :: C.Hash C.BlockHeader}
  deriving (Show, Eq, Ord, Generic)

$(makeLenses ''ChainPointRow)

toChainPointRow :: C.ChainPoint -> Maybe ChainPointRow
toChainPointRow cp = ChainPointRow <$> C.chainPointToSlotNo cp <*> C.chainPointToHeaderHash cp

instance FromJSON ChainPointRow where
  parseJSON (Object v) =
    ChainPointRow
      <$> v .: "slotNo"
      <*> v .: "blockHeaderHash"
  parseJSON _ = mempty

instance ToJSON ChainPointRow where
  toJSON c =
    object
      [ "slotNo" .= view cpSlotNo c
      , "blockHeaderHash" .= view cpBlockHeaderHash c
      ]

getChainPoint :: ChainPointRow -> C.ChainPoint
getChainPoint cp = C.ChainPoint (cp ^. cpSlotNo) (cp ^. cpBlockHeaderHash)

data SpentInfo = SpentInfo
  { _siSpentBlockInfo :: BlockInfo
  , _siSpentTxId :: C.TxId
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(makeLenses ''SpentInfo)

data UtxoRow = UtxoRow
  { _urUtxo :: !Utxo
  , _urBlockInfo :: !BlockInfo
  , _urSpentInfo :: !(Maybe SpentInfo)
  }
  deriving (Show, Eq, Ord, Generic)

$(makeLenses ''UtxoRow)

data UtxoResult = UtxoResult
  { utxoResultAddress :: !C.AddressAny
  , utxoResultTxIn :: !C.TxIn
  , utxoResultDatum :: !(Maybe C.ScriptData) -- datumhash_datum
  , utxoResultDatumHash :: !(Maybe (C.Hash C.ScriptData))
  , utxoResultValue :: !C.Value
  , utxoResultInlineScript :: !(Maybe C.ScriptInAnyLang)
  , utxoResultInlineScriptHash :: !(Maybe C.ScriptHash)
  , utxoResultTxIndexInBlock :: !TxIndexInBlock
  , utxoResultBlockInfo :: !BlockInfo
  , utxoResultSpentInfo :: !(Maybe SpentInfo)
  , utxoResultTxIns :: [C.TxIn]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Ord UtxoResult where
  compare u u' = compare (utxoResultAddress u, utxoResultTxIn u) (utxoResultAddress u', utxoResultTxIn u')

urCreationSlotNo :: Lens' UtxoRow C.SlotNo
urCreationSlotNo = urBlockInfo . blockInfoSlotNo

urCreationBlockHeaderHash :: Lens' UtxoRow (C.Hash C.BlockHeader)
urCreationBlockHeaderHash = urBlockInfo . blockInfoBlockHeaderHash

urCreationBlockNo :: Lens' UtxoRow C.BlockNo
urCreationBlockNo = urBlockInfo . blockInfoBlockNo

urCreationBlockTimestamp :: Lens' UtxoRow Word64
urCreationBlockTimestamp = urBlockInfo . blockInfoTimestamp

urCreationEpochNo :: Lens' UtxoRow C.EpochNo
urCreationEpochNo = urBlockInfo . blockInfoEpochNo

urSpentTxId :: Traversal' UtxoRow C.TxId
urSpentTxId = urSpentInfo . _Just . siSpentTxId

urSpentSlotNo :: Traversal' UtxoRow C.SlotNo
urSpentSlotNo = urSpentInfo . _Just . siSpentBlockInfo . blockInfoSlotNo

urSpentBlockHeaderHash :: Traversal' UtxoRow (C.Hash C.BlockHeader)
urSpentBlockHeaderHash = urSpentInfo . _Just . siSpentBlockInfo . blockInfoBlockHeaderHash

urSpentBlockNo :: Traversal' UtxoRow C.BlockNo
urSpentBlockNo = urSpentInfo . _Just . siSpentBlockInfo . blockInfoBlockNo

urSpentBlockTimestamp :: Traversal' UtxoRow Word64
urSpentBlockTimestamp = urSpentInfo . _Just . siSpentBlockInfo . blockInfoTimestamp

urSpentEpochNo :: Traversal' UtxoRow C.EpochNo
urSpentEpochNo = urSpentInfo . _Just . siSpentBlockInfo . blockInfoEpochNo

instance FromJSON UtxoRow where
  parseJSON (Object v) =
    let parseSpentInfo = do
          s <- v .:? "spentSlotNo"
          bh <- v .:? "spentBlockHeaderHash"
          bn <- v .:? "spentBlockNo"
          bt <- v .:? "spentBlockTimestamp"
          e <- v .:? "spentEpochNo"
          tId <- v .:? "spentTxId"
          pure $ case (s, bh, bn, bt, e, tId) of
            (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) -> Nothing
            (Just s', Just bh', Just bn', Just bt', Just e', Just txId') ->
              Just $ SpentInfo (BlockInfo s' bh' bn' bt' e') txId'
            _error -> fail "Inconsistent spent info"
     in UtxoRow
          <$> v .: "utxo"
          <*> ( BlockInfo
                  <$> v .: "slotNo"
                  <*> v .: "blockHeaderHash"
                  <*> v .: "blockNo"
                  <*> v .: "blockTimestamp"
                  <*> v .: "epochNo"
              )
          <*> parseSpentInfo
  parseJSON _ = mempty

instance ToJSON UtxoRow where
  toJSON ur =
    object
      [ "utxo" .= view urUtxo ur
      , "slotNo" .= view urCreationSlotNo ur
      , "blockHeaderHash" .= view urCreationBlockHeaderHash ur
      , "blockNo" .= view urCreationBlockNo ur
      , "blockTimestamp" .= view urCreationBlockTimestamp ur
      , "epochNo" .= view urCreationEpochNo ur
      , "spentSlotNo" .= preview urSpentSlotNo ur
      , "spentBlockHeaderHash" .= preview urSpentBlockHeaderHash ur
      , "spentTxId" .= preview urSpentTxId ur
      ]

data instance StorableResult UtxoHandle
  = -- | Result of a 'QueryUtxoByAddress' query
    UtxoByAddressResult {getUtxoByAddressResult :: ![UtxoResult]}
  | -- | Result of a 'LastSyncedBlockInfoQuery'
    LastSyncedBlockInfoResult {getLastSyncedBlockInfo :: !(WithOrigin BlockInfo)}
  deriving (Eq, Show, Ord)

data instance StorableEvent UtxoHandle = UtxoEvent
  { ueUtxos :: ![Utxo]
  , ueInputs :: !(Map C.TxIn C.TxId)
  , ueBlockInfo :: !BlockInfo
  , ueDatum :: !(Map (C.Hash C.ScriptData) C.ScriptData)
  }
  deriving (Eq, Ord, Show, Generic)

-- | The effect of a transaction (or a number of them) on the tx output map.
data TxOutBalance = TxOutBalance
  { _tbUnspent :: !(Map C.TxIn Utxo)
  -- ^ Outputs newly added by the transaction(s)
  , _tbSpent :: !(Map C.TxIn C.TxId)
  -- ^ Outputs spent by the transaction(s)
  }
  deriving stock (Eq, Show, Generic)

makeLenses ''TxOutBalance

instance Semigroup TxOutBalance where
  bUtxoL <> bUtxoR =
    TxOutBalance
      { _tbUnspent = bUtxoL ^. tbUnspent <> bUtxoR ^. tbUnspent
      , _tbSpent = bUtxoL ^. tbSpent <> bUtxoR ^. tbSpent
      }

instance Monoid TxOutBalance where
  mappend = (<>)
  mempty = TxOutBalance mempty mempty

data Spent = Spent
  { _sTxIn :: !C.TxIn
  , _sSpentInfo :: !SpentInfo
  }
  deriving (Show, Eq)

makeLenses ''Spent

instance Ord Spent where
  compare s s' = compare (s ^. sTxIn) (s' ^. sTxIn)

instance HasPoint (StorableEvent UtxoHandle) C.ChainPoint where
  getPoint (UtxoEvent{ueBlockInfo}) =
    C.ChainPoint
      (_blockInfoSlotNo ueBlockInfo)
      (_blockInfoBlockHeaderHash ueBlockInfo)

------------------
-- sql mappings --
------------------

instance ToRow ChainPointRow where
  toRow c =
    toRow
      [ Database.SQLite.Simple.ToField.toField $ c ^. cpSlotNo
      , Database.SQLite.Simple.ToField.toField $ c ^. cpBlockHeaderHash
      ]

instance FromRow ChainPointRow where
  fromRow = ChainPointRow <$> field <*> field

instance ToRow UtxoRow where
  toRow u =
    let C.TxIn txId txIx = u ^. urUtxo . txIn
     in toRow
          [ toField (u ^. urUtxo . address)
          , toField txId
          , toField txIx
          , toField $ u ^. urUtxo . datumHash
          , toField $ u ^. urUtxo . value
          , toField $ u ^. urUtxo . inlineScript
          , toField $ u ^. urUtxo . inlineScriptHash
          , toField $ u ^. urUtxo . txIndexInBlock
          , toField $ u ^. urCreationSlotNo
          , toField $ u ^. urCreationBlockHeaderHash
          , toField $ u ^. urCreationBlockNo
          , toField $ u ^. urCreationBlockTimestamp
          , toField $ u ^. urCreationEpochNo
          ]

instance FromRow UtxoRow where
  fromRow =
    let parseSpentInfo (SpentInfoRow Nothing Nothing Nothing Nothing Nothing Nothing) = pure Nothing
        parseSpentInfo (SpentInfoRow (Just sn) (Just bhh) (Just bn) (Just bt) (Just e) (Just tid)) =
          pure $ Just $ SpentInfo (BlockInfo sn bhh bn bt e) tid
        parseSpentInfo _ =
          fieldWith $ \field' ->
            returnError
              UnexpectedNull
              field'
              "Invalid spent values: Some fields are null, other aren't"
     in do
          utxo <- fromRow
          blockInfo <- fromRow
          spentInfoRow <- fromRow
          spentInfo <- parseSpentInfo spentInfoRow
          pure $ UtxoRow utxo blockInfo spentInfo

-- | Used internally to parse SpentInfo
data SpentInfoRow
  = SpentInfoRow
      !(Maybe C.SlotNo)
      !(Maybe (C.Hash C.BlockHeader))
      !(Maybe C.BlockNo)
      !(Maybe Word64)
      !(Maybe C.EpochNo)
      !(Maybe C.TxId)

instance FromRow SpentInfoRow where
  fromRow = SpentInfoRow <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Utxo where
  fromRow =
    Utxo
      <$> field
      <*> fromRow
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance FromRow SpentInfo where
  fromRow = SpentInfo <$> fromRow <*> field

instance FromRow Spent where
  fromRow = Spent <$> fromRow <*> fromRow

instance ToRow Spent where
  toRow s =
    let C.TxIn txid txix = s ^. sTxIn
     in toRow
          [ toField txid
          , toField txix
          , toField $ s ^. sSpentInfo . siSpentBlockInfo . blockInfoSlotNo
          , toField $ s ^. sSpentInfo . siSpentBlockInfo . blockInfoBlockHeaderHash
          , toField $ s ^. sSpentInfo . siSpentBlockInfo . blockInfoBlockNo
          , toField $ s ^. sSpentInfo . siSpentBlockInfo . blockInfoTimestamp
          , toField $ s ^. sSpentInfo . siSpentBlockInfo . blockInfoEpochNo
          , toField $ s ^. sSpentInfo . siSpentTxId
          ]

instance FromRow UtxoResult where
  fromRow =
    let decodeTxId =
          either (const Nothing) pure . C.deserialiseFromRawBytesHex C.AsTxId . Text.encodeUtf8
        txIdsFromField = do
          concatenatedTxIds <- field
          case concatenatedTxIds of
            Nothing -> pure []
            Just xs | xs == "" -> pure []
            Just xs ->
              case traverse decodeTxId $ Text.splitOn "," xs of
                Nothing -> fieldWith $ \field' ->
                  returnError SQL.ConversionFailed field' ("Can't decode the spent txIds sequence: " <> show xs)
                Just xs' -> pure xs'
        decodeTxIx = fmap C.TxIx . readMaybe . Text.unpack
        txIxesFromField = do
          concatenatedTxIxs <- field
          case concatenatedTxIxs of
            Nothing -> pure []
            Just xs ->
              case traverse decodeTxIx $ Text.splitOn "," xs of
                Nothing -> fieldWith $ \field' ->
                  returnError SQL.ConversionFailed field' "Can't decode the spent txIxs sequence"
                Just xs' -> pure xs'
        txInsFromRow = do
          txIds <- txIdsFromField
          txIxes <- txIxesFromField
          pure $ zipWith C.TxIn txIds txIxes
     in do UtxoResult
          <$> field
          <*> fromRow
          <*> field
          <*> field
          <*> field
          <*> field
          <*> field
          <*> field
          <*> fromRow
          <*> do
            a <- fromRow
            b <- field
            pure $ do
              spentBlockInfo <- a
              spentTxId <- b
              pure $ SpentInfo spentBlockInfo spentTxId
          <*> txInsFromRow

data DatumRow = DatumRow
  { datumRowDatumHash :: C.Hash C.ScriptData
  , datumRowDatum :: C.ScriptData
  }
  deriving (Show, Generic)

instance SQL.ToRow DatumRow where
  toRow (DatumRow dh d) = [toField dh, toField d]

deriving anyclass instance SQL.FromRow DatumRow

{- | Open a connection to DB, and create resources
 The parameter ((k + 1) * 2) specifies the amount of events that are buffered.
 The larger the number, the more RAM the indexer uses. However, we get improved SQL
 queries due to batching more events together.
-}
open
  :: FilePath
  -- ^ SQLite file path
  -> Depth
  -- ^ The Depth parameter k, the larger K, the more RAM the indexer uses
  -> Bool
  -- ^ whether to perform vacuum
  -> StorableMonad UtxoHandle UtxoIndexer
open dbPath (Depth k) isToVacuume = do
  c <- liftSQLError CantStartIndexer (SQL.open dbPath)

  lift $ SQL.execute_ c "PRAGMA journal_mode=WAL"

  lift $
    SQL.execute_
      c
      [sql|CREATE TABLE IF NOT EXISTS unspent_transactions
                    ( address TEXT NOT NULL
                    , txId TEXT NOT NULL
                    , txIx INT NOT NULL
                    , datumHash BLOB
                    , value BLOB
                    , inlineScript BLOB
                    , inlineScriptHash BLOB
                    , txIndexInBlock INT NOT NULL
                    , slotNo INT NOT NULL
                    , blockHeaderHash BLOB NOT NULL
                    , blockNo INT NOT NULL
                    , blockTimestamp INT NOT NULL
                    , epochNo INT NOT NULL
                    )|]

  lift $
    SQL.execute_
      c
      [sql|CREATE TABLE IF NOT EXISTS spent
                    ( txId TEXT NOT NULL
                    , txIx INT NOT NULL
                    , slotNo INT NOT NULL
                    , blockHeaderHash BLOB NOT NULL
                    , blockNo INT NOT NULL
                    , blockTimestamp INT NOT NULL
                    , epochNo INT NOT NULL
                    , spentTxId TEXT NOT NULL
                    )|]

  lift $
    SQL.execute_
      c
      [sql|CREATE TABLE IF NOT EXISTS datumhash_datum ( datum_hash BLOB PRIMARY KEY , datum BLOB)|]

  lift $ createLastSyncTable c

  lift $ SQL.execute_ c [sql|CREATE INDEX IF NOT EXISTS spent_slotNo ON spent (slotNo)|]

  lift $ SQL.execute_ c [sql|CREATE INDEX IF NOT EXISTS spent_txId ON spent (txId, txIx)|]

  lift $
    SQL.execute_
      c
      [sql|CREATE INDEX IF NOT EXISTS spent_tx_inputs ON spent (spentTxId)|]

  lift $
    SQL.execute_
      c
      [sql|CREATE INDEX IF NOT EXISTS unspent_transaction_address ON unspent_transactions (address)|]

  emptyState k (UtxoHandle c k isToVacuume)

getSpentFrom :: StorableEvent UtxoHandle -> [Spent]
getSpentFrom (UtxoEvent _ txIns bi _) = do
  (txin, spentTxId) <- Map.toList txIns
  pure $ Spent txin (SpentInfo bi spentTxId)

{- | Store UtxoEvents
 Events are stored in memory and flushed to SQL, disk, when memory buffer has reached capacity
-}
instance Buffered UtxoHandle where
  persistToStorage
    :: (Foldable f)
    => f (StorableEvent UtxoHandle) -- Events to store
    -> UtxoHandle -- Handler for storing events
    -> StorableMonad UtxoHandle UtxoHandle
  persistToStorage events h | null events = pure h
  persistToStorage events h@(UtxoHandle c _k toVacuume) =
    liftSQLError CantInsertEvent $ do
      let rows = concatMap eventToRows events
          spents = concatMap getSpentFrom events
          datumRows = fmap (uncurry DatumRow) $ Map.toList $ foldMap ueDatum events
          chainPoints :: [C.ChainPoint] =
            (C.ChainPoint <$> view blockInfoSlotNo <*> view blockInfoBlockHeaderHash)
              . ueBlockInfo
              <$> toList events
      bracket_
        (SQL.execute_ c "BEGIN")
        (SQL.execute_ c "COMMIT")
        ( SQL.executeMany
            c
            [sql|INSERT
             INTO unspent_transactions (
               address,
               txId,
               txIx,
               datumHash,
               value,
               inlineScript,
               inlineScriptHash,
               txIndexInBlock,
               slotNo,
               blockHeaderHash,
               blockNo,
               blockTimestamp,
               epochNo
            ) VALUES
            (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|]
            rows
            `concurrently_` SQL.executeMany
              c
              [sql|INSERT
            INTO spent (
              txId,
              txIx, slotNo, blockHeaderHash, blockNo, blockTimestamp, epochNo, spentTxId
            ) VALUES
            (?, ?, ?, ?, ?, ?, ?, ?)|]
              spents
            `concurrently_` SQL.executeMany
              c
              [sql|INSERT OR IGNORE INTO datumhash_datum
                   ( datum_hash
                   , datum
                   )
                   VALUES (?, ?)|]
              datumRows
            `concurrently_` addLastSyncPoints c chainPoints
        )
      -- We want to perform vacuum about once every 100
      when toVacuume $ do
        rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
        when (rndCheck == 42) $ do
          SQL.execute_
            c
            [sql|DELETE FROM
                            unspent_transactions
                          WHERE
                            unspent_transactions.rowid IN (
                              SELECT
                                unspent_transactions.rowid
                              FROM
                                unspent_transactions
                                JOIN spent ON unspent_transactions.txId = spent.txId
                                AND unspent_transactions.txIx = spent.txIx
                            )|]
          -- remove Spent and release space, see https://www.sqlite.org/lang_vacuum.html
          SQL.execute_ c "VACUUM"
      pure h

  getStoredEvents :: UtxoHandle -> StorableMonad UtxoHandle [StorableEvent UtxoHandle]
  getStoredEvents (UtxoHandle{}) = error "!!! This Buffered class method will be removed"

sqliteUtxoByAddressQuery
  :: (FromRow r) => SQL.Connection -> ([SQL.Query], [NamedParam]) -> Maybe SQL.Query -> IO [r]
sqliteUtxoByAddressQuery c (filters, params) order = SQL.queryNamed c query params
  where
    wherePart =
      if null filters
        then ""
        else "WHERE " <> SQL.Query (Text.intercalate " AND " $ SQL.fromQuery <$> filters)
    orderPart = maybe "" (" " <>) order
    query =
      [sql|SELECT
              u.address,
              u.txId,
              u.txIx,
              d.datum,
              u.datumHash,
              u.value,
              u.inlineScript,
              u.inlineScriptHash,
              u.txIndexInBlock,

              u.slotNo,
              u.blockHeaderHash,
              u.blockNo,
              u.blockTimestamp,
              u.epochNo,

              s.slotNo,
              s.blockHeaderHash,
              s.blockNo,
              s.blockTimestamp,
              s.epochNo,
              s.spentTxId,

              GROUP_CONCAT(HEX(s2.txId)),
              GROUP_CONCAT(s2.txIx)

      FROM unspent_transactions u
      LEFT JOIN spent s           ON u.txId = s.txId AND u.txIx = s.txIx
      LEFT JOIN spent s2          ON u.txId = s2.spentTxId
      LEFT JOIN datumhash_datum d ON u.datumHash = d.datum_hash
           |]
        <> wherePart
        <> " GROUP BY u.txId, u.txIx"
        <> orderPart

{- | Retrieve TxIns at a slotNo
 This function is used to reconstruct the original UtxoEvent
-}
getTxIns :: SQL.Connection -> C.SlotNo -> IO (Map C.TxIn C.TxId)
getTxIns c sn = do
  xs <-
    SQL.query
      c
      "SELECT txId, txIx, spentTxId FROM spent WHERE slotNo =?"
      (SQL.Only (sn :: C.SlotNo))
  pure $ Map.fromList $ do
    (txid, txix, spentTx) <- xs
    pure (C.TxIn txix txid, spentTx)

{- | convert utxoEvent to utxoRow
 Note: No `unspent` computation is performed
-}
eventToRows :: StorableEvent UtxoHandle -> [UtxoRow]
eventToRows (UtxoEvent utxos _ bi _) =
  let eventToRow u =
        UtxoRow
          { _urUtxo = u
          , _urBlockInfo = bi
          , _urSpentInfo = Nothing
          }
   in fmap eventToRow utxos

{- | Used internally to gather the information required
 to update the in-database result
 from the UtoxByAddress query
 with the in-memory events
-}
data UtxoByAddressBufferEvents = UtxoByAddressBufferEvents
  { _bufferUtxos :: ![StorableEvent UtxoHandle]
  -- ^ Utxos at the requested address
  , _bufferSpent :: !(Map C.TxIn SpentInfo)
  -- ^ All the spent TxIn stored in memory that occured before the query upper bound
  , _bufferFutureSpent :: !(Map C.TxIn SpentInfo)
  -- ^ All the spent TxIn stored in memory that occured after the query upper bound
  }
  deriving (Eq, Show)

makeLenses ''UtxoByAddressBufferEvents

instance Semigroup UtxoByAddressBufferEvents where
  u1 <> u2 =
    UtxoByAddressBufferEvents
      (u1 ^. bufferUtxos <> u2 ^. bufferUtxos)
      (u1 ^. bufferSpent <> u2 ^. bufferSpent)
      (u1 ^. bufferFutureSpent <> u2 ^. bufferFutureSpent)

instance Monoid UtxoByAddressBufferEvents where
  mempty = UtxoByAddressBufferEvents mempty mempty mempty

-- | Filter in-memory events at the given address and interval
eventsAtAddress
  :: (Foldable f)
  => C.AddressAny
  -> Interval C.SlotNo
  -> f (StorableEvent UtxoHandle)
  -- ^ Utxo event
  -> UtxoByAddressBufferEvents
eventsAtAddress addr snoInterval = foldMap go
  where
    pointFilter :: StorableEvent UtxoHandle -> Bool
    pointFilter = isInInterval snoInterval . _blockInfoSlotNo . ueBlockInfo

    afterBoundCheck :: C.SlotNo -> Bool
    afterBoundCheck slotNo = case upperBound snoInterval of
      Nothing -> False
      Just s -> slotNo > s

    afterUpperBound :: StorableEvent UtxoHandle -> Bool
    afterUpperBound = afterBoundCheck . _blockInfoSlotNo . ueBlockInfo

    utxosAtAddress :: StorableEvent UtxoHandle -> [Utxo]
    utxosAtAddress = filter ((addr ==) . _address) . ueUtxos

    splitEventAtAddress :: StorableEvent UtxoHandle -> [StorableEvent UtxoHandle]
    splitEventAtAddress event =
      [ event{ueUtxos = utxosAtAddress event}
      | not (null $ utxosAtAddress event)
          && pointFilter event
      ]

    generateSpentInfo :: StorableEvent UtxoHandle -> C.TxId -> SpentInfo
    generateSpentInfo event = SpentInfo (ueBlockInfo event)

    getBufferSpent :: StorableEvent UtxoHandle -> Map C.TxIn SpentInfo
    getBufferSpent event =
      if afterUpperBound event
        then mempty
        else fmap (generateSpentInfo event) $ ueInputs event

    getBufferFutureSpent :: StorableEvent UtxoHandle -> Map C.TxIn SpentInfo
    getBufferFutureSpent event =
      if afterUpperBound event
        then fmap (generateSpentInfo event) $ ueInputs event
        else mempty

    go :: StorableEvent UtxoHandle -> UtxoByAddressBufferEvents
    go event =
      UtxoByAddressBufferEvents
        (splitEventAtAddress event)
        (getBufferSpent event)
        (getBufferFutureSpent event)

{- | Query the data stored in the indexer
 Queries SQL + buffered data, where buffered data is the data that will be batched to SQL
-}
instance Queryable UtxoHandle where
  queryStorage
    :: (Foldable f)
    => f (StorableEvent UtxoHandle)
    -> UtxoHandle
    -> StorableQuery UtxoHandle
    -> StorableMonad UtxoHandle (StorableResult UtxoHandle)
  queryStorage memoryEvents (UtxoHandle c _ _) (QueryUtxoByAddressWrapper (QueryUtxoByAddress addr slotInterval)) =
    liftSQLError CantQueryIndexer $ do
      persistedUtxoResults <- sqliteUtxoByAddressQuery c filters $ Just "ORDER BY u.slotNo ASC"
      bufferedUtxoResults <- concat <$> mapM bufferEventUtxoResult bufferEvents
      pure $ UtxoByAddressResult $ mapMaybe filterAddSpent persistedUtxoResults <> bufferedUtxoResults
    where
      UtxoByAddressBufferEvents bufferEvents bufferSpent' bufferFutureSpent' =
        eventsAtAddress addr slotInterval memoryEvents

      filterAddSpent :: UtxoResult -> Maybe UtxoResult
      filterAddSpent utxoResult = case utxoResultSpentInfo utxoResult of
        -- if it's already spent, no need to check if it's spent in the buffer events
        Just _ -> Just utxoResult
        Nothing ->
          let findSelfIn = Map.lookup (utxoResultTxIn utxoResult)
           in if
                | Just _ <- findSelfIn bufferSpent' -> Nothing
                | Just spent <- findSelfIn bufferFutureSpent' -> Just $ utxoResult{utxoResultSpentInfo = Just spent}
                | otherwise -> Just utxoResult

      addressFilter = (["u.address = :address"], [":address" := addr])
      lowerBoundFilter = case lowerBound slotInterval of
        Nothing -> mempty
        Just lowerBound' -> (["u.slotNo >= :lowerBound"], [":lowerBound" := lowerBound'])
      upperBoundFilter = case upperBound slotInterval of
        Nothing -> mempty
        Just upperBound' ->
          (
            [ -- created before the upperBound
              "u.slotNo <= :upperBound"
            , -- unspent or spent after the upper bound
              "(s.slotNo IS NULL OR s.slotNo > :upperBound)"
            ]
          , [":upperBound" := upperBound']
          )
      filters = addressFilter <> lowerBoundFilter <> upperBoundFilter

      bufferEventUtxoResult :: StorableEvent UtxoHandle -> IO [UtxoResult]
      bufferEventUtxoResult (UtxoEvent utxos spents bi datumMap) =
        catMaybes <$> traverse updateSpent utxos
        where
          updateSpent :: Utxo -> IO (Maybe UtxoResult)
          updateSpent u =
            let findSelfIn = Map.lookup (u ^. txIn)
             in if
                  | Just _ <- findSelfIn bufferSpent' -> pure Nothing
                  | maybeSpentInfo@(Just _) <- findSelfIn bufferFutureSpent' -> Just <$> toUtxoResult u maybeSpentInfo
                  | otherwise -> Just <$> toUtxoResult u Nothing

          resolveTxIns :: Utxo -> Map C.TxIn C.TxId -> [C.TxIn]
          resolveTxIns u =
            let txid = (\(C.TxIn x _) -> x) $ u ^. txIn
             in Map.keys . Map.filter (txid ==)

          txIns :: [C.TxIn]
          txIns = case utxos of
            [] -> []
            u : _ -> resolveTxIns u spents

          toUtxoResult :: Utxo -> Maybe SpentInfo -> IO UtxoResult
          toUtxoResult u maybeSpentInfo = do
            let maybeDatumHash = _datumHash u
            maybeDatum <- case flip Map.lookup datumMap =<< maybeDatumHash of
              Just datum -> pure $ Just datum
              Nothing -> maybe (pure Nothing) (fmap (fmap datumRowDatum) . findDatum) maybeDatumHash
            pure $
              UtxoResult
                { utxoResultAddress = _address u
                , utxoResultTxIn = _txIn u
                , utxoResultDatum = maybeDatum
                , utxoResultDatumHash = maybeDatumHash
                , utxoResultValue = _value u
                , utxoResultInlineScript = _inlineScript u
                , utxoResultInlineScriptHash = _inlineScriptHash u
                , utxoResultTxIndexInBlock = _txIndexInBlock u
                , utxoResultBlockInfo = bi
                , utxoResultSpentInfo = maybeSpentInfo
                , utxoResultTxIns = txIns
                }
          findDatum :: C.Hash C.ScriptData -> IO (Maybe DatumRow)
          findDatum hash = do
            listToMaybe
              <$> SQL.query
                c
                "SELECT datum_hash, datum FROM datumhash_datum WHERE datum_hash = ?"
                (SQL.Only hash)
  queryStorage es (UtxoHandle c _ _) LastSyncedBlockInfoQuery =
    let queryLastSlot =
          [sql|SELECT u.slotNo, u.blockHeaderHash, u.blockNo, u.blockTimestamp, u.epochNo
             FROM unspent_transactions u
             GROUP BY u.slotNo
             ORDER BY u.slotNo DESC
             LIMIT ?|]
     in -- We don't send the last event but the one before, to ensure that every indexers reached this point
        -- It's a hack, which should be removed once we have a proper handling of synchronization events.
        --
        -- See Note [Last synced block]
        case toList es of
          -- 2+ elements in memory
          (_ : _ : _) -> pure $
            LastSyncedBlockInfoResult $
              case sortOn (Down . _blockInfoSlotNo . ueBlockInfo) $ toList es of
                _ : p : _xs -> At $ ueBlockInfo p
                _other -> Origin
          -- 1 element in memory
          [_] -> liftSQLError CantQueryIndexer $ do
            persisted <- SQL.query c queryLastSlot (SQL.Only (1 :: Word64))
            pure $
              LastSyncedBlockInfoResult $
                case persisted of
                  bi : _ -> At bi
                  _other -> Origin
          -- 0 element in memory
          [] -> liftSQLError CantQueryIndexer $ do
            persisted <- SQL.query c queryLastSlot (SQL.Only (2 :: Word64))
            pure $
              LastSyncedBlockInfoResult $
                case persisted of
                  _ : bi : _xs -> At bi
                  _other -> Origin

instance Rewindable UtxoHandle where
  rewindStorage :: C.ChainPoint -> UtxoHandle -> StorableMonad UtxoHandle UtxoHandle
  rewindStorage cp@(C.ChainPoint sn _) h@(UtxoHandle c _ _) = liftSQLError CantRollback $ do
    SQL.execute c "DELETE FROM unspent_transactions WHERE slotNo > ?" (SQL.Only sn)
    SQL.execute c "DELETE FROM spent WHERE slotNo > ?" (SQL.Only sn)
    rollbackLastSyncPoints c cp
    pure h
  rewindStorage C.ChainPointAtGenesis h@(UtxoHandle c _ _) = liftSQLError CantRollback $ do
    SQL.execute_ c "DELETE FROM unspent_transactions"
    SQL.execute_ c "DELETE FROM spent"
    rollbackLastSyncPoints c C.ChainPointAtGenesis
    pure h

-- For resuming we need to provide a list of points where we can resume from.
instance Resumable UtxoHandle where
  resumeFromStorage (UtxoHandle c _ _) =
    liftSQLError CantQueryIndexer $ queryLastSyncPoint c

-- | Convert from 'AddressInEra' of the 'CurrentEra' to 'AddressAny'.
toAddr :: C.AddressInEra era -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr) = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

-- | Extract UtxoEvents from Cardano Block
getUtxoEventsFromBlock
  :: (C.IsCardanoEra era)
  => UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> C.Block era
  -> C.EpochNo
  -> POSIXTime
  -> StorableEvent UtxoHandle
  -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEventsFromBlock utxoIndexerConfig (C.Block (C.BlockHeader slotNo bhh blockNo) txs) epochNo posixTime =
  let (blockTimeStampSeconds, _) = properFraction $ nominalDiffTimeToSeconds posixTime
      blockInfo = BlockInfo slotNo bhh blockNo blockTimeStampSeconds epochNo
   in getUtxoEvents utxoIndexerConfig txs blockInfo

-- | Extract UtxoEvents from Cardano Transactions
getUtxoEvents
  :: (C.IsCardanoEra era)
  => UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> [C.Tx era]
  -> BlockInfo
  -> StorableEvent UtxoHandle
  -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents utxoIndexerConfig@(UtxoIndexerConfig maybeTargetAddresses _) txs bi =
  let (TxOutBalance utxos spentTxOuts) =
        foldMap (balanceUtxoFromTx utxoIndexerConfig) $ zip txs [0 ..]
      resolvedUtxos :: [Utxo]
      resolvedUtxos = Map.elems utxos
      plutusDatums :: Map (C.Hash C.ScriptData) C.ScriptData
      plutusDatums = Datum.getPlutusDatumsFromTxs txs
      filteredTxOutDatums :: Map (C.Hash C.ScriptData) C.ScriptData
      filteredTxOutDatums =
        Map.fromList $
          rights $
            map snd $
              Datum.getFilteredAddressDatumsFromTxs (addressesToPredicate maybeTargetAddresses) txs
   in UtxoEvent resolvedUtxos spentTxOuts bi $ Map.union plutusDatums filteredTxOutDatums

-- | does the transaction contain a targetAddress
isAddressInTarget :: Maybe TargetAddresses -> C.AddressAny -> Bool
isAddressInTarget Nothing _ = True -- all addresses are target addresses
isAddressInTarget (Just targetAddresses) addr =
  case addr of
    C.AddressByron _ -> False
    C.AddressShelley addr' -> addr' `elem` targetAddresses

getTxOutFromTxBodyContent :: C.TxBodyContent build era -> [C.TxOut C.CtxTx era]
getTxOutFromTxBodyContent C.TxBodyContent{C.txOuts, C.txReturnCollateral, C.txScriptValidity} = case txScriptValidityToScriptValidity txScriptValidity of
  C.ScriptValid -> txOuts -- When transaction is valid, only transaction fee is collected
  C.ScriptInvalid -> collateral txReturnCollateral -- failed Tx, we collect from collateral and return excess collateral
  where
    collateral C.TxReturnCollateralNone = []
    collateral (C.TxReturnCollateral _ txout) = [txout]

getUtxosFromTxBody
  :: (C.IsCardanoEra era)
  => UtxoIndexerConfig
  -- ^ Utxo Injdexer Configuration, containing targetAddresses and showReferenceScript flag
  -> C.TxBody era
  -> TxIndexInBlock
  -> Map C.TxIn Utxo
getUtxosFromTxBody utxoIndexerConfig txBody@(C.TxBody txBodyContent@C.TxBodyContent{}) txIndexInBlock' =
  fromRight Map.empty (getUtxos $ getTxOutFromTxBodyContent txBodyContent)
  where
    getUtxos :: (C.IsCardanoEra era) => [C.TxOut C.CtxTx era] -> Either C.EraCastError (Map C.TxIn Utxo)
    getUtxos =
      fmap (mconcat . imap txoutToUtxo)
        . traverse (C.eraCast CurrentEra)

    txid = C.getTxId txBody
    txoutToUtxo :: Int -> TxOut -> Map C.TxIn Utxo
    txoutToUtxo ix txout =
      let txin = C.TxIn txid (C.TxIx (fromIntegral ix))
       in case getUtxoFromTxOut utxoIndexerConfig txin txout txIndexInBlock' of
            Nothing -> Map.empty
            Just utxo -> Map.singleton txin utxo

getUtxoFromTxOut
  :: UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> C.TxIn
  -- ^ unique id and position of this transaction
  -> C.TxOut C.CtxTx era
  -- ^ Cardano TxOut
  -> TxIndexInBlock
  -> Maybe Utxo
  -- ^ Utxo
getUtxoFromTxOut (UtxoIndexerConfig maybeTargetAddresses storeReferenceScript) _txIn (C.TxOut addr val dtum refScript) _txIndexInBlock = do
  guard $ isAddressInTarget maybeTargetAddresses _address
  pure $
    Utxo
      { _txIn
      , _address
      , _value = C.txOutValueToValue val
      , _datumHash
      , _inlineScript
      , _inlineScriptHash
      , _txIndexInBlock
      }
  where
    _address = toAddr addr
    (_datum, _datumHash) = case Datum.getTxOutDatumOrHash dtum of
      Nothing -> (Nothing, Nothing)
      Just e -> case e of
        Left hash -> (Nothing, Just hash)
        Right (datumHash'', datum'') -> (Just datum'', Just datumHash'')
    (_inlineScript, _inlineScriptHash) =
      if storeReferenceScript
        then getRefScriptAndHash refScript
        else (Nothing, Nothing) -- supress saving ReferenceScript and its hash

-- | get the inlineScript and inlineScriptHash
getRefScriptAndHash
  :: C.ReferenceScript era
  -> (Maybe C.ScriptInAnyLang, Maybe C.ScriptHash)
getRefScriptAndHash refScript = case refScript of
  C.ReferenceScriptNone -> (Nothing, Nothing)
  C.ReferenceScript _ s@(C.ScriptInAnyLang C.SimpleScriptLanguage script) ->
    ( Just s
    , Just . C.hashScript $ script
    )
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script) ->
    ( Just s
    , Just . C.hashScript $ script
    )
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script) ->
    ( Just s
    , Just . C.hashScript $ script
    )
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script) ->
    ( Just s
    , Just . C.hashScript $ script
    )

getInputsFromTx :: C.Tx era -> Map C.TxIn C.TxId
getInputsFromTx (C.Tx txbody _) = getInputs txbody

{- | Compute TxIn
  If phase-2 validation fails, we only process TxIns associated with collateral
-}
getInputs :: C.TxBody era -> Map C.TxIn C.TxId
getInputs
  b@( C.TxBody
        C.TxBodyContent
          { C.txIns
          , C.txInsCollateral
          , C.txScriptValidity
          }
      ) =
    let inputs = case txScriptValidityToScriptValidity txScriptValidity of
          C.ScriptValid -> fst <$> txIns
          C.ScriptInvalid -> case txInsCollateral of
            C.TxInsCollateralNone -> []
            C.TxInsCollateral _ txins -> txins
     in Map.fromList $ (,C.getTxId b) <$> inputs

{- | Duplicated from cardano-api (not exposed in cardano-api)
 This function should be removed when marconi will depend on a cardano-api version that has accepted this PR:
 https://github.com/input-output-hk/cardano-node/pull/4569
-}
txScriptValidityToScriptValidity :: C.TxScriptValidity era -> C.ScriptValidity
txScriptValidityToScriptValidity C.TxScriptValidityNone = C.ScriptValid
txScriptValidityToScriptValidity (C.TxScriptValidity _ scriptValidity) = scriptValidity

-- | does the transaction contain a targetAddress
isAddressInTarget' :: TargetAddresses -> Utxo -> Bool
isAddressInTarget' targetAddresses utxo =
  case utxo ^. address of
    C.AddressByron _ -> False
    C.AddressShelley addr' -> addr' `elem` targetAddresses

balanceUtxoFromTx
  :: (C.IsCardanoEra era)
  => UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> (C.Tx era, TxIndexInBlock)
  -> TxOutBalance
balanceUtxoFromTx utxoIndexerConfig (C.Tx txBody _, txIndexInBlock') =
  let txInputs = getInputs txBody -- adjusted txInput after phase-2 validation
      utxoRefs :: Map C.TxIn Utxo
      utxoRefs = getUtxosFromTxBody utxoIndexerConfig txBody txIndexInBlock'
   in TxOutBalance utxoRefs txInputs
