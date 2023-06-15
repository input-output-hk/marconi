{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
--      |---------+------+-------+-----------+-------+-------+------------------+--------------+------+-----------------+---------|----------------|
--      | Address | TxId | TxIx  | DatumHash | Datum | Value | InlineScriptHash | InlineScript | Slot | BlockHeaderHash | BlockNo | TxIndexInBlock |
--      |---------+------+-------+-----------+-------+-------+------------------+--------------+------+-----------------+---------|----------------|
-- @
--
-- + table: spent
-- @
--      |------+------|--------+-----------------|
--      | txId | txIx | slotNo | blockHeaderHash |
--      |------+------|--------+-----------------|
-- @
-- To create these tables, we extract all transactions outputs from each transactions fetched with
-- the chain-sync protocol of the local node.

-- | Module for indexing the Utxos in the Cardano blockchain
module Marconi.ChainIndex.Indexers.Utxo where

import Control.Concurrent.Async (concurrently_)
import Control.Exception (bracket_)
import Control.Lens.Combinators (
  Lens',
  Traversal',
  imap,
  preview,
  view,
  _Just,
 )
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (guard, unless, when)
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
import Data.Either (fromRight)
import Data.Foldable (foldl', toList)
import Data.List (groupBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text qualified as Text
import Data.Word (Word64)
import Database.SQLite.Simple (
  NamedParam ((:=)),
  ResultError (UnexpectedNull),
 )
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field, fieldWith)
import Database.SQLite.Simple.ToField (ToField (toField), toField)
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import System.Random.MWC (
  createSystemRandom,
  uniformR,
 )
import Text.RawString.QQ (r)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Marconi.ChainIndex.Error (
  IndexerError (CantInsertEvent, CantQueryIndexer, CantRollback, CantStartIndexer, InvalidQueryInterval),
  liftSQLError,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (
  TargetAddresses,
  TxIndexInBlock,
  TxOut,
  UtxoIndexerConfig (UtxoIndexerConfig),
  pattern CurrentEra,
 )
import Marconi.ChainIndex.Utils (chainPointOrGenesis)
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
  :: (Ord r, Show r)
  => Maybe r
  -- ^ lower bound
  -> r
  -- ^ upper bound
  -> Either IndexerError (Interval r)
interval Nothing p = Right $ LessThanOrEqual p
interval (Just p) p' =
  let --  Enforce the internal invariant
      -- 'InRange'.
      wrap
        :: (Ord r, Show r)
        => (r -> r -> Interval r)
        -> r
        -> r
        -> Either IndexerError (Interval r)
      wrap f x y
        | x <= y = Right $ f x y
        | otherwise =
            Left . InvalidQueryInterval . pack $
              "Invalid Interval. LowerBound, "
                <> show x
                <> " is not less than or equal to upperBound "
                <> show y
   in wrap InRange p p'

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
  , hdlDpeth :: !Int
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

type QueryableAddresses = NonEmpty (StorableQuery UtxoHandle)

type instance StorableMonad UtxoHandle = ExceptT IndexerError IO

type instance StorablePoint UtxoHandle = C.ChainPoint

newtype Depth = Depth Int

data Utxo = Utxo
  { _address :: !C.AddressAny
  , _txIn :: !C.TxIn
  , _datum :: !(Maybe C.ScriptData)
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
      <*> v .: "datum"
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
          , "datum" .= (u ^. datum)
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
  deriving anyclass (FromRow)

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

$(makeLenses ''SpentInfo)

data UtxoRow = UtxoRow
  { _urUtxo :: !Utxo
  , _urBlockInfo :: !BlockInfo
  , _urSpentInfo :: !(Maybe SpentInfo)
  }
  deriving (Show, Eq, Ord, Generic)

$(makeLenses ''UtxoRow)

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
    UtxoResult {getUtxoResult :: ![UtxoRow]}
  | -- | Result of a 'LastSyncedBlockInfoQuery'
    LastSyncedBlockInfoResult {getLastSyncedBlockInfo :: !(WithOrigin BlockInfo)}
  deriving (Eq, Show, Ord)

data instance StorableEvent UtxoHandle = UtxoEvent
  { ueUtxos :: !(Set Utxo)
  , ueInputs :: !(Map C.TxIn C.TxId)
  , ueBlockInfo :: !BlockInfo
  }
  deriving (Eq, Ord, Show, Generic)

-- | mappend, combine and balance Utxos
instance Semigroup (StorableEvent UtxoHandle) where
  (UtxoEvent us is bi) <> (UtxoEvent us' is' bi') =
    let txins = Map.union is is'

        insertUnspent :: Set Utxo -> Utxo -> Set Utxo
        insertUnspent acc u =
          if (u ^. txIn) `Map.notMember` txins
            then Set.insert u acc
            else acc

        utxos =
          foldl' insertUnspent Set.empty $
            Set.union us us'
     in UtxoEvent utxos txins (max bi bi')

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
    let bUnspentKeys :: Set C.TxIn
        bUnspentKeys =
          Map.keysSet $
            (bUtxoR ^. tbUnspent)
              <> ((bUtxoL ^. tbUnspent) `Map.difference` (bUtxoR ^. tbSpent))
        utxoMap :: Map C.TxIn Utxo
        utxoMap = _tbUnspent bUtxoL `Map.union` _tbUnspent bUtxoR
        bSpent =
          bUtxoL
            ^. tbSpent
            <> ((bUtxoR ^. tbSpent) `Map.difference` (bUtxoL ^. tbUnspent))
     in TxOutBalance
          { _tbUnspent = Map.restrictKeys utxoMap bUnspentKeys
          , _tbSpent = bSpent
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
          , toField $ u ^. urUtxo . datum
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
      [r|CREATE TABLE IF NOT EXISTS unspent_transactions
                      ( address TEXT NOT NULL
                      , txId TEXT NOT NULL
                      , txIx INT NOT NULL
                      , datum BLOB
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
      [r|CREATE TABLE IF NOT EXISTS spent
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
      [r|CREATE INDEX IF NOT EXISTS
                      spent_slotNo ON spent (slotNo)|]

  lift $
    SQL.execute_
      c
      [r|CREATE INDEX IF NOT EXISTS
                      spent_txId ON spent (txId, txIx)|]

  lift $
    SQL.execute_
      c
      [r|CREATE INDEX IF NOT EXISTS
                      unspent_transaction_address ON unspent_transactions (address)|]

  emptyState k (UtxoHandle c k isToVacuume)

getSpentFrom :: StorableEvent UtxoHandle -> [Spent]
getSpentFrom (UtxoEvent _ txIns bi) = do
  (txin, spentTxId) <- Map.toList txIns
  pure $ Spent txin (SpentInfo bi spentTxId)

{- | Store UtxoEvents
 Events are stored in memory and flushed to SQL, disk, when memory buffer has reached capacity
-}
instance Buffered UtxoHandle where
  persistToStorage
    :: Foldable f
    => f (StorableEvent UtxoHandle) -- Events to store
    -> UtxoHandle -- Handler for storing events
    -> StorableMonad UtxoHandle UtxoHandle
  persistToStorage events h =
    liftSQLError CantInsertEvent $ do
      let rows = concatMap eventToRows events
          spents = concatMap getSpentFrom events
          c = hdlConnection h
      bracket_
        (SQL.execute_ c "BEGIN")
        (SQL.execute_ c "COMMIT")
        ( concurrently_
            ( unless
                (null rows)
                ( SQL.executeMany
                    c
                    [r|INSERT
                       INTO unspent_transactions (
                         address,
                         txId,
                         txIx,
                         datum,
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
                      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|]
                    rows
                )
            )
            ( unless
                (null spents)
                ( SQL.executeMany
                    c
                    [r|INSERT
                       INTO spent (
                         txId,
                         txIx,
                         slotNo,
                         blockHeaderHash,
                         blockNo,
                         blockTimestamp,
                         epochNo,
                         spentTxId
                       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)|]
                    spents
                )
            )
        )
      -- We want to perform vacuum about once every 100
      when (toVacuume h) $ do
        rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
        when (rndCheck == 42) $ do
          SQL.execute_
            c
            [r|DELETE FROM
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
  getStoredEvents (UtxoHandle c sz _) = liftSQLError CantQueryIndexer $ do
    sns <-
      SQL.query
        c
        [r|SELECT slotNo
           FROM unspent_transactions
           GROUP BY slotNo
           ORDER BY slotNo DESC
           LIMIT ?|]
        (SQL.Only sz)
      :: IO [[Word64]]

    -- Take the slot number of the sz'th slot
    let sn =
          if null sns
            then 0
            else head . last $ take sz sns

    rows :: [UtxoRow] <-
      SQL.query
        c
        [r|SELECT
              u.address,
              u.txId,
              u.txIx,
              u.datum,
              u.datumHash,
              u.value,
              u.inlineScript,
              u.inlineScriptHash,
              u.txIndexInBlock,
              u.slotNo,
              u.blockHeaderHash,
              u.blockNo,
              u.blockTimestamp,
              u.epochNo
           FROM
              unspent_transactions u
           WHERE
              u.slotNo >= ?
           GROUP by
              u.slotNo
           ORDER BY
              u.slotNo ASC|]
        (SQL.Only (sn :: Word64))

    rowsToEvents (getTxIns c) rows

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

-- | Convert UtxoRows to UtxoEvents
rowsToEvents
  :: (C.SlotNo -> IO (Map C.TxIn C.TxId))
  -- ^ function to fetch TxIn
  -> [UtxoRow]
  -- ^ rows to convert back to event
  -> IO [StorableEvent UtxoHandle]
  -- ^ utxo events
rowsToEvents _ [] = pure []
rowsToEvents fetchTxIn rows =
  sortOn (_blockInfoSlotNo . ueBlockInfo) <$> traverse reduce eventsMap
  where
    mkEvent :: UtxoRow -> StorableEvent UtxoHandle
    mkEvent row =
      UtxoEvent
        (Set.singleton $ row ^. urUtxo)
        Map.empty
        ( BlockInfo
            (row ^. urCreationSlotNo)
            (row ^. urCreationBlockHeaderHash)
            (row ^. urCreationBlockNo)
            (row ^. urCreationBlockTimestamp)
            (row ^. urCreationEpochNo)
        )

    newEventWithSpentOnly :: Map C.TxIn C.TxId -> BlockInfo -> StorableEvent UtxoHandle
    newEventWithSpentOnly = UtxoEvent Set.empty

    reduce :: (BlockInfo, [StorableEvent UtxoHandle]) -> IO (StorableEvent UtxoHandle)
    reduce (bi@(BlockInfo sn _ _ _ _), es) = do
      tins <- fetchTxIn sn
      let newE = newEventWithSpentOnly tins bi
      pure $ foldl (<>) newE es

    eventsMap :: [(BlockInfo, [StorableEvent UtxoHandle])]
    eventsMap =
      fmap (\x -> (ueBlockInfo $ head x, x)) $
        groupBy (\el er -> ueBlockInfo el == ueBlockInfo er) $
          fmap mkEvent rows

{- | convert utxoEvent to utxoRow
 Note: No `unspent` computation is performed
-}
eventToRows :: StorableEvent UtxoHandle -> [UtxoRow]
eventToRows (UtxoEvent utxos _ bi) =
  let eventToRow u =
        UtxoRow
          { _urUtxo = u
          , _urBlockInfo = bi
          , _urSpentInfo = Nothing
          }
   in fmap eventToRow $ Set.toList utxos

{- | Used internally to gather the information required
 to update the in-database result
 from the UtoxByAddress query
 with the in-memory events
-}
data UtxoByAddressBufferEvents = UtxoByAddressBufferEvents
  { _bufferUtxos :: ![StorableEvent UtxoHandle]
  -- ^ Utxos at the requested address
  , _bufferSpent :: !(Set C.TxIn)
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

{- | merge in-memory events with SQL retrieved UtxoRows
 Notes, a property of this merge is to remove all spent utxos from the resulting [UtxoRow]
-}
mergeInMemoryAndSql
  :: UtxoByAddressBufferEvents
  -> [UtxoRow]
  -> [UtxoRow]
mergeInMemoryAndSql bufferEvents =
  let asTxIn :: UtxoRow -> C.TxIn
      asTxIn u = u ^. urUtxo . txIn

      excludeBufferSpent :: [UtxoRow] -> [UtxoRow]
      excludeBufferSpent = filter (flip Set.notMember (bufferEvents ^. bufferSpent) . asTxIn)

      updateFutureSpent :: UtxoRow -> UtxoRow
      updateFutureSpent u =
        let spent = Map.lookup (asTxIn u) $ bufferEvents ^. bufferFutureSpent
         in case u ^. urSpentInfo of
              -- if it's already spent, no need to check if it's spent in the buffer events
              Just _ -> u
              Nothing -> u & urSpentInfo .~ spent

      appendBufferUtxos :: [UtxoRow] -> [UtxoRow]
      appendBufferUtxos = (<> (bufferEvents ^. bufferUtxos >>= eventToRows))
   in fmap updateFutureSpent
        . excludeBufferSpent
        . appendBufferUtxos

-- | Filter for events at the given address
eventsAtAddress
  :: Foldable f
  => QueryUtxoByAddress
  -- ^ Address SlotInterval query
  -> f (StorableEvent UtxoHandle)
  -- ^ Utxo event
  -> UtxoByAddressBufferEvents
eventsAtAddress (QueryUtxoByAddress addr snoInterval) =
  let pointFilter :: StorableEvent UtxoHandle -> Bool
      pointFilter = isInInterval snoInterval . _blockInfoSlotNo . ueBlockInfo

      afterBoundCheck :: C.SlotNo -> Bool
      afterBoundCheck slotNo = case upperBound snoInterval of
        Nothing -> False
        Just s -> slotNo > s

      afterUpperBound :: StorableEvent UtxoHandle -> Bool
      afterUpperBound = afterBoundCheck . _blockInfoSlotNo . ueBlockInfo

      addressFilter :: Utxo -> Bool
      addressFilter u = (u ^. address) == addr

      utxosAtAddress :: StorableEvent UtxoHandle -> Set Utxo
      utxosAtAddress = Set.filter addressFilter . ueUtxos

      splitEventAtAddress :: StorableEvent UtxoHandle -> [StorableEvent UtxoHandle]
      splitEventAtAddress event =
        [ event{ueUtxos = utxosAtAddress event}
        | not (null $ utxosAtAddress event)
            && pointFilter event
        ]

      generateSpentInfo :: StorableEvent UtxoHandle -> C.TxId -> SpentInfo
      generateSpentInfo event = SpentInfo (ueBlockInfo event)

      getBufferSpent :: StorableEvent UtxoHandle -> Set C.TxIn
      getBufferSpent event =
        if afterUpperBound event
          then mempty
          else Map.keysSet $ ueInputs event

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
   in foldMap go

utxoAtAddressQuery
  :: SQL.Connection
  -> UtxoByAddressBufferEvents
  -> [SQL.Query]
  -- ^ the filter part of the query
  -> [NamedParam]
  -> IO (StorableResult UtxoHandle)
utxoAtAddressQuery c bufferResult filters params =
  do
    let builtQuery =
          [r|SELECT
                  u.address,
                  u.txId,
                  u.txIx,
                  u.datum,
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
                  s.spentTxId
               FROM
                  unspent_transactions u
               FULL OUTER JOIN spent s ON u.txId = s.txId
               AND u.txIx = s.txIx
               WHERE |]
            <> SQL.Query (Text.intercalate " AND " $ SQL.fromQuery <$> filters)
            <> [r| ORDER BY
                u.slotNo ASC |]
    persistedUtxoRows :: [UtxoRow] <- SQL.queryNamed c builtQuery params
    pure $
      UtxoResult $
        mergeInMemoryAndSql
          bufferResult
          persistedUtxoRows

{- | Query the data stored in the indexer
 Queries SQL + buffered data, where buffered data is the data that will be batched to SQL
-}
instance Queryable UtxoHandle where
  queryStorage
    :: Foldable f
    => f (StorableEvent UtxoHandle)
    -> UtxoHandle
    -> StorableQuery UtxoHandle
    -> StorableMonad UtxoHandle (StorableResult UtxoHandle)
  queryStorage es (UtxoHandle c _ _) (QueryUtxoByAddressWrapper q@(QueryUtxoByAddress addr slotInterval)) =
    let inMemoryEvents :: UtxoByAddressBufferEvents
        inMemoryEvents = eventsAtAddress q es
        addressFilter = (["u.address = :address"], [":address" := addr])
        lowerBoundFilter = case lowerBound slotInterval of
          Nothing -> mempty
          Just x -> (["u.slotNo >= :slotNoLow"], [":slotNoLow" := x])
        upperBoundFilter = case upperBound slotInterval of
          Nothing -> mempty
          Just x -> (["u.slotNo <= :slotNo", "(s.slotNo IS NULL OR s.slotNo > :slotNo)"], [":slotNo" := x])
        filters = addressFilter <> lowerBoundFilter <> upperBoundFilter
     in liftSQLError CantQueryIndexer $ uncurry (utxoAtAddressQuery c inMemoryEvents) filters
  queryStorage es (UtxoHandle c _ _) LastSyncedBlockInfoQuery =
    let queryLastSlot =
          [r|SELECT u.slotNo, u.blockHeaderHash, u.blockNo, u.blockTimestamp, u.epochNo
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
          (_ : _) -> liftSQLError CantQueryIndexer $ do
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
  rewindStorage (C.ChainPoint sn _) h@(UtxoHandle c _ _) = liftSQLError CantRollback $ do
    SQL.execute c "DELETE FROM unspent_transactions WHERE slotNo > ?" (SQL.Only sn)
    SQL.execute c "DELETE FROM spent WHERE slotNo > ?" (SQL.Only sn)
    pure h
  rewindStorage C.ChainPointAtGenesis h@(UtxoHandle c _ _) = liftSQLError CantRollback $ do
    SQL.execute_ c "DELETE FROM unspent_transactions"
    SQL.execute_ c "DELETE FROM spent"
    pure h

-- For resuming we need to provide a list of points where we can resume from.
instance Resumable UtxoHandle where
  resumeFromStorage (UtxoHandle c _ _) =
    liftSQLError CantQueryIndexer $
      fmap chainPointOrGenesis $
        SQL.query_ c "SELECT slotNo, blockHeaderHash FROM unspent_transactions ORDER BY slotNo DESC LIMIT 1"

-- Add pagination to resume
-- Main reason for adding this is to protect against OOM
-- TODO use withAsync to spread the load. as resume, as is implemented here, could take several minutes depending on the amount of data stored.
resumeHelper :: SQL.Connection -> IO [C.ChainPoint]
resumeHelper c =
  let limit :: Int = 216000
      helper
        :: Int -- Page
        -> [C.ChainPoint]
        -> IO [C.ChainPoint] -- Accumulated chainpoints
      helper page tally = do
        let offset = page * limit
        cps <-
          fmap (uncurry C.ChainPoint)
            <$> SQL.query
              c
              [r|SELECT DISTINCT slotNo, blockHeaderHash
                   FROM unspent_transactions
                   ORDER BY slotNo DESC
                   LIMIT ? OFFSET ? |]
              (limit, offset)
        case cps of
          [] -> pure tally
          cps' -> helper (page + 1) (tally <> cps')
   in helper 0 []

-- | Convert from 'AddressInEra' of the 'CurrentEra' to 'AddressAny'.
toAddr :: C.AddressInEra era -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr) = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

-- | Extract UtxoEvents from Cardano Block
getUtxoEventsFromBlock
  :: C.IsCardanoEra era
  => UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> C.Block era
  -> StorableEvent UtxoHandle
  -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEventsFromBlock utxoIndexerConfig (C.Block (C.BlockHeader slotNo bhh blockNo) txs) =
  let blockInfo = BlockInfo slotNo bhh blockNo 0 1 -- TODO Set the last 2 fields when we can
   in getUtxoEvents utxoIndexerConfig txs blockInfo

-- | Extract UtxoEvents from Cardano Transactions
getUtxoEvents
  :: C.IsCardanoEra era
  => UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> [C.Tx era]
  -> BlockInfo
  -> StorableEvent UtxoHandle
  -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents utxoIndexerConfig txs bi =
  let (TxOutBalance utxos spentTxOuts) =
        foldMap (balanceUtxoFromTx utxoIndexerConfig) $ zip txs [0 ..]
      resolvedUtxos :: Set Utxo
      resolvedUtxos = Set.fromList $ Map.elems utxos
   in UtxoEvent resolvedUtxos spentTxOuts bi

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
    getUtxos :: C.IsCardanoEra era => [C.TxOut C.CtxTx era] -> Either C.EraCastError (Map C.TxIn Utxo)
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
      , _datum
      , _datumHash
      , _inlineScript
      , _inlineScriptHash
      , _txIndexInBlock
      }
  where
    _address = toAddr addr
    (_datum, _datumHash) = getScriptDataAndHash dtum
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

-- | Get the datum hash and datum or a transaction output.
getScriptDataAndHash
  :: C.TxOutDatum C.CtxTx era
  -> (Maybe C.ScriptData, Maybe (C.Hash C.ScriptData))
getScriptDataAndHash C.TxOutDatumNone = (Nothing, Nothing)
getScriptDataAndHash (C.TxOutDatumHash _ h) = (Nothing, Just h)
getScriptDataAndHash (C.TxOutDatumInTx _ d) = (Just $ C.getScriptData d, (Just . C.hashScriptDataBytes) d)
getScriptDataAndHash (C.TxOutDatumInline _ d) = (Just $ C.getScriptData d, (Just . C.hashScriptDataBytes) d)

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
  :: C.IsCardanoEra era
  => UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> (C.Tx era, TxIndexInBlock)
  -> TxOutBalance
balanceUtxoFromTx utxoIndexerConfig (C.Tx txBody _, txIndexInBlock') =
  let txInputs = getInputs txBody -- adjusted txInput after phase-2 validation
      utxoRefs :: Map C.TxIn Utxo
      utxoRefs = getUtxosFromTxBody utxoIndexerConfig txBody txIndexInBlock'
   in TxOutBalance utxoRefs txInputs
