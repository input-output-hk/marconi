{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- This module will create the SQL tables:

-- + table: address_datums
--
-- @
--    |---------+------------+---------+------------|
--    | address | datum_hash | slot_no | block_hash |
--    |---------+------------+---------+------------|
-- @
--
-- + table: datumhash_datum
--
-- @
--    |------------+-------|
--    | datum_hash | datum |
--    |------------+-------|
-- @

-- To create these tables, we extract all transactions outputs from each transactions fetched with
-- the chain-sync protocol of the local node.

-- Here is a synopsis of the indexing algorithm.

-- Each transaction output contains an address along with an optional datum hash or an optional inline
-- datum (actual datum).
-- In the inline datum scenario, we simply create an entry in the `address_datums` table with the hash
-- of the datum, and add an entry in the `datumhash_datum` table.
-- In the datum hash scenario, we create an entry in the `address_datums` table, but not in the
-- `datumhash_datum` table, as we don't know the actual value of the datum and we can't infer it from
-- the hash.
-- In that last scenario, we can resolve in the datum hash in one of three ways (which we then simply
-- add an entry in the `datumhash_datum` table):
--
--     * a different transaction output has an inline datum with that same hash
--
--     * a datum with that same hash has been found in the in transaction body
--
--     * a datum with that same hash was included in the witnesses for a Plutus spending script
--     which was included in the transaction body
--
module Marconi.ChainIndex.Legacy.Indexers.AddressDatum (
  -- * AddressDatumIndex
  AddressDatumIndex,
  AddressDatumHandle (AddressDatumHandle),
  StorableEvent (..),
  StorableQuery (..),
  StorableResult (..),
  toAddressDatumIndexEvent,
  AddressDatumQuery,
  AddressDatumResult,
  AddressDatumDepth (..),
  open,
  toDatumRow,
) where

import Cardano.Api qualified as C
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (rights)
import Data.Foldable (
  Foldable (foldl'),
  fold,
  toList,
 )
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (
  catMaybes,
  listToMaybe,
  mapMaybe,
 )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Legacy.Error (
  IndexerError (CantInsertEvent, CantQueryIndexer, CantRollback, CantStartIndexer),
  liftSQLError,
 )
import Marconi.ChainIndex.Legacy.Extract.Datum qualified as Datum
import Marconi.ChainIndex.Legacy.Indexers.LastSync (
  createLastSyncTable,
  insertLastSyncPoints,
  queryLastSyncPoint,
  rollbackLastSyncPoints,
 )
import Marconi.ChainIndex.Legacy.Orphans ()
import Marconi.Core.Storable (
  Buffered (persistToStorage),
  HasPoint (getPoint),
  Queryable (queryStorage),
  Resumable,
  Rewindable (rewindStorage),
  StorableEvent,
  StorableMonad,
  StorablePoint,
  StorableQuery,
  StorableResult,
  emptyState,
 )
import Marconi.Core.Storable qualified as Storable

{- | Define the `handler` data type, meant as a wrapper for the connection type (in this case the
 SQLite connection). In this indexer, we also add the number of events that we want to return from
 the on-disk buffer.
-}
data AddressDatumHandle = AddressDatumHandle
  { addressDatumHandleConnection :: SQL.Connection
  , _addressDatumHandleDiskStore :: Int
  }

type instance StorableMonad AddressDatumHandle = ExceptT (IndexerError Void) IO

{- | 'StorableEvent AddressDatumHandle is the type of events. Events are the data atoms that the
 indexer consumes.
 They depend on the `handle` because they need to eventually be persisted in the database, so the
 database has to be able to accomodate them.
-}

-- We store the datum hashes of each address that we processed.
-- Then we keep a separate 'Map' which stores the actual datum given a datum hash.
-- Note that we don't always have the actual datum for a given hash.
data instance StorableEvent AddressDatumHandle
  = AddressDatumIndexEvent
      (Map C.AddressAny (Set (C.Hash C.ScriptData)))
      (Map (C.Hash C.ScriptData) C.ScriptData)
      !C.ChainPoint
  deriving (Eq, Show, Ord)

instance Semigroup (StorableEvent AddressDatumHandle) where
  AddressDatumIndexEvent ad1 d1 c1 <> AddressDatumIndexEvent ad2 d2 c2 =
    AddressDatumIndexEvent
      (Map.unionWith (<>) ad1 ad2)
      (Map.union d1 d2)
      (max c1 c2)

instance Monoid (StorableEvent AddressDatumHandle) where
  mempty = AddressDatumIndexEvent Map.empty Map.empty C.ChainPointAtGenesis
  mappend = (<>)

type instance StorablePoint AddressDatumHandle = C.ChainPoint

instance HasPoint (StorableEvent AddressDatumHandle) C.ChainPoint where
  getPoint (AddressDatumIndexEvent _ _ s) = s

data instance StorableQuery AddressDatumHandle
  = AllAddressesQuery
  | AddressDatumQuery C.AddressAny

data instance StorableResult AddressDatumHandle
  = AllAddressesResult (Set C.AddressAny)
  | AddressDatumResult (Set C.ScriptData)
  deriving (Eq, Show)

type AddressDatumQuery = StorableQuery AddressDatumHandle
type AddressDatumResult = StorableResult AddressDatumHandle

type AddressDatumIndex = Storable.State AddressDatumHandle

newtype AddressDatumDepth = AddressDatumDepth Int

-- * SQLite

data AddressDatumHashRow = AddressDatumHashRow
  { addressDatumRowAddress :: !C.AddressAny
  , addressDatumRowDatumHash :: !(C.Hash C.ScriptData)
  , addressDatumRowSlot :: !C.SlotNo
  , addressDatumRowBlockHash :: !(C.Hash C.BlockHeader)
  }
  deriving (Show, Generic)

instance SQL.ToRow AddressDatumHashRow where
  toRow (AddressDatumHashRow addr d slotNo blockHash) =
    [ SQL.toField addr
    , SQL.toField d
    , SQL.toField slotNo
    , SQL.toField blockHash
    ]

deriving anyclass instance SQL.FromRow AddressDatumHashRow

data DatumRow = DatumRow
  { datumRowDatumHash :: C.Hash C.ScriptData
  , datumRowDatum :: C.ScriptData
  }
  deriving (Show, Generic)

instance SQL.ToRow DatumRow where
  toRow (DatumRow dh d) = [SQL.toField dh, SQL.toField d]

deriving anyclass instance SQL.FromRow DatumRow

toAddressDatumIndexEvent
  :: Maybe (C.Address C.ShelleyAddr -> Bool)
  -> [C.Tx era]
  -> C.ChainPoint
  -> StorableEvent AddressDatumHandle
toAddressDatumIndexEvent addressFilter txs = AddressDatumIndexEvent addressDatumHashMap (Map.union filteredTxOutDatums plutusDatums)
  where
    plutusDatums :: Map (C.Hash C.ScriptData) C.ScriptData
    plutusDatums = Datum.getPlutusDatumsFromTxs txs

    filteredAddressDatums
      :: [(C.AddressAny, Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))]
    filteredAddressDatums = Datum.getFilteredAddressDatumsFromTxs addressFilter txs

    filteredTxOutDatums :: Map (C.Hash C.ScriptData) C.ScriptData
    filteredTxOutDatums = Map.fromList $ rights $ map snd filteredAddressDatums

    addressDatumHashMap :: Map C.AddressAny (Set (C.Hash C.ScriptData))
    addressDatumHashMap = Map.fromListWith Set.union $ map (fmap (Set.singleton . either id fst)) filteredAddressDatums

instance Buffered AddressDatumHandle where
  persistToStorage
    :: (Foldable f)
    => f (StorableEvent AddressDatumHandle)
    -> AddressDatumHandle
    -> StorableMonad AddressDatumHandle AddressDatumHandle
  persistToStorage es h =
    liftSQLError CantInsertEvent $ do
      let addressDatumHashRows = foldl' (\ea e -> ea ++ toAddressDatumHashRow e) [] es
          datumRows = foldl' (\ea e -> ea ++ toDatumRow e) [] es
          c = addressDatumHandleConnection h
          getChainPoint (AddressDatumIndexEvent _ _ cp) = cp
          chainPoints = getChainPoint <$> toList es

      SQL.withTransaction c $ do
        forM_ addressDatumHashRows $
          SQL.execute
            c
            [sql|INSERT INTO address_datums
                ( address
                , datum_hash
                , slot_no
                , block_hash
                )
               VALUES (?, ?, ?, ?)|]

        SQL.executeMany
          c
          [sql|INSERT OR IGNORE INTO datumhash_datum
               ( datum_hash
               , datum
               )
               VALUES (?, ?)|]
          datumRows

        insertLastSyncPoints c chainPoints

      pure h
    where
      toAddressDatumHashRow :: StorableEvent AddressDatumHandle -> [AddressDatumHashRow]
      toAddressDatumHashRow (AddressDatumIndexEvent _ _ C.ChainPointAtGenesis) = []
      toAddressDatumHashRow (AddressDatumIndexEvent addressDatumHashMap _ (C.ChainPoint sl bh)) = do
        (addr, dhs) <- Map.toList addressDatumHashMap
        dh <- Set.toList dhs
        pure $ AddressDatumHashRow addr dh sl bh

  getStoredEvents
    :: AddressDatumHandle
    -> StorableMonad AddressDatumHandle [StorableEvent AddressDatumHandle]
  getStoredEvents (AddressDatumHandle c n) =
    liftSQLError CantQueryIndexer $ do
      sns :: [[Integer]] <-
        SQL.query
          c
          [sql|SELECT slot_no
               FROM address_datums
               GROUP BY slot_no
               ORDER BY slot_no
               DESC LIMIT ?|]
          (SQL.Only n)
      -- Take the slot number of the sz'th slot
      let sn =
            if null sns
              then 0
              else head . last $ take n sns
      res <-
        SQL.query
          c
          [sql|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
               FROM address_datums
               LEFT JOIN datumhash_datum
               ON datumhash_datum.datum_hash = address_datums.datum_hash
               WHERE slot_no >= ?
               ORDER BY slot_no DESC, address, datumhash_datum.datum_hash|]
          (SQL.Only (sn :: Integer))
      pure $ asEvents res

toDatumRow :: StorableEvent AddressDatumHandle -> [DatumRow]
toDatumRow (AddressDatumIndexEvent _ datumMap _) =
  fmap (uncurry DatumRow) $ Map.toList datumMap

-- | This function recomposes the in-memory format from the database records.
asEvents
  :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo, C.Hash C.BlockHeader)]
  -- ^ Should be sorted by C.SlotNo in ascending order.
  -> [StorableEvent AddressDatumHandle]
asEvents events =
  fmap toEvent $ NonEmpty.groupWith (\(_, _, _, s, _) -> s) events
  where
    toEvent
      :: NonEmpty
          ( C.AddressAny
          , C.Hash C.ScriptData
          , Maybe C.ScriptData
          , C.SlotNo
          , C.Hash C.BlockHeader
          )
      -> StorableEvent AddressDatumHandle
    toEvent es =
      let (_, _, _, slot, blockHash) = NonEmpty.head es
       in AddressDatumIndexEvent (toAddressDatums es) (toDatumMap es) (C.ChainPoint slot blockHash)

    toAddressDatums
      :: NonEmpty
          ( C.AddressAny
          , C.Hash C.ScriptData
          , Maybe C.ScriptData
          , C.SlotNo
          , C.Hash C.BlockHeader
          )
      -> Map C.AddressAny (Set (C.Hash C.ScriptData))
    toAddressDatums es =
      Map.fromListWith (<>) $
        NonEmpty.toList $
          fmap (\(addr, dh, _, _, _) -> (addr, Set.singleton dh)) es

    toDatumMap
      :: NonEmpty
          ( C.AddressAny
          , C.Hash C.ScriptData
          , Maybe C.ScriptData
          , C.SlotNo
          , C.Hash C.BlockHeader
          )
      -> Map (C.Hash C.ScriptData) C.ScriptData
    toDatumMap es =
      Map.fromList $
        catMaybes $
          NonEmpty.toList $
            fmap (\(_, dh, d, _, _) -> fmap (dh,) d) es

instance Queryable AddressDatumHandle where
  queryStorage
    :: (Foldable f)
    => f (StorableEvent AddressDatumHandle)
    -> AddressDatumHandle
    -> StorableQuery AddressDatumHandle
    -> StorableMonad AddressDatumHandle (StorableResult AddressDatumHandle)
  queryStorage es (AddressDatumHandle c _) AllAddressesQuery = liftSQLError CantQueryIndexer $ do
    persistedData
      :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo, C.Hash C.BlockHeader)] <-
      SQL.query
        c
        [sql|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
                   FROM address_datums
                   LEFT JOIN datumhash_datum
                   ON datumhash_datum.datum_hash = address_datums.datum_hash
                   ORDER BY slot_no ASC, address, datumhash_datum.datum_hash|]
        ()
    let addressDatumIndexEvents = asEvents persistedData ++ toList es
    pure $
      AllAddressesResult $
        Set.fromList $
          concatMap (\(AddressDatumIndexEvent addrMap _ _) -> Map.keys addrMap) addressDatumIndexEvents
  queryStorage es (AddressDatumHandle c _) (AddressDatumQuery q) =
    liftSQLError CantQueryIndexer $ do
      persistedData
        :: [(C.AddressAny, C.Hash C.ScriptData, Maybe C.ScriptData, C.SlotNo, C.Hash C.BlockHeader)] <-
        SQL.query
          c
          [sql|SELECT address, address_datums.datum_hash, datumhash_datum.datum, slot_no, block_hash
                   FROM address_datums
                   LEFT JOIN datumhash_datum
                   ON datumhash_datum.datum_hash = address_datums.datum_hash
                   WHERE address = ?
                   ORDER BY slot_no ASC, address, datumhash_datum.datum_hash|]
          (SQL.Only q)

      -- IMPORTANT: Ordering is quite important here, as the `filterWithQueryInterval`
      -- function assumes events are ordered from oldest (the head) to most recent.
      let addressDatumIndexEvents = asEvents persistedData ++ toList es
      let (AddressDatumIndexEvent addressDatumMap datumMap cp) = fold addressDatumIndexEvents

      -- Datum hashes that are linked to an address, but do not have a corresponding datum value
      -- associated with it.
      let unresolvedDatumHashes =
            Set.toList $ fold (Map.elems addressDatumMap) `Set.difference` Map.keysSet datumMap
      datums <- forM unresolvedDatumHashes $ \dh -> do
        maybeDatumRow <- findDatum dh
        pure $ fmap (\(DatumRow _ d) -> (dh, d)) maybeDatumRow
      let resolvedDatumHashes = Map.fromList $ catMaybes datums

      pure $
        AddressDatumResult $
          storableEventToResult
            q
            (AddressDatumIndexEvent addressDatumMap (datumMap <> resolvedDatumHashes) cp)
    where
      storableEventToResult
        :: C.AddressAny
        -> StorableEvent AddressDatumHandle
        -> Set C.ScriptData
      storableEventToResult targetAddr (AddressDatumIndexEvent addressDatumMap datumMap _chainPoint) =
        Set.map snd $
          Set.filter ((==) targetAddr . fst) $
            foldMap (\(addr, datumHashes) -> Set.map (addr,) $ resolveMapKeys datumHashes datumMap) $
              Map.toList addressDatumMap

      resolveMapKeys
        :: (Ord k, Ord v)
        => Set k
        -> Map k v
        -> Set v
      resolveMapKeys keys m =
        -- TODO Not efficient to convert back n forth between Set
        Set.fromList $ mapMaybe (\k -> Map.lookup k m) $ Set.toList keys

      findDatum :: C.Hash C.ScriptData -> IO (Maybe DatumRow)
      findDatum hash = do
        listToMaybe
          <$> SQL.query
            c
            "SELECT datum_hash, datum FROM datumhash_datum WHERE datum_hash = ?"
            (SQL.Only hash)

instance Rewindable AddressDatumHandle where
  rewindStorage
    :: C.ChainPoint
    -> AddressDatumHandle
    -> StorableMonad AddressDatumHandle AddressDatumHandle
  rewindStorage C.ChainPointAtGenesis h@(AddressDatumHandle c _) =
    liftSQLError CantRollback $ do
      SQL.execute_ c "DELETE FROM address_datums"
      rollbackLastSyncPoints c C.ChainPointAtGenesis
      pure h
  rewindStorage cp@(C.ChainPoint sn _) h@(AddressDatumHandle c _) =
    liftSQLError CantRollback $ do
      SQL.execute c "DELETE FROM address_datums WHERE slot_no > ?" (SQL.Only sn)
      rollbackLastSyncPoints c cp
      pure h

instance Resumable AddressDatumHandle where
  resumeFromStorage
    :: AddressDatumHandle
    -> StorableMonad AddressDatumHandle C.ChainPoint
  resumeFromStorage (AddressDatumHandle c _) =
    liftSQLError CantQueryIndexer $ queryLastSyncPoint c

open
  :: FilePath
  -> AddressDatumDepth
  -> StorableMonad AddressDatumHandle AddressDatumIndex
open dbPath (AddressDatumDepth k) = do
  c <- liftSQLError CantStartIndexer (SQL.open dbPath)
  lift $ SQL.execute_ c "PRAGMA journal_mode=WAL"
  lift $
    SQL.execute_
      c
      [sql|CREATE TABLE IF NOT EXISTS address_datums
            ( address TEXT NOT NULL
            , datum_hash BLOB NOT NULL
            , slot_no INT NOT NULL
            , block_hash BLOB NOT NULL
            )|]
  lift $
    SQL.execute_
      c
      [sql|CREATE TABLE IF NOT EXISTS datumhash_datum
                      ( datum_hash BLOB PRIMARY KEY
                      , datum BLOB
                      )|]
  lift $
    SQL.execute_
      c
      [sql|CREATE INDEX IF NOT EXISTS address_datums_index
           ON address_datums (address)|]

  lift $ createLastSyncTable c

  emptyState k (AddressDatumHandle c k)
