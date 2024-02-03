{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO Make explicit internal modules of marconi-core. Everything expect `Marconi.Core`
-- should be in `Marconi.Core.Internal.*`.
-- TODO Discenpancy between query of ListIndexer and SqliteIndexer in case where we provide
-- ChainPointAtGenesis. Check test scenario when we provide the ChainPointAtGenesis in the
-- generator.
-- TODO Change back Timed to TimedEvent
module Marconi.Starter.Indexers.AddressCount where

import Cardano.Api qualified as C
import Cardano.BM.Trace (nullTracer)
import Control.Exception (throwIO)
import Control.Lens (at, folded, sumOf, to, (^.))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Indexer.Worker qualified as Core
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (BlockEvent (BlockEvent), SecurityParam (SecurityParam))
import Marconi.Cardano.Indexers.SyncHelper (mkSyncedSqliteIndexer)
import Marconi.Core (
  IndexerError,
  ListIndexer,
  MixedIndexer,
  Point,
  Queryable (query),
  Result,
  SQLInsertPlan (SQLInsertPlan),
  SQLRollbackPlan (SQLRollbackPlan),
  SQLiteIndexer,
  Timed (Timed),
  connection,
  event,
  events,
  mkListIndexer,
  mkMixedIndexer,
  point,
 )
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer (SQLiteDBLocation, defaultInsertPlan)

type SQLiteStandardIndexer event = Core.StandardIndexer IO Core.SQLiteIndexer event
type AddressCountIndexer = SQLiteStandardIndexer AddressCountEvent
type AddressCountStandardWorker = Core.StandardWorker IO BlockEvent AddressCountEvent SQLiteIndexer

addressCountWorker
  :: SQLiteDBLocation
  -> SecurityParam
  -> IO
      ( Core.StandardWorker
          IO
          BlockEvent
          AddressCountEvent
          Core.SQLiteIndexer
      )
addressCountWorker dbPath securityParam = do
  let extract = getEventsFromBlock
  ix <- either throwIO pure =<< runExceptT (mkAddressCountSqliteIndexer dbPath)
  let config =
        Core.StandardWorkerConfig
          "AddressCount"
          securityParam
          (Core.mkCatchupConfig 2_000 10)
          (pure . extract)
          nullTracer
  Core.mkStandardWorker config ix

newtype AddressCountEvent = AddressCountEvent {unAddressCountEvent :: Map C.AddressAny Int}
  deriving (Show)

type instance Point AddressCountEvent = C.ChainPoint

-- TODO Not great that we need to add this
-- This is needed when using StandardIndexer.
type instance Point (WithDistance AddressCountEvent) = C.ChainPoint

newtype AddressCountQuery = AddressCountQuery C.AddressAny
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON)

type instance Result AddressCountQuery = Int

-- Note that for Marconi, it is more optimal to return `Nothing` instead of
-- `Just $ AddressCountEvent mempty`.
getEventsFromBlock :: BlockEvent -> Maybe AddressCountEvent
getEventsFromBlock (BlockEvent (C.BlockInMode (C.Block _ txs) _) _ _) =
  let addrCounts = Map.fromListWith (+) $ concatMap getEventsFromTx txs
   in if Map.null addrCounts
        then Nothing
        else Just $ AddressCountEvent addrCounts
  where
    getEventsFromTx :: C.Tx era -> [(C.AddressAny, Int)]
    getEventsFromTx (C.Tx (C.TxBody C.TxBodyContent{..}) _) =
      fmap (\(C.TxOut (C.AddressInEra _ addr) _ _ _) -> (C.toAddressAny addr, 1)) txOuts

{- | Query the in-memory indexer

TODO The 'Point AddressCountEvent' param is not used in this query yet.
That parameter is necessary in the scenario where there is more than one indexer and each run at different speeds.
Marconi uses that parameter to make sure that the result are consistent accross the different query calls.
-}
instance (Monad m) => Queryable m AddressCountEvent AddressCountQuery ListIndexer where
  query
    :: Point AddressCountEvent -- give me what you know up to this point, potentially a point in future
    -> AddressCountQuery
    -> ListIndexer AddressCountEvent -- get the point for ListIndexer
    -> m (Result AddressCountQuery)
  query C.ChainPointAtGenesis _ _ = pure 0
  query _ (AddressCountQuery addr) lsIndexer = do
    pure $
      sumOf
        ( events
            . traverse
            . event
            . to unAddressCountEvent
            . at addr
            . folded
        )
        lsIndexer

-- * SQLite indexer

-- | Query the SQLite indexer.
instance (MonadIO m) => Queryable m AddressCountEvent AddressCountQuery SQLiteIndexer where
  query
    :: Point AddressCountEvent -- give me what you know up to this point, potentially a point in future
    -> AddressCountQuery
    -> SQLiteIndexer AddressCountEvent -- get the point for ListIndexer
    -> m (Result AddressCountQuery)
  query C.ChainPointAtGenesis _ _ = pure 0
  query _ (AddressCountQuery addr) sqliteIndexer = do
    (results :: [[Int]]) <-
      liftIO $
        SQL.query
          (sqliteIndexer ^. connection)
          [sql|SELECT count FROM address_count
               WHERE address = ?|]
          (SQL.Only addr)
    pure $ sum $ concat results

data AddressCountRow = AddressCountRow
  { addressCountRowAddr :: !C.AddressAny
  , addressCountRowCount :: !Int
  }
  deriving (Generic, SQL.ToRow)

instance SQL.ToRow (Timed C.ChainPoint AddressCountRow) where
  toRow u =
    let (AddressCountRow addr count) = u ^. event
        (snoField, bhhField) = case u ^. point of
          C.ChainPointAtGenesis -> (SQL.SQLNull, SQL.SQLNull)
          (C.ChainPoint sno bhh) -> (SQL.toField sno, SQL.toField bhh)
     in SQL.toRow
          ( SQL.toField addr
          , SQL.toField count
          , snoField
          , bhhField
          )

eventToRows :: Timed C.ChainPoint AddressCountEvent -> [Timed C.ChainPoint AddressCountRow]
eventToRows te =
  let addrCounts = Map.toList $ unAddressCountEvent $ te ^. event
      cp = te ^. point
   in case cp of
        C.ChainPointAtGenesis ->
          []
        C.ChainPoint _ _ ->
          fmap (\(addr, count) -> Timed cp (AddressCountRow addr count)) addrCounts

mkAddressCountSqliteIndexer
  :: (MonadIO m, MonadError IndexerError m)
  => SQLiteDBLocation
  -> m (SQLiteIndexer AddressCountEvent)
mkAddressCountSqliteIndexer dbPath = do
  mkSyncedSqliteIndexer
    dbPath
    [dbCreation] -- request launched when the indexer is created
    [
      [ SQLInsertPlan (defaultInsertPlan eventToRows addressCountInsertQuery)
      ]
    ] -- requests launched when an event is stored
    [SQLRollbackPlan (Core.defaultRollbackPlan "address_count" "slotNo" C.chainPointToSlotNo)]
  where
    dbCreation =
      [sql|CREATE TABLE IF NOT EXISTS address_count
                 ( address BLOB NOT NULL
                 , count INT NOT NULL
                 , slotNo INT NOT NULL
                 , blockHeaderHash BLOB
                 )|]
    addressCountInsertQuery :: SQL.Query -- AddressCount table SQL statement
    addressCountInsertQuery =
      [sql|INSERT
               INTO address_count (
                 address,
                 count,
                 slotNo,
                 blockHeaderHash
              ) VALUES (?, ?, ?, ?)|]

-- | Make a SQLiteIndexer
mkAddressCountMixedIndexer
  :: (MonadIO m, MonadError IndexerError m)
  => SQLiteDBLocation
  -- ^ SQLite DB path
  -> SecurityParam
  -- ^ We use securityParam to set the Indexers flush sise to database
  -> m (MixedIndexer SQLiteIndexer ListIndexer AddressCountEvent)
mkAddressCountMixedIndexer dbPath (SecurityParam w64) =
  let keepInMemory = fromIntegral w64 -- how much to keep in memory to minimize disk rollbacks
      flushSize = keepInMemory `div` 6 -- how often to flush to disk
   in mkAddressCountMixedIndexer' dbPath keepInMemory flushSize

-- | Make a SQLiteIndexer
mkAddressCountMixedIndexer'
  :: (MonadIO m, MonadError IndexerError m)
  => SQLiteDBLocation
  -- ^ SQLite DB path
  -> Word
  -- ^  events keept in memory post flush
  -> Word
  -- ^ flush size
  -> m (MixedIndexer SQLiteIndexer ListIndexer AddressCountEvent)
mkAddressCountMixedIndexer' dbPath keep flush = do
  sqliteIndexer <- mkAddressCountSqliteIndexer dbPath
  pure $ mkMixedIndexer keep flush sqliteIndexer mkListIndexer
