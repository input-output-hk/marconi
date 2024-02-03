{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Provide standard query and helper functions to track the last sync points of an indexer
module Marconi.Cardano.Indexers.SyncHelper (
  syncTableCreation,
  syncSetStablePoint,
  syncLastPointQuery,
  syncHistoryQuery,
  mkSingleInsertSyncedSqliteIndexer,
  mkSyncedSqliteIndexer,
) where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer qualified as SQL

{- | A simple table to store the last stable point of an indexer.

Note that in Byron era, there can be multiple added blocks in the same slot. That explains why we
can't use the `PRIMARY KEY` constraint for `slotNo` only.
-}
syncTableCreation :: SQL.Query
syncTableCreation =
  [sql|CREATE TABLE IF NOT EXISTS sync
  ( lock TEXT NOT NULL DEFAULT 'there should be only one row'
  , slotNo INT NOT NULL
  , blockHeaderHash BLOB NOT NULL
  , CONSTRAINT sync_pk PRIMARY KEY (lock)
  , CONSTRAINT sync_lock CHECK (lock='there should be only one row')
  )|]

{- | Used to insert data in the snc table.
The parameter order is @slotNo@, @blockHeaderHash@.
This should not be used in general, as 'syncInsertPlan' provides a higher level function to
handle insertion of data.

  The query ensure that we only replace the content on a greater slot.

  Considering the issue of the blocks issued at the same slots during the Byron era, there is very
  few chance that the current implementation would lead to an issue. The problematic scenario would
  be:

  1. The indexer must index an event that in the second block of the two issue at the same slot at
  the end of an epoch of the Byron era.
  2. The indexer has stopped right after indexing this second block.
  3. On restart, another indexer is behind this second block.
-}
syncSetStablePoint :: Core.SetLastStablePointQuery
syncSetStablePoint =
  Core.SetLastStablePointQuery
    [sql|
      INSERT INTO sync (slotNo, blockHeaderHash) VALUES (?, ?)
      ON CONFLICT (lock)
      DO UPDATE SET
      slotNo = excluded.slotNo,
      blockHeaderHash = excluded.blockHeaderHash
      WHERE sync.slotNo < excluded.slotNo
    |]

-- TODO Revisit comment

-- | Query for the last point of an indexer
syncLastPointQuery :: Core.GetLastStablePointQuery
syncLastPointQuery = Core.GetLastStablePointQuery syncHistoryQuery

-- TODO Revisit comment

{- | Query the last sync point up to the first immutable event

Used to find resuming points on restart.
-}
syncHistoryQuery :: SQL.Query
syncHistoryQuery = [sql|SELECT slotNo, blockHeaderHash FROM sync ORDER BY slotNo|]

-- | A helper to create an indexer for Cardano for a single table, with an immutable point tracker
mkSingleInsertSyncedSqliteIndexer
  :: forall m event param
   . ( MonadIO m
     , MonadError Core.IndexerError m
     , SQL.ToRow param
     , Core.Point event ~ C.ChainPoint
     )
  => SQL.SQLiteDBLocation
  -> (Core.Timed (Core.Point event) event -> param)
  -> SQL.Query
  -- ^ the creation query
  -> SQL.Query
  -- ^ the insert query
  -> Core.SQLRollbackPlan (Core.Point event)
  -- ^ the rollback query
  -> m (Core.SQLiteIndexer event)
mkSingleInsertSyncedSqliteIndexer path extract tableCreation insertQuery rollbackPlan =
  Core.mkSqliteIndexer
    path
    [tableCreation, syncTableCreation]
    [[Core.SQLInsertPlan (SQL.defaultInsertPlan (pure . extract) insertQuery)]]
    [rollbackPlan]
    syncSetStablePoint
    syncLastPointQuery

-- | A helper to create an indexer for Cardano, with an immutable point tracker
mkSyncedSqliteIndexer
  :: forall m event
   . ( MonadIO m
     , MonadError Core.IndexerError m
     , Core.Point event ~ C.ChainPoint
     )
  => SQL.SQLiteDBLocation
  -> [SQL.Query]
  -- ^ cration statement
  -> [[Core.SQLInsertPlan event]]
  -- ^ extract relevent data out of an event and store them
  -> [Core.SQLRollbackPlan (Core.Point event)]
  -- ^ the rollback queries
  -> m (Core.SQLiteIndexer event)
mkSyncedSqliteIndexer path tablesCreation insertPlans rollbackPlans =
  Core.mkSqliteIndexer
    path
    (syncTableCreation : tablesCreation)
    insertPlans
    rollbackPlans
    syncSetStablePoint
    syncLastPointQuery
