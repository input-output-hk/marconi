{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Provide standard query and helper functions to track the last sync points of an indexer
module Marconi.ChainIndex.Experimental.Indexers.SyncHelper (
  syncTableCreation,
  syncSizeLimitTrigger,
  withSyncTable,
  syncInsertPlan,
  syncLastPointQuery,
  syncHistoryQuery,
  syncRollbackPlan,
) where

import Cardano.Api qualified as C
import Data.Text qualified as Text
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Core.Experiment qualified as Core

-- | A simple table to store the passed chainpoints
syncTableCreation :: SQL.Query
syncTableCreation =
  [sql|CREATE TABLE IF NOT EXISTS sync (slotNo INT PRIMARY KEY, blockHeaderHash BLOB)|]

{- | Used to keep only the @securityParam + 1@ most recent rows of the sync table.
Do not use if some of your request relies on data from the sync table
-}
syncSizeLimitTrigger :: Word64 -> SQL.Query
syncSizeLimitTrigger securityParam =
  [sql| CREATE TRIGGER IF NOT EXISTS clear_sync
        AFTER INSERT ON sync
        WHEN (SELECT COUNT(*) FROM sync) > |]
    <> SQL.Query (Text.pack $ show $ securityParam + 1)
    <> [sql| BEGIN
          DELETE FROM sync
          WHERE slotNo = (SELECT MIN(slotNo) FROM sync);
        END|]

withSyncTable :: Word64 -> [SQL.Query] -> [SQL.Query]
withSyncTable syncTableSize = mappend [syncTableCreation, syncSizeLimitTrigger syncTableSize]

{- | Used to insert data in the snc table.
The parameter order is @slotNo@, @blockHeaderHash@.
This should not be used in general, as 'syncInsertPlan' provides a higher level function to
handle insertion of data.
-}
syncInsertPlan :: Core.InsertPointQuery
syncInsertPlan =
  Core.InsertPointQuery [sql|INSERT INTO sync (slotNo, blockHeaderHash) VALUES (?, ?)|]

-- | Query for the last point of an indexer
syncLastPointQuery :: Core.GetLastSyncQuery
syncLastPointQuery = Core.GetLastSyncQuery $ syncHistoryQuery 1

{- | Query the last sync point up to the first immutable event

Used to find resuming points on restart.

Note that if you want to get one stable point, you need to pass @securityParam + 1@
-}
syncHistoryQuery :: SecurityParam -> SQL.Query
syncHistoryQuery securityParam =
  [sql|SELECT * FROM sync ORDER BY slotNo DESC LIMIT |]
    <> SQL.Query (Text.pack $ show securityParam)

-- | Standard rollback plan for the sync table
syncRollbackPlan :: Core.SQLRollbackPlan C.ChainPoint
syncRollbackPlan = Core.SQLRollbackPlan "sync" "slotNo" C.chainPointToSlotNo
