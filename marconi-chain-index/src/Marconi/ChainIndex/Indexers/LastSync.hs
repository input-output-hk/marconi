{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | SQlite helpers to manage the last sync table
module Marconi.ChainIndex.Indexers.LastSync (
  createLastSyncTable,
  insertLastSyncPoints,
  queryLastSyncPoint,
  rollbackLastSyncPoints,
) where

import Cardano.Api qualified as C
import Data.Maybe (mapMaybe)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Utils (
  chainPointOrGenesis,
 )

-- |  create a table with a single row, to store the last sync point of the indexer
createLastSyncTable :: SQL.Connection -> IO ()
createLastSyncTable c =
  SQL.execute_
    c
    [sql|CREATE TABLE IF NOT EXISTS lastSync
    ( slotNo INT PRIMARY KEY
    , blockHeaderHash BLOB NOT NULL
    )|]

-- | update the last sync point of the indexer
insertLastSyncPoints :: SQL.Connection -> [C.ChainPoint] -> IO ()
insertLastSyncPoints c chainPoints = do
  let notGenesis C.ChainPointAtGenesis = Nothing
      notGenesis (C.ChainPoint slotNo blockHeader) = Just (slotNo, blockHeader)
  deleteAllLastSyncPoints c
  SQL.executeMany c [sql|INSERT INTO lastSync VALUES (?, ?)|] (mapMaybe notGenesis chainPoints)

-- | get the last sync point of the indexer
queryLastSyncPoint :: SQL.Connection -> IO C.ChainPoint
queryLastSyncPoint c =
  chainPointOrGenesis
    <$> SQL.query_
      c
      [sql|SELECT slotNo, blockHeaderHash FROM lastSync ORDER BY slotNo DESC|]

-- | Remove chainpoints that are after the rollback
rollbackLastSyncPoints :: SQL.Connection -> C.ChainPoint -> IO ()
rollbackLastSyncPoints c C.ChainPointAtGenesis = deleteAllLastSyncPoints c
rollbackLastSyncPoints c (C.ChainPoint sno _) =
  SQL.execute c [sql|DELETE FROM lastSync WHERE slotNo > ?|] (SQL.Only sno)

deleteAllLastSyncPoints :: SQL.Connection -> IO ()
deleteAllLastSyncPoints c = SQL.execute_ c [sql|DELETE FROM lastSync|]
