{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | SQlite helpers to manage the last sync table
module Marconi.ChainIndex.Indexers.LastSync (
  createLastSyncTable,
  updateLastSyncTable,
  queryLastSyncPoint,
) where

import Cardano.Api qualified as C
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
    ( id INT PRIMARY KEY CHECK (id = 1)
    , blockHeaderHash BLOB NOT NULL
    , slotNo INT NOT NULL
    )|]

-- | update the last sync point of the indexer
updateLastSyncTable :: SQL.Connection -> C.ChainPoint -> IO ()
updateLastSyncTable c (C.ChainPoint slotNo blockHeader) =
  SQL.execute
    c
    [sql|REPLACE INTO lastSync VALUES (1, ?, ?)|]
    (blockHeader, slotNo)
updateLastSyncTable c C.ChainPointAtGenesis =
  SQL.execute_
    c
    [sql|DELETE FROM lastSync|]

-- | get the last sync point of the indexer
queryLastSyncPoint :: SQL.Connection -> IO C.ChainPoint
queryLastSyncPoint c =
  chainPointOrGenesis <$> SQL.query_ c "SELECT slotNo, blockHeaderHash FROM lastSync"
