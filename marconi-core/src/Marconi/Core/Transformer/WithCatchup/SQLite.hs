{-# LANGUAGE QuasiQuotes #-}

module Marconi.Core.Transformer.WithCatchup.SQLite (
  createIndexTable,
) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace (logInfo)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- | Utility function for creating an SQLite database index.

Typically used with 'Marconi.Core.Tranformer.WithCatchup' as a way to create SQLite indexes when
fully synchronized. This function includes some logging so that the user is informed when the
creation of the SQLite indexes is done.

An example would look like:

@
  let stdoutTrace = ...
      c = ...
      slotNoIndexName = "utxo_slotNo"
      createSlotNoIndexStatement =
        "CREATE INDEX IF NOT EXISTS "
          <> fromString slotNoIndexName
          <> " ON utxo (slotNo)"
  createIndexTable "Utxo" stdoutTrace c slotNoIndexName createSlotNoIndexStatement
@
-}
createIndexTable
  :: Text
  -- ^ Pretty name of the indexer
  -> Trace IO Text
  -- ^ Trace for logging.
  -> SQL.Connection
  -- ^ SQLite database connection
  -> String
  -- ^ SQLite index name
  -> SQL.Query
  -- ^ SQL statement for creating the index table
  -> IO ()
createIndexTable indexerName stdoutTrace c indexName createIndexStatement = do
  (doesIndexExist :: Bool) <-
    liftIO $
      fmap ((> (0 :: Int)) . sum . concat) $
        SQL.query_
          c
          ( [sql|SELECT COUNT(*)
                 FROM sqlite_master
                 WHERE type='index' AND name='|]
              <> fromString indexName
              <> "'"
          )
  unless doesIndexExist $ do
    logInfo stdoutTrace $
      "Creating SQL index table '"
        <> Text.pack indexName
        <> "' for "
        <> indexerName
        <> " indexer..."
    liftIO $ SQL.execute_ c createIndexStatement
    logInfo stdoutTrace $
      "Finished creating SQL index table '"
        <> Text.pack indexName
        <> "' for "
        <> indexerName
        <> " indexer..."
