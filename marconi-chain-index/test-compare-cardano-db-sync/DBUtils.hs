{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DBUtils where

import Control.Concurrent.STM.TMChan ()
import Control.Exception (handle)
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple ()
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromRow ()
import Database.PostgreSQL.Simple.ToRow ()
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.FilePath (combine)

import Hedgehog qualified

data DbConnection = DbConnection
  { _dbcSQLite :: SQLite.Connection
  , _dbcPG :: PG.Connection
  }
  deriving (Generic)

$(makeLenses ''DbConnection)

{- | Connect to cardano-db-sync postgres with password from
 DBSYNC_PG_URL, Postgres Connection URI, env variable, see section 34.1.1.2.Connection URIs, https://www.postgresql.org/docs/current/libpq-connect.html
-}
getDbSyncPgConnection :: Hedgehog.PropertyT IO PG.Connection
getDbSyncPgConnection = do
  url <- envOrFail "DBSYNC_PG_URL"
  liftIO $
    maybe
      (fail "Failed parsing Postgres Connection URL")
      PG.connect
      (parseDatabaseUrl url)

getSQLiteConnection :: Hedgehog.PropertyT IO SQLite.Connection
getSQLiteConnection = do
  path <- flip combine "utxo.db" <$> envOrFail "MARCONI_DB_DIRECTORY_PATH"
  liftIO $
    handle
      (\(e :: SQLite.SQLError) -> fail (show e))
      ( do
          c <- SQLite.open path
          SQLite.execute_ c "PRAGMA journal_mode=WAL"
          pure c
      )

mkDbConnection :: Hedgehog.PropertyT IO DbConnection
mkDbConnection = do
  s <- getSQLiteConnection
  p <- getDbSyncPgConnection
  pure $ DbConnection s p

-- | Get string from the environment or fail test with instruction.
envOrFail :: String -> Hedgehog.PropertyT IO String
envOrFail str =
  liftIO $
    lookupEnv str >>= \case
      Just v -> return v
      Nothing -> fail $ str <> " environment variable not set!"
