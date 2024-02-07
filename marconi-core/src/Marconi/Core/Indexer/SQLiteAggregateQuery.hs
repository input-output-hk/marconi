{-# LANGUAGE ConstraintKinds #-}

{- |
    A @SQLiteAggregateQuery@ provides a way to query the content of several @SQLiteIndexer@s
    as if they were a single indexer.

    Note that @SQLiteAggregateQuery doesn't implement @IsIndex@: it won't index anything,
    and won't pass any information to the indexers it relies on.
    It's sole purpose is to provide a unified access for queries.
-}
module Marconi.Core.Indexer.SQLiteAggregateQuery (
  mkSQLiteAggregateQuery,
  SQLiteSourceProvider (..),
  IsSourceProvider,
  SQLiteAggregateQuery (..),
  aggregateConnection,
  Pool.withResource,
  HasDatabasePath (..),
) where

import Control.Concurrent qualified as Con
import Control.Exception (throw)
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Marconi.Core.Class (
  Closeable (close),
  IsSync (lastStablePoint, lastSyncPoint),
 )
import Marconi.Core.Indexer.SQLiteIndexer (SQLiteIndexer)
import Marconi.Core.Indexer.SQLiteIndexer qualified as SQLite
import Marconi.Core.Type (Point)

-- | A class for indexer that has access to a SQLite database and know the path to this database
class HasDatabasePath indexer where
  -- | Retrieve the database path from the indexer
  getDatabasePath :: indexer event -> SQLite.SQLiteDBLocation

instance HasDatabasePath SQLiteIndexer where
  getDatabasePath = Lens.view SQLite.databasePath

-- | Alias to gather typeclasses required to be a source provider
type IsSourceProvider m event indexer = (IsSync m event indexer, HasDatabasePath indexer)

{- | A wrapper around indexers.

Its purpose is mainly to allow the use of a heterogenerous lists of indexers as a source for a
@SQLiteAggregateQuery@.
-}
data SQLiteSourceProvider m point
  = forall indexer event.
    (IsSourceProvider m event indexer, Point event ~ point) =>
    SQLiteSourceProvider (Con.MVar (indexer event))

-- | An aggregation of SQLite indexers used to build query across indexers.
data SQLiteAggregateQuery m point event = SQLiteAggregateQuery
  { _databases :: [SQLiteSourceProvider m point]
  -- ^ The indexers that provides database access to this query
  , _aggregateConnection :: Pool SQL.Connection
  -- ^ The connection that provides a read access accross the different databases
  }

-- | The indexers that provides database access to this query, wrapeed in a @SQLiteSourceProvider@
databases :: Lens.Lens' (SQLiteAggregateQuery m point event) [SQLiteSourceProvider m point]
databases = Lens.lens _databases (\agg _databases -> agg{_databases})

-- | The connection that provides a read access accross the different databases
aggregateConnection :: Lens.Lens' (SQLiteAggregateQuery m point event) (Pool SQL.Connection)
aggregateConnection =
  Lens.lens _aggregateConnection (\agg _aggregateConnection -> agg{_aggregateConnection})

{- | Build a @SQLiteSourceProvider@ from a map that attaches
each database of the provided sources to the corresponding alias
-}
mkSQLiteAggregateQuery
  :: Map String (SQLiteSourceProvider m point)
  -- ^ A map of indexers. The keys are used as an alias int the attach statement
  -> IO (SQLiteAggregateQuery m point event)
mkSQLiteAggregateQuery sources = do
  let expectPersistentDB (SQLite.Storage path) = path
      expectPersistentDB SQLite.Memory = throw SQLite.ExpectedPersistentDB
      databaseFromSource :: SQLiteSourceProvider m point -> IO FilePath
      databaseFromSource (SQLiteSourceProvider ix) = Con.withMVar ix (pure . expectPersistentDB . getDatabasePath)
      attachDb con name src =
        liftIO $ do
          databasePath <- databaseFromSource src
          SQL.executeNamed
            con
            "ATTACH DATABASE :path AS :name"
            [":path" := databasePath, ":name" := name]
      createConnection = do
        con <- SQLite.readOnlyConnection SQLite.inMemoryDB
        void $ Map.traverseWithKey (attachDb con) sources
        pure con
  let tenHours = 36000
      poolSize = 100
      poolConfig =
        Pool.defaultPoolConfig
          createConnection
          SQL.close
          tenHours
          poolSize
  pool <- Pool.newPool poolConfig
  pure $ SQLiteAggregateQuery (Map.elems sources) pool

instance (MonadIO m) => Closeable m (SQLiteAggregateQuery m point) where
  close indexer = liftIO $ Pool.destroyAllResources $ indexer ^. aggregateConnection

instance
  (MonadIO m, MonadTrans t, Monad (t m), Ord point, point ~ Point event)
  => IsSync (t m) event (SQLiteAggregateQuery m point)
  where
  lastSyncPoint query@(SQLiteAggregateQuery _ _) =
    let getPoint :: SQLiteSourceProvider m point -> m point
        getPoint (SQLiteSourceProvider ix) = do
          ix' <- liftIO $ Con.readMVar ix
          lastSyncPoint ix'
     in lift . fmap minimum . traverse getPoint . Lens.view databases $ query

  lastStablePoint query@(SQLiteAggregateQuery _ _) =
    let getPoint :: SQLiteSourceProvider m point -> m point
        getPoint (SQLiteSourceProvider ix) = do
          ix' <- liftIO $ Con.readMVar ix
          lastStablePoint ix'
     in lift . fmap minimum . traverse getPoint . Lens.view databases $ query

instance
  (MonadIO m, Ord point, point ~ Point event)
  => IsSync m event (SQLiteAggregateQuery m point)
  where
  lastSyncPoint =
    let getPoint :: SQLiteSourceProvider m point -> m point
        getPoint (SQLiteSourceProvider ix) = do
          ix' <- liftIO $ Con.readMVar ix
          lastSyncPoint ix'
     in fmap minimum . traverse getPoint . Lens.view databases

  lastStablePoint =
    let getPoint :: SQLiteSourceProvider m point -> m point
        getPoint (SQLiteSourceProvider ix) = do
          ix' <- liftIO $ Con.readMVar ix
          lastStablePoint ix'
     in fmap minimum . traverse getPoint . Lens.view databases
