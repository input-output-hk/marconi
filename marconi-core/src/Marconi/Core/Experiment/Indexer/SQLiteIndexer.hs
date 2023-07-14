{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    On-disk indexer backed by a sqlite database.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Indexer.SQLiteIndexer (
  SQLiteIndexer (SQLiteIndexer),
  handle,
  insertPlan,
  dbLastSync,
  mkSqliteIndexer,
  mkSingleInsertSqliteIndexer,
  rollbackSQLiteIndexerWith,
  querySQLiteIndexerWith,
  querySyncedOnlySQLiteIndexerWith,
  handleSQLErrors,
  SQLInsertPlan (SQLInsertPlan, planInsert, planExtractor),
) where

import Control.Concurrent.Async qualified as Async
import Control.Exception (catch)
import Control.Lens (makeLenses)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQL

import Data.Foldable (Foldable (toList), traverse_)
import Data.Maybe (catMaybes)
import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending),
  IsSync (lastSyncPoint),
  indexIfJust,
 )
import Marconi.Core.Experiment.Type (
  IndexerError (IndexerInternalError, InvalidIndexer),
  Point,
  QueryError (AheadOfLastSync),
  Result,
  Timed,
  point,
 )

-- | A 'SQLInsertPlan' provides a piece information about how an event should be inserted in the database
data SQLInsertPlan event = forall a.
  (SQL.ToRow a) =>
  SQLInsertPlan
  { planExtractor :: Timed (Point event) event -> [a]
  -- ^ How to transform the event into a type that can be handle by the database
  , planInsert :: SQL.Query
  -- ^ The insert statement for the extracted data
  }

-- | Provide the minimal elements required to use a SQLite database to back an indexer.
data SQLiteIndexer event = SQLiteIndexer
  { _handle :: SQL.Connection
  -- ^ The connection used to interact with the database
  , _insertPlan :: [[SQLInsertPlan event]]
  -- ^ A plan is a list of lists : each 'SQLInsertPlan' in a list is executed concurrently.
  -- The different @[SQLInsertPlan]@ are executed in sequence.
  , _dbLastSync :: Point event
  -- ^ We keep the sync point in memory to avoid an SQL to retrieve it
  }

makeLenses 'SQLiteIndexer

{- | Start a new indexer or resume an existing SQLite indexer

 The main difference with 'SQLiteIndexer' is
 that we set 'dbLastSync' thanks to the provided query
-}
mkSqliteIndexer
  :: (MonadIO m)
  => (MonadError IndexerError m)
  => (HasGenesis (Point event))
  => (SQL.FromRow (Point event))
  => SQL.Connection
  -> [[SQLInsertPlan event]]
  -- ^ extract @param@ out of a 'Timed'
  -> SQL.Query
  -- ^ the lastSyncQuery
  -> m (SQLiteIndexer event)
mkSqliteIndexer _handle _insertPlan lastSyncQuery =
  let getLastSync = do
        res <- runLastSyncQuery _handle lastSyncQuery
        case res of
          [] -> pure genesis
          [x] -> pure x
          _other -> throwError (InvalidIndexer "Ambiguous sync point")
   in do
        _dbLastSync <- getLastSync
        pure $
          SQLiteIndexer
            { _handle
            , _insertPlan
            , _dbLastSync
            }

{- | A smart constructor for indexer that want to map an event to a single table.
 We just have to set the type family of `InsertRecord event` to `[param]` and
 then to provide the expected parameters.

 It is monomorphic restriction of 'mkSqliteIndexer'
-}
mkSingleInsertSqliteIndexer
  :: (MonadIO m)
  => (MonadError IndexerError m)
  => (SQL.FromRow (Point event))
  => (SQL.ToRow param)
  => (HasGenesis (Point event))
  => SQL.Connection
  -> (Timed (Point event) event -> param)
  -- ^ extract @param@ out of a 'Timed'
  -> SQL.Query
  -- ^ the insert query
  -> SQL.Query
  -- ^ the lastSyncQuery
  -> m (SQLiteIndexer event)
mkSingleInsertSqliteIndexer con extract insert = mkSqliteIndexer con [[SQLInsertPlan (pure . extract) insert]]

handleSQLErrors :: IO a -> IO (Either IndexerError a)
handleSQLErrors value =
  fmap Right value
    `catch` (\(x :: SQL.FormatError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
    `catch` (\(x :: SQL.ResultError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
    `catch` (\(x :: SQL.SQLError) -> pure . Left . IndexerInternalError . Text.pack $ show x)

-- | Run a list of insert queries in one single transaction.
runIndexQueriesStep
  :: (MonadIO m)
  => (MonadError IndexerError m)
  => SQL.Connection
  -> [Timed (Point event) event]
  -> [SQLInsertPlan event]
  -> m ()
runIndexQueriesStep _ _ [] = pure ()
runIndexQueriesStep c events xs =
  let runIndexQuery (SQLInsertPlan planExtractor planInsert) = do
        let rows = planExtractor =<< events
        case rows of
          [] -> pure ()
          [x] -> SQL.execute c planInsert x
          _nonEmpty -> SQL.executeMany c planInsert rows
   in either throwError pure <=< liftIO $
        handleSQLErrors (SQL.withTransaction c $ Async.mapConcurrently_ runIndexQuery xs)

-- | Run a list of insert queries in one single transaction.
runIndexQueries
  :: (MonadIO m)
  => (MonadError IndexerError m)
  => SQL.Connection
  -> [Timed (Point event) event]
  -> [[SQLInsertPlan event]]
  -> m ()
runIndexQueries c = traverse_ . runIndexQueriesStep c

runLastSyncQuery
  :: (MonadError IndexerError m)
  => (MonadIO m)
  => (SQL.FromRow r)
  => SQL.Connection
  -> SQL.Query
  -> m [r]
runLastSyncQuery connection lastSyncQuery =
  either throwError pure <=< liftIO $
    handleSQLErrors (SQL.query connection lastSyncQuery ())

instance
  (MonadIO m, MonadError IndexerError m)
  => IsIndex m event SQLiteIndexer
  where
  index =
    let addEvent e indexer = do
          runIndexQueries (indexer ^. handle) [e] (indexer ^. insertPlan)
          pure indexer
        setDbLastSync p indexer = pure $ indexer & dbLastSync .~ p
     in indexIfJust addEvent setDbLastSync

  indexAllDescending evts indexer = do
    let updateLastSync = case toList evts of
          [] -> id
          (x : _xs) -> dbLastSync .~ (x ^. point)
    runIndexQueries
      (indexer ^. handle)
      (catMaybes . toList $ sequence <$> evts)
      (indexer ^. insertPlan)
    pure $ updateLastSync indexer

instance (MonadIO m) => IsSync m event SQLiteIndexer where
  lastSyncPoint indexer =
    pure $ indexer ^. dbLastSync

instance (MonadIO m) => Closeable m SQLiteIndexer where
  close indexer = liftIO $ SQL.close $ indexer ^. handle

-- | A helper for the definition of the 'Rollbackable' typeclass for 'SQLiteIndexer'
rollbackSQLiteIndexerWith
  :: (MonadIO m, SQL.ToRow (Point event))
  => SQL.Query
  -- ^ The rollback statement
  -> Point event
  -- ^ Point will be passed as a parameter to the query
  -> SQLiteIndexer event
  -- ^ We're just using the connection
  -> m (SQLiteIndexer event)
rollbackSQLiteIndexerWith q p indexer = do
  let c = indexer ^. handle
  liftIO $
    SQL.withTransaction
      c
      (SQL.execute c q p)
  pure $ indexer & dbLastSync .~ p

{- | A helper for the definition of the 'Queryable' typeclass for 'SQLiteIndexer'

 The helper just remove a bit of the boilerplate needed to transform data
 to query the database.

 It doesn't contain any logic, except a check for 'AheadOfLastSync' error,
 in which case it throws the 'AheadOfLastSync' exception with a partial result.
 If you don't want to query the database on a partial result,
 use 'querySyncedOnlySQLiteIndexerWith'

 It doesn't filter the result based on the given data point.
-}
querySQLiteIndexerWith
  :: (MonadIO m)
  => (MonadError (QueryError query) m)
  => (Ord (Point event))
  => (SQL.FromRow r)
  => (Point event -> query -> [SQL.NamedParam])
  -> SQL.Query
  -- ^ The sqlite query statement
  -- ^ A preprocessing of the query, to obtain SQL parameters
  -> (query -> [r] -> Result query)
  -- ^ Post processing of the result, to obtain the final result
  -> Point event
  -> query
  -> SQLiteIndexer event
  -> m (Result query)
querySQLiteIndexerWith toNamedParam sqlQuery fromRows p q indexer =
  do
    let c = indexer ^. handle
    res <- liftIO $ SQL.queryNamed c sqlQuery (toNamedParam p q)
    when (p < indexer ^. dbLastSync) $
      throwError (AheadOfLastSync $ Just $ fromRows q res)
    pure $ fromRows q res

{- | A helper for the definition of the 'Queryable' typeclass for 'SQLiteIndexer'

 The helper just remove a bit of the boilerplate needed to transform data
 to query the database.

 It doesn't contain any logic, except a check for 'AheadOfLastSync' error,
 in which case it throws the 'AheadOfLastSync' without any result attached.

 It doesn't filter the result based on the given data point.
-}
querySyncedOnlySQLiteIndexerWith
  :: (MonadIO m)
  => (MonadError (QueryError query) m)
  => (Ord (Point event))
  => (SQL.FromRow r)
  => (Point event -> query -> [SQL.NamedParam])
  -> SQL.Query
  -- ^ The sqlite query statement
  -- ^ A preprocessing of the query, to obtain SQL parameters
  -> (query -> [r] -> Result query)
  -- ^ Post processing of the result, to obtain the final result
  -> Point event
  -> query
  -> SQLiteIndexer event
  -> m (Result query)
querySyncedOnlySQLiteIndexerWith toNamedParam sqlQuery fromRows p q indexer =
  do
    let c = indexer ^. handle
    when (p < indexer ^. dbLastSync) $
      throwError (AheadOfLastSync Nothing)
    res <- liftIO $ SQL.queryNamed c sqlQuery (toNamedParam p q)
    pure $ fromRows q res
