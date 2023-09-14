{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    On-disk indexer backed by a sqlite database.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Indexer.SQLiteIndexer (
  SQLiteIndexer (SQLiteIndexer),
  databasePath,
  connection,
  insertPlan,
  InsertPointQuery (InsertPointQuery, getInsertPointQuery),
  GetLastSyncPointsQuery (
    GetLastSyncPointsQuery,
    getLastSyncPointsQuery
  ),
  mkSqliteIndexer,
  mkSingleInsertSqliteIndexer,
  querySQLiteIndexerWith,
  querySyncedOnlySQLiteIndexerWith,
  handleSQLErrors,
  lastSyncPointsQuery,
  dbLastSync,
  SQLInsertPlan (SQLInsertPlan, planInsert, planExtractor),
  SQLRollbackPlan (SQLRollbackPlan, tableName, pointName, pointExtractor),

  -- * Reexport from SQLite
  SQL.ToRow (..),
) where

import Control.Concurrent.Async qualified as Async
import Control.Exception (Handler (Handler), catches)
import Control.Lens (makeLenses)
import Control.Lens.Operators ((&), (.~), (^.), (^..))
import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (Foldable (toList), traverse_)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending, rollback),
  IsSync (lastSyncPoint, lastSyncPoints),
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

newtype InsertPointQuery = InsertPointQuery {getInsertPointQuery :: SQL.Query}

{- | A 'SQLRollbackPlan' provides a piece of information on how to perform a rollback on the data
 inserted in the database.
-}
data SQLRollbackPlan point = forall a.
  (SQL.ToField a) =>
  SQLRollbackPlan
  { tableName :: String
  -- ^ the table to rollback
  , pointName :: String
  -- ^ The name of the point field in the table
  , pointExtractor :: point -> Maybe a
  -- ^ How we transform the data to the point field. Returning 'Nothing' essentially means that we
  -- delete all information from the database. Returning 'Just a' means that we will delete all
  -- rows with a point higher than 'point'.
  }

-- | A newtype to get the 'n' last sync points from an indexer.
newtype GetLastSyncPointsQuery = GetLastSyncPointsQuery
  { getLastSyncPointsQuery :: Word -> SQL.Query
  }

-- | Provide the minimal elements required to use a SQLite database to back an indexer.
data SQLiteIndexer event = SQLiteIndexer
  { _databasePath :: FilePath
  -- ^ The location of the database
  , _connection :: SQL.Connection
  -- ^ The connection used to interact with the database
  , _insertPlan :: [[SQLInsertPlan event]]
  -- ^ A plan is a list of lists : each 'SQLInsertPlan' in a list is executed concurrently.
  -- The different @[SQLInsertPlan]@ are executed in sequence.
  , _insertPoint :: Maybe InsertPointQuery
  -- ^ The query to keep track of the indexed point
  , _rollbackPlan :: [SQLRollbackPlan (Point event)]
  -- ^ The list of tables we update on rollback, with the information required to update them
  , _lastSyncPointsQuery :: GetLastSyncPointsQuery
  -- ^ The SQL query to fetch the last sync points from the indexer.
  , _dbLastSync :: Point event
  -- ^ We keep the sync point in memory to avoid an SQL to retrieve it
  }

makeLenses ''SQLiteIndexer

{- | Start a new indexer or resume an existing SQLite indexer

 The main difference with 'SQLiteIndexer' is that we set 'dbLastSync' thanks to the provided query.
 It helps resuming an existing indexer.
-}
mkSqliteIndexer
  :: forall event m
   . ( MonadIO m
     , MonadError IndexerError m
     , HasGenesis (Point event)
     , SQL.FromRow (Point event)
     )
  => FilePath
  -> [SQL.Query]
  -- ^ cration statement
  -> [[SQLInsertPlan event]]
  -- ^ extract @param@ out of a 'Timed'
  -> Maybe InsertPointQuery
  -- ^ we may omit the insert point query if the SQLInsertPlan makes it redundant
  -> [SQLRollbackPlan (Point event)]
  -- ^ the rollbackQuery
  -> GetLastSyncPointsQuery
  -- ^ The SQL query to fetch the last sync points from the indexer.
  -> m (SQLiteIndexer event)
mkSqliteIndexer _databasePath _creationStatements _insertPlan _insertPoint _rollbackPlan _lastSyncPointsQuery =
  let getLastSyncPoint :: SQL.Connection -> m (Point event)
      getLastSyncPoint h = do
        res <-
          runLastSyncPointsQuery
            h
            _lastSyncPointsQuery
            1
        pure $ maybe genesis NonEmpty.last $ NonEmpty.nonEmpty res
   in do
        _connection <- liftIO $ SQL.open _databasePath -- TODO clean exception on invalid file
        traverse_ (liftIO . SQL.execute_ _connection) _creationStatements
        -- allow for concurrent insert/query.
        -- see SQLite WAL, https://www.sqlite.org/wal.html
        liftIO $ SQL.execute_ _connection "PRAGMA journal_mode=WAL"
        _dbLastSync <- getLastSyncPoint _connection
        pure $
          SQLiteIndexer
            { _databasePath
            , _connection
            , _insertPlan
            , _insertPoint
            , _rollbackPlan
            , _lastSyncPointsQuery
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
  => FilePath
  -> (Timed (Point event) event -> param)
  -- ^ extract @param@ out of a 'Timed'
  -> SQL.Query
  -- ^ the creation query
  -> SQL.Query
  -- ^ the insert query
  -> SQLRollbackPlan (Point event)
  -- ^ the rollback query
  -> GetLastSyncPointsQuery
  -- ^ The SQL query to fetch the last sync points from the indexer.
  -> m (SQLiteIndexer event)
mkSingleInsertSqliteIndexer path extract create insert rollback' =
  mkSqliteIndexer path [create] [[SQLInsertPlan (pure . extract) insert]] Nothing [rollback']

-- | Map SQLite errors to an indexer error
handleSQLErrors :: IO a -> IO (Either IndexerError a)
handleSQLErrors value =
  fmap Right value
    `catches` [ Handler (\(x :: SQL.FormatError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
              , Handler (\(x :: SQL.ResultError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
              , Handler (\(x :: SQL.SQLError) -> pure . Left . IndexerInternalError . Text.pack $ show x)
              ]

-- | Run a list of insert queries in one single transaction.
runIndexQueriesStep
  :: SQL.Connection
  -> [Timed (Point event) event]
  -> [SQLInsertPlan event]
  -> IO ()
runIndexQueriesStep _ _ [] = pure ()
runIndexQueriesStep c events plan =
  let runIndexQuery (SQLInsertPlan planExtractor planInsert) = do
        let rows = planExtractor =<< events
        case rows of
          [] -> pure ()
          [x] -> SQL.execute c planInsert x
          _nonEmpty -> SQL.executeMany c planInsert rows
   in Async.mapConcurrently_ runIndexQuery plan

-- | Run a list of insert queries in one single transaction.
runIndexPlan
  :: SQL.Connection
  -> [Timed (Point event) event]
  -> [[SQLInsertPlan event]]
  -> IO ()
runIndexPlan c = traverse_ . runIndexQueriesStep c

runInsertPoint
  :: (SQL.ToRow point)
  => SQL.Connection
  -> [point]
  -> InsertPointQuery
  -> IO ()
runInsertPoint c points (InsertPointQuery insertPoint') =
  SQL.executeMany c insertPoint' points

-- | Run a list of insert queries in one single transaction.
runIndexQueries
  :: (MonadIO m, MonadError IndexerError m, SQL.ToRow (Point event))
  => SQL.Connection
  -> [Timed (Point event) (Maybe event)]
  -> [[SQLInsertPlan event]]
  -> Maybe InsertPointQuery
  -> m ()
runIndexQueries c events' plan insertPoint' =
  let nonEmptyEvents = (catMaybes . toList $ sequence <$> events')
      indexEvent = case nonEmptyEvents of
        [] -> Nothing
        _nonEmpty -> Just $ runIndexPlan c nonEmptyEvents plan
      indexPoints = runInsertPoint c (events' ^.. traverse . point) <$> insertPoint'
      allIndex = case (indexEvent, indexPoints) of
        (Nothing, Nothing) -> Nothing
        (Just rPlan, Nothing) -> Just rPlan
        (Nothing, Just rPoint) -> Just rPoint
        (Just rPlan, Just rPoint) -> Just $ Async.concurrently_ rPlan rPoint
   in case allIndex of
        Nothing -> pure ()
        Just runIndexers ->
          let
           in either throwError pure <=< liftIO $
                handleSQLErrors (SQL.withTransaction c runIndexers)

indexEvents
  :: (MonadIO m, MonadError IndexerError m, SQL.ToRow (Point event))
  => [Timed (Point event) (Maybe event)]
  -> SQLiteIndexer event
  -> m (SQLiteIndexer event)
indexEvents [] indexer = pure indexer
indexEvents evts@(e : _) indexer = do
  let setDbLastSync p = pure $ indexer & dbLastSync .~ p
  runIndexQueries (indexer ^. connection) evts (indexer ^. insertPlan) (indexer ^. insertPoint)
  setDbLastSync (e ^. point)

runLastSyncPointsQuery
  :: (MonadError IndexerError m, MonadIO m, SQL.FromRow r)
  => SQL.Connection
  -> GetLastSyncPointsQuery
  -> Word
  -> m [r]
runLastSyncPointsQuery conn (GetLastSyncPointsQuery mkQuery) n =
  either throwError pure <=< liftIO $
    handleSQLErrors (SQL.query_ conn (mkQuery n))

instance
  (MonadIO m, MonadError IndexerError m, SQL.ToRow (Point event))
  => IsIndex m event SQLiteIndexer
  where
  index = indexEvents . pure

  indexAllDescending = indexEvents . toList

  rollback p indexer = do
    let c = indexer ^. connection
        deleteAllQuery tName = "DELETE FROM " <> tName
        deleteAll = SQL.execute_ c . deleteAllQuery . SQL.Query . Text.pack
        deleteUntilQuery tName pName =
          deleteAllQuery tName <> " WHERE " <> pName <> " > :point"
        deleteUntil :: (SQL.ToField a) => String -> String -> a -> IO ()
        deleteUntil tName pName pt =
          SQL.executeNamed
            c
            (deleteUntilQuery (SQL.Query $ Text.pack tName) (SQL.Query $ Text.pack pName))
            [":point" SQL.:= pt]
        rollbackTable (SQLRollbackPlan tableName pointName extractor) =
          case extractor p of
            Nothing -> deleteAll tableName
            Just pt -> deleteUntil tableName pointName pt
    liftIO $
      SQL.withTransaction c $
        traverse_ rollbackTable (indexer ^. rollbackPlan)
    pure $ indexer & dbLastSync .~ p

instance
  (MonadIO m, SQL.FromRow (Point event))
  => IsSync m event SQLiteIndexer
  where
  lastSyncPoint indexer =
    pure $ indexer ^. dbLastSync

  lastSyncPoints :: Word -> SQLiteIndexer event -> m [Point event]
  lastSyncPoints n indexer = do
    let getLastSyncPointsQuery = indexer ^. lastSyncPointsQuery
    resultE <- runExceptT $ runLastSyncPointsQuery (indexer ^. connection) getLastSyncPointsQuery n
    case resultE of
      -- TODO Revisit this. Should probably throw error. However, can't just add constraint
      -- 'MonadError IndexerError event' because then it conflicts with other functions which have
      -- 'MonadError (QueryError query) event)'
      Left _err -> List.singleton <$> lastSyncPoint indexer -- throwError $ OtherIndexError ""
      Right v -> pure v

instance (MonadIO m) => Closeable m SQLiteIndexer where
  close indexer = liftIO $ SQL.close $ indexer ^. connection

{- | A helper for the definition of the @Queryable@ typeclass for 'SQLiteIndexer'

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
  -- ^ A preprocessing of the query, to obtain SQL parameters
  -> (query -> SQL.Query)
  -- ^ The sqlite query statement
  -> (query -> [r] -> Result query)
  -- ^ Post processing of the result, to obtain the final result
  -> Point event
  -> query
  -> SQLiteIndexer event
  -> m (Result query)
querySQLiteIndexerWith toNamedParam sqlQuery fromRows p q indexer =
  do
    let c = indexer ^. connection
    res <- liftIO $ SQL.queryNamed c (sqlQuery q) (toNamedParam p q)
    when (p > indexer ^. dbLastSync) $
      throwError (AheadOfLastSync $ Just $ fromRows q res)
    pure $ fromRows q res

{- | A helper for the definition of the 'Queryable' typeclass for 'SQLiteIndexer'.

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
  -- ^ A preprocessing of the query, to obtain SQL parameters
  -> (query -> SQL.Query)
  -- ^ The sqlite query statement
  -> (query -> [r] -> Result query)
  -- ^ Post processing of the result, to obtain the final result
  -> Point event
  -> query
  -> SQLiteIndexer event
  -> m (Result query)
querySyncedOnlySQLiteIndexerWith toNamedParam sqlQuery fromRows p q indexer =
  do
    let c = indexer ^. connection
    when (p > indexer ^. dbLastSync) $
      throwError (AheadOfLastSync Nothing)
    res <- liftIO $ SQL.queryNamed c (sqlQuery q) (toNamedParam p q)
    pure $ fromRows q res
