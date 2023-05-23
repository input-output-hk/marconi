{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    On-disk indexer backed by a sqlite database.

    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Indexer.SQLiteIndexer
    ( SQLiteIndexer (SQLiteIndexer)
        , handle
        , prepareInsert
        , buildInsert
        , dbLastSync
    , sqliteIndexer
    , singleInsertSQLiteIndexer
    , rollbackSQLiteIndexerWith
    , querySQLiteIndexerWith
    , querySyncedOnlySQLiteIndexerWith
    , InsertRecord
    , IndexQuery (..)
    ) where

import Control.Concurrent.Async qualified as Async
import Control.Exception (catch)
import Control.Lens (folded, makeLenses, maximumOf)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQL

import Data.Foldable (traverse_)
import Marconi.Core.Experiment.Class (Closeable (close), HasGenesis (genesis), IsIndex (index, indexAll),
                                      IsSync (lastSyncPoint))
import Marconi.Core.Experiment.Type (IndexerError (IndexerInternalError, InvalidIndexer), Point,
                                     QueryError (AheadOfLastSync), Result, TimedEvent, point)

-- |
type family InsertRecord event

data IndexQuery
    = forall param. SQL.ToRow param
    => IndexQuery
        { insertQuery :: SQL.Query
        , params      :: [param]
         -- ^ It's a list because me want to be able to deal with bulk insert.
        }


-- | Provide the minimal elements required to use a SQLite database to back an indexer.
data SQLiteIndexer event
    = SQLiteIndexer
        { _handle        :: SQL.Connection
          -- ^ The connection used to interact with the database
        , _prepareInsert :: TimedEvent event -> InsertRecord event
          -- ^ 'InsertRecord' is the typed representation of what has to be inserted in the database
          -- It should be a monoid, to allow insertion of 0 to n rows in a single transaction.
          --
          -- A list is fine if you plan to perform a single insert to store your event.
          -- If you need several inserts, a record where each field correspond to a list is probably
          -- a good choice.
        , _buildInsert   :: InsertRecord event -> [[IndexQuery]]
          -- ^ Map the 'InsertRecord' representation to a list of 'IndexQuery',
          -- to actually performed the insertion in the database.
          -- One can think at the insert record as a typed representation of the parameters of the queries,
          --
          -- The return type is a list of list because each list will be run sequentialy.
        , _dbLastSync    :: Point event
          -- ^ We keep the sync point in memory to avoid an SQL to retrieve it
        }

makeLenses ''SQLiteIndexer

sqliteIndexer
    :: SQL.ToRow param
    => SQL.FromRow (Point event)
    => MonadIO m
    => MonadError IndexerError m
    => InsertRecord event ~ [param]
    => HasGenesis (Point event)
    => SQL.Connection
    -> (TimedEvent event -> [param])
    -- ^ extract @param@ out of a 'TimedEvent'
    -> SQL.Query
    -- ^ the insert query
    -> SQL.Query
    -- ^ the lastSyncQuery
    -> m (SQLiteIndexer event)
sqliteIndexer _handle _prepareInsert insertQuery lastSyncQuery
    = let

    getLastSync = do
        res <- runLastSyncQuery _handle lastSyncQuery
        case res of
            []     -> pure genesis
            [x]    -> pure x
            _other -> throwError (InvalidIndexer "Ambiguous sync point")

    in do
        _dbLastSync <- getLastSync
        pure $ SQLiteIndexer
            {_handle
            , _prepareInsert
            , _buildInsert = pure . pure . IndexQuery insertQuery
            , _dbLastSync
            }

singleInsertSQLiteIndexer
    :: SQL.ToRow param
    => SQL.FromRow (Point event)
    => MonadIO m
    => MonadError IndexerError m
    => InsertRecord event ~ [param]
    => HasGenesis (Point event)
    => SQL.Connection
    -> (TimedEvent event -> [param])
    -- ^ extract @param@ out of a 'TimedEvent'
    -> SQL.Query
    -- ^ the insert query
    -> SQL.Query
    -- ^ the lastSyncQuery
    -> m (SQLiteIndexer event)
singleInsertSQLiteIndexer = sqliteIndexer

handleSQLErrors :: IO a -> IO (Either IndexerError a)
handleSQLErrors value = fmap Right value
     `catch` (\(x :: SQL.FormatError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
     `catch` (\(x :: SQL.ResultError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
     `catch` (\(x :: SQL.SQLError) -> pure . Left . IndexerInternalError . Text.pack $ show x)


-- | Run a list of insert queries in one single transaction.
runIndexQueriesStep
    :: MonadIO m
    => MonadError IndexerError m
    => SQL.Connection -> [IndexQuery] -> m ()
runIndexQueriesStep _ [] = pure ()
runIndexQueriesStep c xs = let

    runIndexQuery (IndexQuery insertQuery params)
        = SQL.executeMany c insertQuery params

    in either throwError pure <=< liftIO
        $ handleSQLErrors (SQL.withTransaction c $ Async.mapConcurrently_ runIndexQuery xs)

-- | Run a list of insert queries in one single transaction.
runIndexQueries
    :: MonadIO m
    => MonadError IndexerError m
    => SQL.Connection -> [[IndexQuery]] -> m ()
runIndexQueries c = traverse_ (runIndexQueriesStep c)

runLastSyncQuery
    :: MonadError IndexerError m
    => MonadIO m
    => SQL.FromRow r
    => SQL.Connection
    -> SQL.Query
    -> m [r]
runLastSyncQuery connection lastSyncQuery
        = either throwError pure <=< liftIO
        $ handleSQLErrors (SQL.query connection lastSyncQuery ())

instance (MonadIO m, Monoid (InsertRecord event), MonadError IndexerError m)
    => IsIndex m event SQLiteIndexer where

    index timedEvent indexer = do
        let indexQueries = indexer ^. buildInsert $ indexer ^. prepareInsert $ timedEvent
        runIndexQueries (indexer ^. handle) indexQueries
        pure $ indexer & dbLastSync .~ (timedEvent ^. point)

    indexAll evts indexer = do

        let indexQueries = indexer ^. buildInsert $ foldMap (indexer ^. prepareInsert) evts
            updateLastSync = maybe id (dbLastSync .~) (maximumOf (folded . point) evts)

        runIndexQueries (indexer ^. handle) indexQueries
        pure $ updateLastSync indexer


instance MonadIO m => IsSync m event SQLiteIndexer where

    lastSyncPoint indexer
        = pure $ indexer ^. dbLastSync


instance MonadIO m => Closeable m SQLiteIndexer where

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
    liftIO $ SQL.withTransaction c
        (SQL.execute c q p)
    pure $ indexer & dbLastSync .~ p

-- | A helper for the definition of the 'Queryable' typeclass for 'SQLiteIndexer'
--
-- The helper just remove a bit of the boilerplate needed to transform data
-- to query the database.
--
-- It doesn't contain any logic, except a check for 'AheadOfLastSync' error,
-- in which case it throws the 'AheadOfLastSync' exception with a partial result.
-- If you don't want to query the database on a partial result,
-- use 'querySyncedOnlySQLiteIndexerWith'
--
-- It doesn't filter the result based on the given data point.
querySQLiteIndexerWith
    :: MonadIO m
    => MonadError (QueryError query) m
    => Ord (Point event)
    => SQL.FromRow r
    => (Point event -> query -> [SQL.NamedParam])
    -> SQL.Query
    -- ^ The sqlite query statement
    -- ^ A preprocessing of the query, to obtain SQL parameters
    -> (query -> [r] -> Result query)
    -- ^ Post processing of the result, to obtain the final result
    -> Point event -> query -> SQLiteIndexer event -> m (Result query)
querySQLiteIndexerWith toNamedParam sqlQuery fromRows p q indexer
    = do
        let c = indexer ^. handle
        res <- liftIO $ SQL.queryNamed c sqlQuery (toNamedParam p q)
        when (p < indexer ^. dbLastSync)
            $ throwError (AheadOfLastSync $ Just $ fromRows q res)
        pure $ fromRows q res

-- | A helper for the definition of the 'Queryable' typeclass for 'SQLiteIndexer'
--
-- The helper just remove a bit of the boilerplate needed to transform data
-- to query the database.
--
-- It doesn't contain any logic, except a check for 'AheadOfLastSync' error,
-- in which case it throws the 'AheadOfLastSync' without any result attached.
--
-- It doesn't filter the result based on the given data point.
querySyncedOnlySQLiteIndexerWith
    :: MonadIO m
    => MonadError (QueryError query) m
    => Ord (Point event)
    => SQL.FromRow r
    => (Point event -> query -> [SQL.NamedParam])
    -> SQL.Query
    -- ^ The sqlite query statement
    -- ^ A preprocessing of the query, to obtain SQL parameters
    -> (query -> [r] -> Result query)
    -- ^ Post processing of the result, to obtain the final result
    -> Point event -> query -> SQLiteIndexer event -> m (Result query)
querySyncedOnlySQLiteIndexerWith toNamedParam sqlQuery fromRows p q indexer
    = do
        let c = indexer ^. handle
        when (p < indexer ^. dbLastSync)
            $ throwError (AheadOfLastSync Nothing)
        res <- liftIO $ SQL.queryNamed c sqlQuery (toNamedParam p q)
        pure $ fromRows q res

