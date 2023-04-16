{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- for DerivingVia
{- |
 This module propose an alternative to the index implementation proposed in 'RewindableIndex.Storable'.

 = Motivation

 The point we wanted to address are the folowing:

    * @Storable@ implementation is designed in a way that strongly promotes indexers
      that rely on a mix of database and in-memory storage.
      We try to propose a more generic design that would allow:

        * full in-memory indexers
        * indexer backed by a simple file
        * indexer transformers, that add capability (logging, caching...) to an indexer
        * mock indexer, for testing purpose, with predefined behaviour
        * group of indexers, synchronised as a single indexer
        * implement in-memory/database storage that rely on other query heuristic

    * The original implementation considered the @StorablePoint@ as data that can be derived from @Event@,
      leading to the design of synthetic events to deal with indexer that didn't index enough data.

    * In marconi, the original design uses a callback design to handle `MVar` modification,
      we wanted to address this point as well.

 = Terminology

    * An /indexer/ is a data structure that can store any type of /events/.
    * Each /event/ is emitted at a givent /point/ in time.
    * A /query/ is a request that the /indexer/ should be able to answer.
    * An /indexer instance/ corresponds to the use of an indexer for a specific type of events.
    * Most of the time, an /indexer instance/ requires the implementation of some typeclasses to specify its behaviour.
    * An /indexer transformer/ slightly alter the behaviour of an existing transformer.
      In general, an /indexer transformer/ should add its logic to one of the typeclasses
      that specify the behaviour of an indexer.
    * A /coordinator/ is a specific kind of indexer that pass events to a set of indexer it coordonates.
    * A /worker/ is a wrapper around an indexer, that hides the indexer types to a coordinator
      and handle locally the lifecycle of the indexer.


 = Content

    * Base type classes to define an indexer, its query interface, and the required plumbing to handle rollback.
    * A full in-memory indexer (naive), a full SQLite indexer
      and an indexer that compose it with a SQL layer for persistence.
    * A coordinator for indexers, that can be exposed as an itdexer itself.
    * Some queries that can be applied to many indexers.
    * Several transformers for indexers:

        * Tracing, as a modifier to an existing indexer.
          (it allows us to opt-in for traces if we want, indexer by indexer)
        * Delay to delay event processing for heavy computation.
        * Pruning, to compact data that can't be rollbacked.

  Contrary to the original Marconi design,
  indexers don't have a unique (in-memory/sqlite) implementation,
  even if it is probably the one that is used as a reference indexer
  at the momont.

 = TODO List (non-exhaustive)

    * Provide a less naive in-memory indexer implementation than the list one.
      /This is actually debatable, as most of the data manipulation in memory fits very well with the complexity/
      /of the list operations (except if we need to foldr the in memory resume to answer a query)./
    * Test, test, test. The current code is partially tested, and it's wrong.
    * Re-implement some of our indexers.
    * Split up this mess in modules.
    * Generate haddock, double-check it, fill the void.
    * Provide a tutorial on how to write indexer, transformers, and how to instantiate them.
    * Cold start from disk.
    * Provide MonadState version of the functions that manipulates the indexer.
    * Add custom error handling at the worker and at the coordinator level,
      even if its likely that the coordinator can't do much aside distributing poison pills.

 = How-to

 == Setup an existing indexer

        1. You need to define a type for @event@ (the input of your indexer).
        As soon as it's done, define the 'Point' type instance for this event,
        'Point' is a way to know when the Point was emitted.
        It can be a time, a slot number, whatever information that tracks when
        an event happen.

        2. Choose the based indexing instances that you need to index your events.

            The most common approach is to go for a 'MixedIndexer'
            with an in memory indexer and a disk indexer.
            At the moment, 'ListIndexer' is the only pre-existing in memory indexer
            and 'SQLiteIndexer' is the only pre-existing on disk indexer.

            If you want to go with it, it's probably better to follow the steps below
            with the `ListIndexer` first, then with the `SQLiteIndexer`
            and finally with the `MixedIndexer`.

        3. You then need to implement the 'IsIndex' and 'IsSync'
           typeclasses for your indexer.

            If you went for a 'MixedIndexer' with a 'ListIndexer' and a 'SQLiteIndexer',
            these instances are already defined.

        4. You can then continue with 'Rewindable' instances for your indexer
           that handles the rollbacks.

            With the default 'MixedIndexer'/'ListIndexer'/'SQLiteIndexer' mix,
            you only need to define the instance for 'SQLiteIndexer'.
            'rewindSQLiteIndexerWith' can help to avoid boilerplate.

        5. The next step is probably to define a query type for your indexer.
        A query type usually come with a 'Result' type instance,
        the expected answer for this query.

        6. Then, for this query you need to define the 'Queryable' instances that
        correspond to your indexer.

            If you use a 'MixedIndexer' you need to define 'Queryable'
            only for the on-disk storage (@store@) and 'ResumableResult'
            for the in-memory part (@mem@).

        7. You have the minimal needed to run your indexer.

 == Write a new indexer

    The steps are almost the same as the steps 3 to 7 for reusing an indexer,
    except that you have to think about how it should be done in the general
    case.

        Best practices is to implement as much as we can 'event'/'query' agnostic
        instances of the typeclasses of these module for the new indexer.
        When it's impossible to write a typeclass, think about how we can reduce the
        boilerplate with a helper function, and expose it.

        Try to provide a smart constructor to hide most of the complexity of your indexer.

        Then you also probably need to provide a helper to create workers for this indexer.
-}
module Marconi.Core.Experiment
    (

    -- * Core types and typeclasses

    -- ** Core types
    --
    -- | Marconi's indexers relies on three main concepts:
    --
    --     1. @event@ the information we index;
    --     2. @indexer@ that stores relevant (for them) pieces of information
    --     from the @event@s;
    --     3. @query@ that define what can be asked to an @indexer@.
    --
      Point
    , Result
    , Container
    , TimedEvent (TimedEvent)
        , point
        , event
    -- ** Core typeclasses
    , IsIndex (..)
    , index'
    , indexAll'
    , HasGenesis (..)
    , IsSync (..)
    , isAheadOfSync
    , Queryable (..)
    , query'
    , queryLatest
    , queryLatest'
    , ResumableResult (..)
    , Rewindable (..)
    , Prunable (..)
    , Resumable (..)
    -- ** Errors
    , IndexerError
    , QueryError (..)

    -- * Core Indexers
    --
    -- | A bunch of indexers that should cover the general need.
    --
    --     * 'ListIndexer' to store events in memory.
    --     * 'SQLiteIndexer' to store events in a SQLite database.
    --     * 'MixedIndexer' to store recent events in an indexer
    --       and older events in another one.

    -- ** In memory
    --
    -- | A Full in memory indexer, backed by a simple list of events.
    -- Most of the logic for this indexer is generic.
    --
    -- To create an indexer instance that uses a list indexer,
    -- you have to define:
    --
    --     * the type of @event@ you want to store
    --     * the type(s) of query your indexer instance must handle
    --     * the 'Queryable' interface for these queries.
    , ListIndexer
        , listIndexer
        , events
        , latest
    -- ** In database
    -- | An in-memory indexer that stores its events in a SQLite database.
    --
    -- We try to provide as much machinery as possible to minimize the work needed
    -- to create an indexer instance that uses a 'SQLiteIndexer'.
    -- Populating a constructor and defining the queries
    -- (and the corresponding 'Queryable' interface)
    -- should be enough to have an operational indexer.
    , SQLiteIndexer (SQLiteIndexer)
        , handle
        , prepareInsert
        , buildInsert
        , dbLastSync
        , rewindSQLiteIndexerWith
        , querySQLiteIndexerWith
        , querySyncedOnlySQLiteIndexerWith
    -- ** Mixed inedexer
    -- | An indexer that uses two indexer internally:
    -- one for the most recents points, another for the older points.
    --
    -- The idea is that recent events are _volatile_ and can be rollbacked,
    -- so they must be easy to delete if needed.
    -- Older events are stable and can be store on disk to be persisted.
    , MixedIndexer
        , mixedIndexer
        , inMemory
        , inDatabase
    -- | When we want to store an event in a database, it may happen that you want to store it in many tables,
    -- ending with several insert.
    --
    -- This leads to two major issues:
    --     - Each query has its own parameter type, we consequently don't have a unique type for a parametrised query.
    --     - When we perform the insert, we want to process in the same way all the queries.
    --     - We can't know in the general case neither how many query will be needed, nor the param types.
    --     - We want to minimise the boilerplate for a end user.
    --
    -- To tackle these issue, we wrap our queries in a opaque type, @IndexQuery@,
    -- which hides the query parameters.
    -- Internally, we only have to deal with an @[IndexQuery]@ to be able to insert an event.
    , IndexQuery (..)
    -- | How we map an event to its sql representation
    --
    -- In general, it consists in breaking the event in many fields of a record,
    -- each field corresponding to the parameters required to insert a part of the event in one table.
    --
    -- Usually, an insert record will be of the form:
    --
    -- @
    -- data MyInsertRecord
    --     = MyInsertRecord
    --     { _tableA :: [ParamsForTableA]
    --     { _tableB :: [ParamsForTableB]
    --     -- ...
    --     }
    -- @
    , InsertRecord
    -- | A smart constructor for indexer that want to map an event to a single table.
    -- We just have to set the type family of `InsertRecord event` to `[param]` and
    -- then to provide the expected parameters.
    , singleInsertSQLiteIndexer
    -- | The indexer of the most recent events must be able to send a part
    -- of its events to the other indexer when they are stable.
    --
    -- The number of events that are sent and the number of events kept in memory
    -- is controlled by the 'MixedIndexer'
    , Flushable (..)
    -- ** LastPointIndexer
    , LastPointIndexer
        , lastPointIndexer

    -- * Running indexers
    -- | To control a set of indexers simultaneously,
    -- we want to be able to consider a list of them.
    -- Unfortunately, each indexer has its own purpose and (usually) operates
    -- on its own @event@ type.
    -- As a consequence, a list of indexer would be an heterogeneous list,
    -- which is hard to manipulate.
    -- 'WorkerM' (and 'Worker') offers an opaque representation of a 'Worker',
    -- which can be embed in a homogeneous list.
    --
    -- Coordinator take a bunch of 'Worker' and synchronise their work,
    -- sending them events and rollbacks.

    -- ** Workers
    , WorkerM (..)
    , Worker
    , WorkerIndexer
    , createWorker'
    , createWorker
    , createWorkerPure
    -- *** Worker creation for specific indexers
    , createListIndexerWorker
    , createSQLiteIndexerWorker
    , startWorker
    , ProcessedInput (..)
    -- ** Coordinator
    , Coordinator
        , lastSync
        , workers
        , tokens
        , channel
        , nbWorkers
    , start
    , step

    -- * Common queries
    --
    -- Queries that can be implemented for all indexers
    , EventAtQuery (..)
    , EventsMatchingQuery (..)
    , allEvents

    -- * Indexer Transformers
    -- ** Tracer
    , WithTracer
        , withTracer
        , tracedIndexer
        , tracer
    -- ** Delay
    , WithDelay
        , withDelay
        , delayedIndexer
        , delayCapacity
        , delayLength
        , delayBuffer
    -- ** Control Pruning
    , WithPruning
        , withPruning
        , prunedIndexer
        , securityParam
        , pruneEvery
        , nextPruning
        , stepsBeforeNext
        , currentDepth
    -- ** Caching
    , WithCache
        , withCache
        , addCacheFor
        , cachedIndexer
    -- ** Index Wrapper
    --
    -- | Wrap an indexer with some extra information to modify its behaviour
    --
    -- The wrapper comes with some instances that relay the function to the wrapped indexer,
    -- without any extra behaviour.
    --
    -- An indexer transformer can be a newtype of 'IndexWrapper',
    -- reuse some of its instances with @deriving via@,
    -- and specifies its own instances when it wants to add logic in it.

    -- *** Derive via machinery
    , IndexWrapper (IndexWrapper)
        , wrappedIndexer
        , wrapperConfig
    -- *** Helpers
    --
    -- | Via methods have two major utilities.
    --
    --     1. In some cases, we can't use deriving via to derive a typeclass,
    --     then, via method can help in the declaration of our typeclasses.
    --     2. Often, when an indexer transformer modify the default behaviour
    --     of an indexer typeclass, we need to call the some methods of the
    --     underlying indexer, via methods make it easier.
    , pruneVia
    , pruningPointVia
    , rewindVia
    , indexVia
    , indexAllVia
    , lastSyncPointVia
    , queryVia
    , queryLatestVia
    , syncPointsVia
    ) where

import Control.Concurrent qualified as Con (modifyMVar_, newEmptyMVar, newMVar, newQSemN, readMVar, signalQSemN,
                                            tryPutMVar, tryReadMVar, waitQSemN)
import Control.Concurrent.STM qualified as STM (atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Tracer qualified as Tracer (traceWith)
import Data.Sequence qualified as Seq

import Control.Concurrent (MVar, QSemN, ThreadId, forkFinally, killThread)
import Control.Lens (Getter, IndexedTraversal', Lens', at, filtered, folded, indexed, itraverseOf, lens, makeLenses,
                     maximumOf, set, to, view)
import Control.Lens.Operators ((%~), (&), (+~), (-~), (.~), (^.), (^..), (^?))
import Control.Monad (forever, guard, unless, void, when, (<=<))
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Tracer (Tracer)

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (TChan)
import Control.Exception (Exception, catch, throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.Foldable (foldl', foldrM, traverse_)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Sequence (Seq (Empty, (:|>)), (<|))
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQL


-- | A point in time, the concrete type of a point is now derived from an indexer event,
-- instead of an event.
-- The reason is that you may not want to always carry around a point when you manipulate an event.
type family Point event

-- | A result is a data family from the corresponding query descriptor.
-- A query is tied to an indexer by a typeclass, this design choice has two main reasons:
--     * we want to be able to define different query for the same indexer
--       (eg. we may want to define two distinct query types for an utxo indexer:
--       one to ge all the utxo for a given address,
--       another one for to get all the utxos emitted at a given slot).
--     * we want to assign a query type to different indexers.
type family Result query

-- | Attach an event to a point in time
data TimedEvent event =
     TimedEvent
         { _point :: Point event
         , _event :: event
         }

deriving stock instance (Show event, Show (Point event)) => Show (TimedEvent event)

makeLenses 'TimedEvent

-- | Error that can occur when you index events
data IndexerError
   = RollbackBehindHistory
     -- ^ An indexer don't have access to the history at the point that is asked
   | IndexerInternalError Text
     -- ^ The indexer did not respond
   | InvalidIndexer Text
     -- ^ The indexer is in an invalid state and can't recover
   | OtherIndexError Text
     -- ^ Any other cause of failure

instance Exception IndexerError where

deriving stock instance Show IndexerError

-- | The base class of an indexer.
-- The indexer should provide two main functionalities:
-- indexing events, and providing its last synchronisation point.
--
--     * @indexer@ the indexer implementation type
--     * @event@ the indexed events
--     * @m@ the monad in which our indexer operates
class Monad m => IsIndex m event indexer where

    -- | index an event at a given point in time
    index
        :: Eq (Point event)
        => TimedEvent event -> indexer event -> m (indexer event)

    -- | Index a bunch of points, associated to their event, in an indexer
    indexAll
        :: (Ord (Point event), Traversable f)
        => f (TimedEvent event) -> indexer event -> m (indexer event)
    indexAll = flip (foldrM index)

    {-# MINIMAL index #-}

-- | Like @index@, but internalise its error in the result.
--
-- It's useful when you don't want to internalise the error in the monad stack to handle it explicitly,
-- it's often used when we target IO as we don't want to mess with @IOException@.
index'
    ::
    ( IsIndex (ExceptT err m) event indexer
    , Monad m
    , Eq (Point event)
    )
    => TimedEvent event
    -> indexer event
    -> m (Either err (indexer event))
index' evt = runExceptT . index evt

-- | Like @indexAll@, but internalise the error in the result.
indexAll'
    ::
    ( IsIndex (ExceptT err m) event indexer
    , Monad m
    , Traversable f
    , Ord (Point event)
    )
    => f (TimedEvent event)
    -> indexer event
    -> m (Either err (indexer event))
indexAll' evt = runExceptT . indexAll evt

class HasGenesis t where

    -- The point before anything starts
    genesis :: t

class IsSync m event indexer where

    -- | Last sync of the indexer
    lastSyncPoint :: indexer event -> m (Point event)

-- | Check if the given point is ahead of the last syncPoint of an indexer,
isAheadOfSync ::
    (Ord (Point event), IsSync m event indexer, Functor m)
    => Point event -> indexer event -> m Bool
isAheadOfSync p indexer = (p >) <$> lastSyncPoint indexer


-- | Error that can occurs when you query an indexer
data QueryError query
   = AheadOfLastSync (Maybe (Result query))
     -- ^ The required point is ahead of the current index.
     -- The error may still provide its latest result if it make sense for the given query.
     --
     -- It can be useful for indexer that contains a partial knowledge and that want to pass
     -- this knowledge to another indexer to complete the query.
   | NotStoredAnymore
     -- ^ The requested point is too far in the past and has been pruned
   | IndexerQueryError Text
     -- ^ The indexer query failed

deriving stock instance Show (Result query) => Show (QueryError query)

-- | The indexer can answer a Query to produce the corresponding result of that query.
--
--     * @indexer@ is the indexer implementation type
--     * @event@ the indexer events
--     * @query@ the type of query we want to answer
--     * @m@ the monad in which our indexer operates
class Queryable m event query indexer where

    -- | Query an indexer at a given point in time
    -- It can be read as:
    -- "With the knowledge you have at that point in time,
    --  what is your answer to this query?"
    query
        :: Ord (Point event)
        => Point event
        -> query
        -> indexer event
        -> m (Result query)

-- | Like 'query', but internalise @QueryError@ in the result.
query'
    :: Queryable (ExceptT (QueryError query) m) event query indexer
    => Ord (Point event)
    => Point event
    -> query
    -> indexer event
    -> m (Either (QueryError query) (Result query))
query' p q = runExceptT . query p q

-- | Like 'query', but use the latest point of the indexer instead of a provided one
queryLatest
    :: Queryable m event query indexer
    => IsSync m event indexer
    => MonadError (QueryError query) m
    => Ord (Point event)
    => query
    -> indexer event
    -> m (Result query)
queryLatest q indexer = do
    p <- lastSyncPoint indexer
    query p q indexer

-- | Like 'query\'', but use the latest point of the indexer instead of a provided one
queryLatest'
    :: Queryable (ExceptT (QueryError query) m) event query indexer
    => IsSync m event indexer
    => Monad m
    => Ord (Point event)
    => query
    -> indexer event
    -> m (Either (QueryError query) (Result query))
queryLatest' q indexer = do
    p <- lastSyncPoint indexer
    query' p q indexer


-- | The indexer can take a result and complete it with its events
class ResumableResult m event query indexer where

    resumeResult
       :: Ord (Point event)
       => Point event
       -> query
       -> indexer event
       -> m (Result query)
       -> m (Result query)


-- | We can reset an indexer to a previous `Point`
--
--     * @indexer@ is the indexer implementation type
--     * @event@ the indexer events
--     * @m@ the monad in which our indexer operates
class Rewindable m event indexer where

    rewind :: Ord (Point event) => Point event -> indexer event -> m (indexer event)

-- | The indexer can prune old data.
-- The main purpose is to speed up query processing.
-- If the indexer is 'Rewindable' and 'Prunable',
-- it can't 'rewind' behind the 'pruningPoint',
-- the idea is to call 'prune' on points that can't be rollbacked anymore.
--
--     * @indexer@ is the indexer implementation type
--     * @desc@ the descriptor of the indexer, fixing the @Point@ types
--     * @m@ the monad in which our indexer operates
class Prunable m event indexer where

    -- Prune events of the indexer up to a given point in time
    prune :: Ord (Point event) => Point event -> indexer event -> m (indexer event)

    -- The latest pruned point (events up to the result are pruned)
    pruningPoint :: indexer event -> m (Maybe (Point event))

-- | Points from which we can restract safely
class Resumable m event indexer where

    -- | List the points that we still have in the indexer, allowing us to restart from them
    syncPoints :: Ord (Point event) => indexer event -> m [Point event]


-- | How events can be extracted from an indexer
type family Container (indexer :: * -> *) :: * -> *

-- | Define a way to flush old events out of a container
class Flushable m indexer where

    -- | Check if there isn't space left in memory
    currentLength :: indexer event -> m Word

    -- | Clear the memory and return its content
    flushMemory
        :: Word
           -- ^ How many event do we keep
        -> indexer event
        -> m (Container indexer (TimedEvent event), indexer event)

-- | The constructor is not exposed, use 'listIndexer' instead.
data ListIndexer event =
    ListIndexer
    { _events :: [TimedEvent event]
      -- ^ Stored @event@s, associated with their history 'Point'
    , _latest :: Point event
      -- ^ Ease access to the latest sync point
    }

deriving stock instance (Show event, Show (Point event)) => Show (ListIndexer event)

type instance Container ListIndexer = []

makeLenses 'ListIndexer

-- | A smart constructor for list indexer, starting at genesis with an empty list§.
listIndexer :: HasGenesis (Point event) => ListIndexer event
listIndexer = ListIndexer [] genesis

instance Monad m => IsIndex m event ListIndexer where

    index timedEvent ix = let

        appendEvent :: ListIndexer event -> ListIndexer event
        appendEvent = events %~ (timedEvent:)

        updateLatest :: ListIndexer event -> ListIndexer event
        updateLatest = latest .~ (timedEvent ^. point)

        in do
            pure $ ix
                & appendEvent
                & updateLatest

instance Applicative m => IsSync m event ListIndexer where
    lastSyncPoint = pure . view latest

instance Applicative m => Flushable m ListIndexer where

    -- | How many events are stored in the indexer
    currentLength ix = pure $ fromIntegral $ length (ix ^. events)

    -- | Flush a given number of events from the indexer
    flushMemory n ix = let

        (freshest, oldest) = splitAt (fromIntegral n) $ ix ^. events

        in pure (oldest, ix & events .~ freshest)

instance Applicative m => Rewindable m event ListIndexer where

    rewind p ix = let

        adjustLatestPoint :: ListIndexer event -> ListIndexer event
        adjustLatestPoint = latest .~ p

        cleanEventsAfterRollback :: ListIndexer event -> ListIndexer event
        cleanEventsAfterRollback = events %~ dropWhile isEventAfterRollback

        isIndexBeforeRollback :: ListIndexer event -> Bool
        isIndexBeforeRollback x = x ^. latest < p

        isEventAfterRollback :: TimedEvent event -> Bool
        isEventAfterRollback x = x ^. point > p

        in pure
        $ if isIndexBeforeRollback ix
             then ix -- if we're already before the rollback, we don't have to do anything
             else ix
                & cleanEventsAfterRollback
                & adjustLatestPoint

instance Applicative m => Resumable m event ListIndexer where

    syncPoints ix = let

      indexPoints = ix ^.. events . folded . point
      -- if the latest point of the index is not a stored event, we add it to the list of points
      addLatestIfNeeded p []        = [p]
      addLatestIfNeeded p ps@(p':_) = if p == p' then ps else p:ps

      in pure $ addLatestIfNeeded (ix ^. latest) indexPoints


data IndexQuery
    = forall param. SQL.ToRow param
    => IndexQuery
        { insertQuery :: SQL.Query
        , params      :: [param]
         -- ^ It's a list because me want to be able to deal with bulk insert.
        }

type family InsertRecord event

-- | Provide the minimal elements required to use a SQLite database to back an indexer.
data SQLiteIndexer event
    = SQLiteIndexer
        { _handle        :: SQL.Connection
          -- ^ The connection used to interact with the database
        , _prepareInsert :: TimedEvent event -> InsertRecord event
          -- ^ 'InsertRecord' is the typed representation of what has to be inserted in the database
          -- It should be a monoid, to allow insertion of 0 to n rows in a single transaction.
          --
          -- A list of something is fine if you plan to perform an single insert to store your evnt.
          -- If you need several inserts, a record where each field correspond to a list is probably
          -- a good choice.
        , _buildInsert   :: InsertRecord event -> [IndexQuery]
          -- ^ Map the 'InsertRecord' representation to 'IndexQuery',
          -- to actually performed the insertion in the database.
          -- One can think at the insert record as a typed representation of the parameters of the queries,
          -- ^ The query to extract the latest sync point from the database.
        , _dbLastSync    :: Point event
          -- ^ We keep the sync point in memory to avoid an SQL to retrieve it
        }

makeLenses ''SQLiteIndexer

-- | Run a list of insert queries in one single transaction.
runIndexQueries
    :: MonadIO m
    => MonadError IndexerError m
    => SQL.Connection -> [IndexQuery] -> m ()
runIndexQueries _ [] = pure ()
runIndexQueries c xs = let

    runIndexQuery (IndexQuery insertQuery params)
        = SQL.executeMany c insertQuery params

    in either throwError pure <=< liftIO
        $ fmap Right (SQL.withTransaction c $ mapConcurrently_ runIndexQuery xs)
            `catch` (\(x :: SQL.FormatError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
            `catch` (\(x :: SQL.ResultError) -> pure . Left . InvalidIndexer . Text.pack $ show x)
            `catch` (\(x :: SQL.SQLError) -> pure . Left . IndexerInternalError . Text.pack $ show x)

singleInsertSQLiteIndexer
    :: SQL.ToRow param
    => InsertRecord event ~ [param]
    => HasGenesis (Point event)
    => SQL.Connection
    -> (TimedEvent event -> param)
    -- ^ extract @param@ out of a 'TimedEvent'
    -> SQL.Query
    -- ^ the insert query
    -> SQLiteIndexer event
singleInsertSQLiteIndexer c toParam insertQuery
    = SQLiteIndexer
        {_handle = c
        , _prepareInsert = pure . toParam
        , _buildInsert = pure . IndexQuery insertQuery
        , _dbLastSync = genesis
        }

instance (MonadIO m, Monoid (InsertRecord event), MonadError IndexerError m)
    => IsIndex m event SQLiteIndexer where

    index timedEvent indexer = do
        let indexQueries = indexer ^. buildInsert
                $ indexer ^. prepareInsert
                $ timedEvent
        runIndexQueries (indexer ^. handle) indexQueries
        pure $ indexer & dbLastSync .~ (timedEvent ^. point)

    indexAll evts indexer = do

        let indexQueries = indexer ^. buildInsert $ foldMap (indexer ^. prepareInsert) evts
            updateLastSync = maybe id (dbLastSync .~) (maximumOf (folded . point) evts)

        runIndexQueries (indexer ^. handle) indexQueries
        pure $ updateLastSync indexer

instance (HasGenesis (Point event), SQL.FromRow (Point event), MonadIO m)
    => IsSync m event SQLiteIndexer where

    lastSyncPoint indexer
        = pure $ indexer ^. dbLastSync

-- | A helper for the definition of the 'Rewindable' typeclass for 'SQLiteIndexer'
rewindSQLiteIndexerWith
    :: (MonadIO m, SQL.ToRow (Point event))
    => SQL.Query
    -- ^ The rewind statement
    -> Point event
    -- ^ Point will be passed as a parameter to the query
    -> SQLiteIndexer event
    -- ^ We're just using the connection
    -> m (SQLiteIndexer event)
rewindSQLiteIndexerWith q p indexer = do
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

-- | LastPointIndexer.
-- An indexer that does nothing except keeping track of the last point.
-- While it may sound useless,
-- it can be usefull when you want to benefit of the capabilities of a transformer.
newtype LastPointIndexer event = LastPointIndexer {_lastPoint :: Point event}

deriving stock instance (Show event, Show (Point event)) => Show (LastPointIndexer event)

makeLenses 'LastPointIndexer

-- | A smart constructor for 'LastPointIndexer'
lastPointIndexer :: HasGenesis (Point event) => LastPointIndexer event
lastPointIndexer = LastPointIndexer genesis

instance (HasGenesis (Point event), Monad m)
    => IsIndex m event LastPointIndexer where

    index timedEvent _ = pure $ LastPointIndexer $ timedEvent ^. point

    indexAll evts _ = pure $ LastPointIndexer $ fromMaybe genesis $ maximumOf (folded . point) evts

instance Applicative m => IsSync m event LastPointIndexer where
    lastSyncPoint = pure . view lastPoint

instance Applicative m => Rewindable m event LastPointIndexer where

    rewind p _ = pure $ LastPointIndexer p

-- | The different types of input of a worker
data ProcessedInput event
   = Rollback (Point event)
   -- ^ A rollback happen and indexers need to go back to the given point in time
   | Index (TimedEvent event)
   -- ^ A new event has to be indexed

mapIndex
    :: Applicative f
    => Point event ~ Point event'
    => (event -> f event') -> ProcessedInput event -> f (ProcessedInput event')
mapIndex _ (Rollback p)       = pure $ Rollback p
mapIndex f (Index timedEvent) = Index . TimedEvent (timedEvent ^. point) <$> f (timedEvent ^. event)

-- Type alias for the type classes that are required to build a worker for an indexer
type WorkerIndexer n event indexer
    = ( IsIndex n event indexer
    , IsSync n event indexer
    , Resumable n event indexer
    , Rewindable n event indexer
    )

data WorkerM m input point =
    forall indexer event n.
    ( WorkerIndexer n event indexer
    , Point event ~ point
    ) =>
    Worker
        { workerState    :: MVar (indexer event)
          -- ^ the indexer controlled by this worker
        , transformInput :: input -> m event
          -- ^ used by the worker to check whether an input is a rollback or an event
        , hoistError     :: forall a. n a -> ExceptT IndexerError m a
          -- ^ used by the worker to check whether an input is a rollback or an event
        , errorBox       :: MVar IndexerError
          -- ^ a place where the worker places error that it can't handle,
          -- to notify the coordinator
        , onExit         :: indexer event -> n ()
        }

type Worker = WorkerM IO

-- | create a worker for an indexer, retuning the worker and the @MVar@ it's using internally
createWorker'
    :: ( MonadIO m, WorkerIndexer n event indexer)
    => (forall a. n a -> ExceptT IndexerError m a)
    -> (indexer event -> n ())
    -> (input -> m event)
    -> indexer event
    -> m (MVar (indexer event), WorkerM m input (Point event))
createWorker' hoist takePoisonPill getEvent ix = do
    workerState <- liftIO $ Con.newMVar ix
    errorBox <- liftIO Con.newEmptyMVar
    pure (workerState, Worker workerState getEvent hoist errorBox takePoisonPill)

-- | create a worker for an indexer that doesn't throw error
createWorkerPure
    :: (MonadIO m , WorkerIndexer m event indexer)
    => (indexer event -> m ())
    -> (input -> m event)
    -> indexer event
    -> m (MVar (indexer event), WorkerM m input (Point event))
createWorkerPure = createWorker' lift

-- | create a worker for an indexer that already throws IndexerError
createWorker
    :: (MonadIO m , WorkerIndexer (ExceptT IndexerError m) event indexer)
    => (indexer event -> ExceptT IndexerError m ())
    -> (input -> m event)
    -> indexer event
    -> m (MVar (indexer event), WorkerM m input (Point event))
createWorker = createWorker' id

-- | create a worker for a 'ListIndexer'
createListIndexerWorker
    :: (MonadIO m , WorkerIndexer m event ListIndexer)
    => (input -> m event)
    -> ListIndexer event
    -> m (MVar (ListIndexer event), WorkerM m input (Point event))
createListIndexerWorker = createWorkerPure (const $ pure ())

-- | create a worker for a 'SQLiteIndexer'
createSQLiteIndexerWorker
    :: (MonadIO m , WorkerIndexer m event SQLiteIndexer)
    => (input -> m event)
    -> SQLiteIndexer event
    -> m (MVar (SQLiteIndexer event), WorkerM m input (Point event))
createSQLiteIndexerWorker = createWorkerPure (liftIO . SQL.close . view handle)

-- | The worker notify its coordinator that it's ready
-- and starts waiting for new events and process them as they come
startWorker
    :: MonadIO m
    => Ord (Point input)
    => TChan (ProcessedInput input)
    -> QSemN
    -> Worker input (Point input) ->
    m ThreadId
startWorker chan tokens (Worker ix transformInput hoistError errorBox takePoisonPill) = let

    unlockCoordinator :: IO ()
    unlockCoordinator = Con.signalQSemN tokens 1

    fresherThan :: Ord (Point event) => TimedEvent event -> Point event -> Bool
    fresherThan evt p = evt ^. point > p

    indexEvent timedEvent = Con.modifyMVar_ ix $ \indexer -> do
        result <- runExceptT $ do
            indexerLastPoint <- hoistError $ lastSyncPoint indexer
            if timedEvent `fresherThan` indexerLastPoint
               then hoistError $ index timedEvent indexer
               else pure indexer
        either raiseError pure result

    raiseError err = do
        -- We don't need to check if tryPutMVar succeed
        -- because if @errorBox@ is already full, our job is done anyway
        void $ Con.tryPutMVar errorBox err
        unlockCoordinator
        throw err


    handleRollback p = Con.modifyMVar_ ix $ \indexer -> do
        result <- runExceptT $ hoistError $ rewind p indexer
        either raiseError pure result

    swallowPill = do
        indexer <- Con.readMVar ix
        void $ runExceptT $ hoistError $ takePoisonPill indexer

    loop chan' = forever $ do
        input <- STM.atomically $ STM.readTChan chan'
        processedEvent <- mapIndex transformInput input
        case processedEvent of
            Rollback p -> handleRollback p
            Index e    -> indexEvent e
        unlockCoordinator

    in liftIO $ do
        chan' <- STM.atomically $ STM.dupTChan chan
        forkFinally (loop chan') (const swallowPill)

-- | A coordinator synchronises the event processing of a list of indexers.
-- A coordinator is itself is an indexer.
-- It means that we can create a tree of indexer, with coordinators that partially process the data at each node,
-- and with concrete indexers at the leaves.
data Coordinator input = Coordinator
  { _lastSync  :: Point input -- ^ the last common sync point for the workers
  , _workers   :: [Worker input (Point input)] -- ^ the list of workers managed by this coordinator
  , _threadIds :: [ThreadId] -- ^ the thread ids of the workers
  , _tokens    :: QSemN -- ^ use to synchronise the worker
  , _channel   :: TChan (ProcessedInput input) -- ^ to dispatch input to workers
  , _nbWorkers :: Int -- ^ how many workers are we waiting for, should always be equal to @length workers@
  }

-- TODO handwrite lenses to avoid invalid states
makeLenses 'Coordinator

-- | create a coordinator with started workers
start
    :: HasGenesis (Point input)
    => Ord (Point input)
    => [Worker input (Point input)] -> IO (Coordinator input)
start workers' = let

    startWorkers channel' tokens' = traverse (startWorker channel' tokens') workers'

    in do
        let nb = length workers'
        tokens' <- Con.newQSemN 0 -- starts empty, will be filled when the workers will start
        channel' <- STM.newBroadcastTChanIO
        threadIds' <- startWorkers channel' tokens'
        pure $ Coordinator genesis workers' threadIds' tokens' channel' nb

-- | A coordinator step (send an input to its workers, wait for an ack of every worker before listening again)
step
    :: (HasGenesis (Point input)
    , Ord (Point input)
    , MonadIO m
    , MonadError IndexerError m)
    => Coordinator input -> ProcessedInput input -> m (Coordinator input)
step coordinator input = do
        case input of
            Index e    -> index e coordinator
            Rollback p -> rewind p coordinator

waitWorkers :: Coordinator input -> IO ()
waitWorkers coordinator = Con.waitQSemN (coordinator ^. tokens) (coordinator ^. nbWorkers)

dispatchNewInput :: Coordinator input -> ProcessedInput input -> IO ()
dispatchNewInput coordinator = STM.atomically . STM.writeTChan (coordinator ^. channel)

healthCheck
    :: MonadIO m
    => Coordinator input -> m (Maybe IndexerError)
healthCheck c = do
   let ws = c ^. workers
   errors <- liftIO $ traverse Con.tryReadMVar $ errorBox <$> ws
   pure $ listToMaybe $ catMaybes errors


-- A coordinator can be consider as an indexer that forwards the input to its worker
instance (MonadIO m, MonadError IndexerError m) => IsIndex m event Coordinator where

    index timedEvent coordinator = let

        setLastSync c e = c & lastSync .~ (e ^. point)

        stopWorkers = traverse_ killThread $ coordinator ^. threadIds

        in do
            liftIO $ dispatchNewInput coordinator $ Index timedEvent
            liftIO $ waitWorkers coordinator
            errors <- healthCheck coordinator
            case errors of
                Just err -> liftIO stopWorkers *> throwError err
                Nothing  -> pure $ setLastSync coordinator timedEvent

instance MonadIO m => IsSync m event Coordinator where
    lastSyncPoint indexer = pure $ indexer ^. lastSync

-- | To rewind a coordinator, we try and rewind all the workers.
instance
    (HasGenesis (Point event)
    , MonadIO m
    , MonadError IndexerError m
    ) => Rewindable m event Coordinator where

    rewind p = let

        setLastSync c = c & lastSync .~ p

        rewindWorkers :: Coordinator event -> m (Coordinator event)
        rewindWorkers c = do
            liftIO $ dispatchNewInput c $ Rollback p
            liftIO $ waitWorkers c
            errors <- healthCheck c
            case errors of
                Just err -> throwError err
                Nothing  -> pure $ setLastSync c

        in rewindWorkers

-- There is no point in providing a 'Queryable' interface for 'CoordinatorIndex' though,
-- as it's sole interest would be to get the latest synchronisation points,
-- but 'query' requires a 'Point' to provide a result.


-- | Get the event stored by the indexer at a given point in time
data EventAtQuery event = EventAtQuery
   deriving (Eq, Ord, Show)

-- | The result of EventAtQuery is always an event.
-- The error cases are handled by the query interface.
-- in time
type instance Result (EventAtQuery event) = event

instance MonadError (QueryError (EventAtQuery event)) m
    => Queryable m event (EventAtQuery event) ListIndexer where

    query p EventAtQuery ix = do
        let isAtPoint e p' = e ^. point == p'
        aHeadOfSync <- not <$> isAheadOfSync p ix
        when aHeadOfSync
            $ throwError $ AheadOfLastSync Nothing
        maybe
             -- If we can't find the point and if it's in the past, we probably pruned it
            (throwError NotStoredAnymore)
            pure
            $ ix ^? events . folded . filtered (`isAtPoint` p) . event

instance MonadError (QueryError (EventAtQuery event)) m
    => ResumableResult m event (EventAtQuery event) ListIndexer where

    resumeResult p q indexer result = result `catchError` \case
        -- If we didn't find a result in the 1st indexer, try in memory
        _inDatabaseError -> query p q indexer

-- ** Filtering available events

-- | Query an indexer to find all events that match a given predicate
--
-- The result should return the most recent first
newtype EventsMatchingQuery event
    = EventsMatchingQuery {predicate :: event -> Bool}

-- | Get all the events that are stored in the indexer
allEvents :: EventsMatchingQuery event
allEvents = EventsMatchingQuery (const True)

-- | The result of an @EventMatchingQuery@
type instance Result (EventsMatchingQuery event) = [TimedEvent event]

instance (MonadError (QueryError (EventsMatchingQuery event)) m, Applicative m)
    => Queryable m event (EventsMatchingQuery event) ListIndexer where

    query p q ix = do
        let isBefore p' e = p' >= e ^. point
        let result = ix ^.. events
                         . folded . filtered (isBefore p)
                         . filtered (predicate q . view event)
        check <- not <$> isAheadOfSync p ix
        if check
            then pure result
            else throwError . AheadOfLastSync . Just $ result

instance MonadError (QueryError (EventsMatchingQuery event)) m
    => ResumableResult m event (EventsMatchingQuery event) ListIndexer where

    resumeResult p q indexer result = let
        extractDbResult = result `catchError` \case
             -- If we find an incomplete result in the first indexer, complete it
            AheadOfLastSync (Just r) -> pure r
             -- For any other error, forward it
            inDatabaseError          -> throwError inDatabaseError

        extractMemoryResult = query p q indexer `catchError` \case
             -- If we find an incomplete result in the first indexer, complete it
            NotStoredAnymore -> pure []
             -- For any other error, forward it
            inMemoryError    -> throwError inMemoryError
        in do
            dbResult <- extractDbResult
            memoryResult <- extractMemoryResult
            pure $ memoryResult <> dbResult


data IndexWrapper config indexer event
    = IndexWrapper
        { _wrapperConfig  :: config event
        , _wrappedIndexer :: indexer event
        }

makeLenses 'IndexWrapper

-- | Helper to implement the @index@ functon of 'IsIndex' when we use a wrapper.
-- If you don't want to perform any other side logic, use @deriving via@ instead.
indexVia
    :: (IsIndex m event indexer, Eq (Point event))
    => Lens' s (indexer event) -> TimedEvent event -> s -> m s
indexVia l = l . index

-- | Helper to implement the @index@ functon of 'IsIndex' when we use a wrapper.
-- If you don't want to perform any other side logic, use @deriving via@ instead.
indexAllVia
    :: (Ord (Point event), IsIndex m event indexer, Traversable f)
    => Lens' s (indexer event) -> f (TimedEvent event) -> s -> m s
indexAllVia l = l . indexAll

instance
    (Monad m, IsIndex m event indexer)
    => IsIndex m event (IndexWrapper config indexer) where

    index = indexVia wrappedIndexer

-- | Helper to implement the @lastSyncPoint@ functon of 'IsSync' when we use a wrapper.
-- If you don't want to perform any other side logic, use @deriving via@ instead.
lastSyncPointVia
    :: IsSync m event indexer
    => Getter s (indexer event) -> s -> m (Point event)
lastSyncPointVia l = lastSyncPoint . view l

instance IsSync event m index
    => IsSync event m (IndexWrapper config index) where

    lastSyncPoint = lastSyncPointVia wrappedIndexer

-- | Helper to implement the @query@ functon of 'Queryable' when we use a wrapper.
-- If you don't want to perform any other side logic, use @deriving via@ instead.
queryVia
    :: (Queryable m event query indexer, Ord (Point event))
    => Getter s (indexer event)
    -> Point event -> query -> s -> m (Result query)
queryVia l p q = query p q . view l

-- | Helper to implement the @query@ functon of 'Queryable' when we use a wrapper.
-- If you don't want to perform any other side logic, use @deriving via@ instead.
queryLatestVia
    ::
    ( Queryable m event query indexer
    , MonadError (QueryError query) m
    , Ord (Point event)
    , IsSync m event indexer)
    => Getter s (indexer event)
    -> query -> s -> m (Result query)
queryLatestVia l q = queryLatest q . view l

instance Queryable m event query indexer
    => Queryable m event query (IndexWrapper config indexer) where

    query =  queryVia wrappedIndexer

-- | Helper to implement the @query@ functon of 'Resumable' when we use a wrapper.
-- If you don't want to perform any other side logic, use @deriving via@ instead.
syncPointsVia
    :: (Resumable m event indexer, Ord (Point event))
    => Getter s (indexer event) -> s -> m [Point event]
syncPointsVia l = syncPoints . view l


instance Resumable m event indexer
    => Resumable m event (IndexWrapper config indexer) where

    syncPoints = syncPointsVia wrappedIndexer

-- | Helper to implement the @prune@ functon of 'Prunable' when we use a wrapper.
-- Unfortunately, as @m@ must have a functor instance, we can't use @deriving via@ directly.
pruneVia
    :: (Functor m, Prunable m event indexer, Ord (Point event))
    => Lens' s (indexer event) -> Point event -> s -> m s
pruneVia l = l . prune

-- | Helper to implement the @pruningPoint@ functon of 'Prunable' when we use a wrapper.
-- Unfortunately, as @m@ must have a functor instance, we can't use @deriving via@ directly.
pruningPointVia
    :: Prunable m event indexer
    => Getter s (indexer event) -> s -> m (Maybe (Point event))
pruningPointVia l = pruningPoint . view l

-- | Helper to implement the @rewind@ functon of 'Rewindable' when we use a wrapper.
-- Unfortunately, as @m@ must have a functor instance, we can't use @deriving via@ directly.
rewindVia
    :: (Functor m, Rewindable m event indexer, Ord (Point event), HasGenesis (Point event))
    => Lens' s (indexer event)
    -> Point event -> s -> m s
rewindVia l p = l (rewind p)


newtype ProcessedInputTracer m event = ProcessedInputTracer { _unwrapTracer :: Tracer m (ProcessedInput event)}

makeLenses 'ProcessedInputTracer

-- | A tracer modifier that adds tracing to an existing indexer
newtype WithTracer m indexer event
    = WithTracer { _tracerWrapper :: IndexWrapper (ProcessedInputTracer m) indexer event }

withTracer :: Tracer m (ProcessedInput event) -> indexer event -> WithTracer m indexer event
withTracer tr = WithTracer . IndexWrapper (ProcessedInputTracer tr)

makeLenses 'WithTracer

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance IsSync m event indexer => IsSync m event (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance (MonadTrans t, IsSync (t m) event indexer) => IsSync (t m) event (WithTracer m indexer)


deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance Queryable m event query indexer => Queryable m event query (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance (MonadTrans t, Queryable (t m) event query indexer)
        => Queryable (t m) event query (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance Resumable m event indexer => Resumable m event (WithTracer m indexer)

tracer :: Lens' (WithTracer m indexer event) (Tracer m (ProcessedInput event))
tracer = tracerWrapper . wrapperConfig . unwrapTracer

tracedIndexer :: Lens' (WithTracer m indexer event) (indexer event)
tracedIndexer = tracerWrapper . wrappedIndexer

instance
    (Applicative m, IsIndex m event index)
    => IsIndex m event (WithTracer m index) where

    index timedEvent indexer = do
        res <- indexVia tracedIndexer timedEvent indexer
        Tracer.traceWith (indexer ^. tracer) $ Index timedEvent
        pure res

instance (MonadTrans t, Monad m, Monad (t m),  IsIndex (t m) event index)
    => IsIndex (t m) event (WithTracer m index) where

    index timedEvent indexer = do
        res <- indexVia tracedIndexer timedEvent indexer
        lift $ Tracer.traceWith (indexer ^. tracer) $ Index timedEvent
        pure res


instance
    ( Monad m
    , Rewindable m event index
    , HasGenesis (Point event)
    ) => Rewindable m event (WithTracer m index) where

    rewind p indexer = let

         rewindWrappedIndexer p' = rewindVia tracedIndexer p' indexer

         traceRewind =
              Tracer.traceWith (indexer ^. tracer) (Rollback p)

        in do
        -- Warn about the rewind first
        traceRewind
        rewindWrappedIndexer p

instance (Functor m, Prunable m event indexer)
    => Prunable m event (WithTracer m indexer) where

    prune = pruneVia tracedIndexer

    pruningPoint = pruningPointVia tracedIndexer

data DelayConfig event
    = DelayConfig
        { _configDelayCapacity :: Word
        , _configDelayLength   :: Word
        , _configDelayBuffer   :: Seq (TimedEvent event)
        }

makeLenses 'DelayConfig

-- | When indexing computation is expensive, you may want to delay it to avoid expensive rollback
-- 'WithDelay' buffers events before sending them to the underlying indexer.
-- Buffered events are sent when the buffers overflows.
--
-- An indexer wrapped in 'WithDelay' won't interact nicely with a coordinator at the moment,
-- as 'WithDelay' acts as it's processing an event while it only postpones the processing.
--
-- As a consequence, 'WithDelay' is preferably used at the top of the hierarchy.
newtype WithDelay indexer event
    = WithDelay { _delayWrapper :: IndexWrapper DelayConfig indexer event}

-- | A smart constructor for 'WithDelay'
withDelay
    :: Word -- ^ capacity
    -> indexer event
    -> WithDelay indexer event
withDelay c = WithDelay . IndexWrapper (DelayConfig c 0 Seq.empty)

makeLenses 'WithDelay

deriving via (IndexWrapper DelayConfig indexer)
    instance IsSync m event indexer => IsSync m event (WithDelay indexer)

deriving via (IndexWrapper DelayConfig indexer)
    instance Resumable m event indexer => Resumable m event (WithDelay indexer)

deriving via (IndexWrapper DelayConfig indexer)
    instance Queryable m event query indexer => Queryable m event query (WithDelay indexer)

delayedIndexer :: Lens' (WithDelay indexer event) (indexer event)
delayedIndexer = delayWrapper . wrappedIndexer

delayCapacity :: Lens' (WithDelay indexer event) Word
delayCapacity = delayWrapper . wrapperConfig . configDelayCapacity

delayLength :: Lens' (WithDelay indexer event) Word
delayLength = delayWrapper . wrapperConfig . configDelayLength

delayBuffer :: Lens' (WithDelay indexer event) (Seq (TimedEvent event))
delayBuffer = delayWrapper . wrapperConfig . configDelayBuffer

instance
    (Monad m, IsIndex m event indexer)
    => IsIndex m event (WithDelay indexer) where

    index timedEvent indexer = let

        bufferIsFull b = (b ^. delayLength) >= (b ^. delayCapacity)

        bufferEvent = (delayLength +~ 1) . (delayBuffer %~ (timedEvent <|))

        pushAndGetOldest = \case
            Empty            -> (timedEvent, Empty)
            (buffer' :|> e') -> (e', timedEvent <| buffer')

        in do
        if not $ bufferIsFull indexer
        then pure $ bufferEvent indexer
        else do
            let b = indexer ^. delayBuffer
                (oldest, buffer') = pushAndGetOldest b
            res <- indexVia delayedIndexer oldest indexer
            pure $ res & delayBuffer .~ buffer'

instance
    ( Monad m
    , Rewindable m event indexer
    , HasGenesis (Point event)
    , Ord (Point event)
    ) => Rewindable m event (WithDelay indexer) where

    rewind p indexer = let

        rewindWrappedIndexer p' = rewindVia delayedIndexer p' indexer

        resetBuffer = (delayLength .~ 0) . (delayBuffer .~ Seq.empty)

        before =  Seq.dropWhileL ((> p) . view point) $ indexer ^. delayBuffer

        in if Seq.null before
           -- if we empty the delay buffer,
           -- some events in the wrapped indexer may need a rewrite
           then resetBuffer <$> rewindWrappedIndexer p
           else pure $ indexer
                   & delayBuffer .~ before
                   & delayLength .~ fromIntegral (Seq.length before)

-- ** Pruning control

data PruningConfig event
    = PruningConfig
        { _configSecurityParam   :: Word
          -- ^ how far can a rollback go
        , _configPruneEvery      :: Word
          -- ^ once we have enough events, how often do we prune
        , _configNextPruning     :: Seq (Point event)
          -- ^ list of pruning point
        , _configStepsBeforeNext :: Word
          -- ^ events required before next aggregation milestones
        , _configCurrentDepth    :: Word
          -- ^ how many events aren't pruned yet
        }

makeLenses ''PruningConfig

-- | WithPruning control when we should prune an indexer
--
-- The main purpose is to optimize storage for events that can't be rollbacked anymore.
--
-- In some contexts, once you know that an event can't be rollback anymore,
-- your indexer may not need it, or may process it's information to make it
-- irrelevant.
-- In this case, you may want to `prune` the stored events.
newtype WithPruning indexer event
    = WithPruning { _pruningWrapper :: IndexWrapper PruningConfig indexer event }

makeLenses ''WithPruning

withPruning
    :: Word
          -- ^ how far can a rollback go
    -> Word
          -- ^ once we have enough events, how often do we prune
    -> indexer event
    -> WithPruning indexer event
withPruning sec every
    = WithPruning
    . IndexWrapper (PruningConfig sec every Seq.empty every 0)

deriving via (IndexWrapper PruningConfig indexer)
    instance IsSync m event indexer => IsSync m event (WithPruning indexer)

deriving via (IndexWrapper PruningConfig indexer)
    instance Queryable m event query indexer => Queryable m event query (WithPruning indexer)

prunedIndexer :: Lens' (WithPruning indexer event) (indexer event)
prunedIndexer = pruningWrapper . wrappedIndexer

securityParam :: Lens' (WithPruning indexer event) Word
securityParam = pruningWrapper . wrapperConfig . configSecurityParam

pruneEvery :: Lens' (WithPruning indexer event) Word
pruneEvery = pruningWrapper . wrapperConfig . configPruneEvery

nextPruning :: Lens' (WithPruning indexer event) (Seq (Point event))
nextPruning = pruningWrapper . wrapperConfig . configNextPruning

stepsBeforeNext :: Lens' (WithPruning indexer event) Word
stepsBeforeNext = pruningWrapper . wrapperConfig . configStepsBeforeNext

currentDepth :: Lens' (WithPruning indexer event) Word
currentDepth = pruningWrapper . wrapperConfig . configCurrentDepth

pruneAt
    :: WithPruning indexer event
    -> Maybe (Point event, WithPruning indexer event)
pruneAt indexer = let

    nextPruningDepth = indexer ^. securityParam + indexer ^. pruneEvery

    reachPruningPoint = indexer ^. currentDepth >= nextPruningDepth

    dequeueNextPruningPoint =
        case indexer ^. nextPruning of
            Empty    -> Nothing
            xs :|> p -> let
                indexer' = indexer
                    & nextPruning .~ xs
                    & currentDepth -~ indexer ^. pruneEvery
                in Just (p, indexer')

    in guard reachPruningPoint *> dequeueNextPruningPoint


startNewStep
    :: Point event
    -> WithPruning indexer event
    -> WithPruning indexer event
startNewStep p indexer
    = indexer
        & nextPruning %~ (p <|)
        & stepsBeforeNext .~ (indexer ^. pruneEvery)

tick
    :: Point event
    -> WithPruning indexer event
    -> (Maybe (Point event), WithPruning indexer event)
tick p indexer = let

    countEvent = (currentDepth +~ 1) . (stepsBeforeNext -~ 1)

    adjustStep ix = if ix ^. stepsBeforeNext == 0
        then startNewStep p ix
        else ix

    indexer' = adjustStep $ countEvent indexer

    in maybe (Nothing, indexer') (first Just) $ pruneAt indexer'


instance
    (Monad m, Ord (Point event), Prunable m event indexer, IsIndex m event indexer)
    => IsIndex m event (WithPruning indexer) where

    index timedEvent indexer = do
        indexer' <- indexVia prunedIndexer timedEvent indexer
        let (mp, indexer'') = tick (timedEvent ^. point) indexer'
        maybe
          (pure indexer'')
          (\p -> pruneVia prunedIndexer p indexer)
          mp

-- | The rewindable instance for `WithPruning` is a defensive heuristic
-- that may provide a non optimal behaviour but ensure that we don't
-- mess up with the rollbackable events.
instance
    ( Monad m
    , MonadError IndexerError m
    , Prunable m event indexer
    , Rewindable m event indexer
    , HasGenesis (Point event)
    , Ord (Point event)
    ) => Rewindable m event (WithPruning indexer) where

    rewind p indexer = let

        resetStep :: WithPruning indexer event -> WithPruning indexer event
        resetStep = do
            stepLength <- view pruneEvery
            set stepsBeforeNext stepLength

        removePruningPointsAfterRollback
            :: Point event
            -> WithPruning indexer event -> WithPruning indexer event
        removePruningPointsAfterRollback p' = nextPruning %~ Seq.dropWhileL (> p')

        countFromPruningPoints :: WithPruning indexer event -> WithPruning indexer event
        countFromPruningPoints = do
            points <- view nextPruning
            stepLength <- view pruneEvery
            -- We can safely consider that for each Pruning point still in the pipe,
            -- we have 'stepLength' events available in the indexer
            set currentDepth (fromIntegral $ length points * fromIntegral stepLength)

        isRollbackAfterPruning :: m Bool
        isRollbackAfterPruning = do
            p' <- pruningPoint $ indexer ^. prunedIndexer
            pure $ maybe True (p >=) p'

        in do
            valid <- isRollbackAfterPruning
            unless valid
                $ throwError RollbackBehindHistory
            countFromPruningPoints
                . removePruningPointsAfterRollback p
                . resetStep
                <$> rewindVia prunedIndexer p indexer


data CacheConfig query event
    = CacheConfig
      { _configCache     :: Map query (Result query)
      , _configOnForward :: TimedEvent event -> Result query -> Result query
      }

configCache :: Lens' (CacheConfig query event) (Map query (Result query))
configCache = lens _configCache (\cfg c -> cfg {_configCache = c})

configCacheEntries :: IndexedTraversal' query (CacheConfig query event) (Result query)
configCacheEntries f cfg
    = (\c -> cfg {_configCache = c})
    <$> Map.traverseWithKey (indexed f) (_configCache cfg)

configOnForward
    :: Getter
        (CacheConfig query event)
        (TimedEvent event -> Result query -> Result query)
configOnForward = to _configOnForward

-- | Setup a cache for some requests.
--
-- The cache is active only for the latest `Point`.
-- As a consequence, using `WithCache` is more effective on top of the on-disk
-- part of a `MixedIndexer`, or any other part of an indexer that has a relatively
-- stable sync point.
newtype WithCache query indexer event
    = WithCache { _cacheWrapper :: IndexWrapper (CacheConfig query) indexer event }

makeLenses 'WithCache

-- | A smart constructor for 'WithCache'.
-- The cache starts empty, you can populate it with 'addCacheFor'
withCache
    :: Ord query
    => (TimedEvent event -> Result query -> Result query)
    -> indexer event
    -> WithCache query indexer event
withCache _configOnForward
    = WithCache . IndexWrapper
    ( CacheConfig
        { _configCache = mempty
        , _configOnForward
        }
    )

-- | A (indexed-)traversal to all the entries of the cache
cacheEntries :: IndexedTraversal' query (WithCache query indexer event) (Result query)
cacheEntries = cacheWrapper . wrapperConfig . configCacheEntries

-- | Access to the cache
cache :: Lens' (WithCache query indexer event) (Map query (Result query))
cache = cacheWrapper . wrapperConfig . configCache

-- | How do we add event to existing cache
onForward
    :: Getter
        (WithCache query indexer event)
        (TimedEvent event -> Result query -> Result query)
onForward = cacheWrapper . wrapperConfig . configOnForward

-- | Access to the indexer that is cached.
cachedIndexer :: Lens' (WithCache query indexer event) (indexer event)
cachedIndexer = cacheWrapper . wrappedIndexer

-- | Add a cache for a specific query.
--
-- When added, the cache query the underlying indexer to populate the cache for this query.
--
-- If you want to add several indexers at the same time, use traverse.
addCacheFor
    :: Queryable (ExceptT (QueryError query) m) event query indexer
    => IsSync (ExceptT (QueryError query) m) event indexer
    => Monad m
    => MonadError IndexerError m
    => Ord query
    => Ord (Point event)
    => query
    -> WithCache query indexer event
    -> m (WithCache query indexer event)
addCacheFor q indexer
    = do
        initialResult <- runExceptT $ queryLatestVia cachedIndexer q indexer
        case initialResult of
            Left _err    -> throwError $ OtherIndexError "Can't create cache"
            Right result -> pure $ indexer & cache %~ Map.insert q result

deriving via (IndexWrapper (CacheConfig query) indexer)
    instance Resumable m event indexer => Resumable m event (WithCache query indexer)

deriving via (IndexWrapper (CacheConfig query) indexer)
    instance IsSync m event indexer => IsSync m event (WithCache query indexer)

-- | This instances update all the cached queries with the incoming event
-- and then pass this event to the underlying indexer.
instance
    (Applicative m, IsIndex m event index)
    => IsIndex m event (WithCache query index) where

    index timedEvent indexer = do
        indexer' <- indexVia cachedIndexer timedEvent indexer
        pure $ indexer' & cacheEntries %~ (indexer' ^. onForward) timedEvent

    indexAll evts indexer = do
        indexer' <- indexAllVia cachedIndexer evts indexer
        pure $ indexer' & cacheEntries %~ flip (foldl' (flip $ indexer' ^. onForward)) evts

rewindCache
    :: Applicative f
    => Ord (Point event)
    => Queryable f event query indexer
    => Point event
    -> WithCache query indexer event
    -> f (WithCache query indexer event)
rewindCache p indexer
    = itraverseOf
        cacheEntries
        (\q -> const $ queryVia cachedIndexer p q indexer)
        indexer

-- | Rewind the underlying indexer, clear the cache,
-- repopulate it with queries to the underlying indexer.
instance
    ( Monad m
    , Rewindable m event index
    , HasGenesis (Point event)
    , Queryable m event query index
    ) => Rewindable m event (WithCache query index) where

    rewind p indexer = do
        res <- rewindVia cachedIndexer p indexer
        rewindCache p res

instance
    (Ord query
    , Ord (Point event)
    , IsSync m event index
    , MonadError (QueryError query) m
    , Monad m
    , Queryable m event query index
    ) => Queryable m event query (WithCache query index) where


    -- | On a query, if it miss the cache, query the indexer.
    -- If the cache is fresher than the request point, query the underlying indexer,
    -- if it's the cached point, send the result.
    -- If the cache is behind the requested point, send an 'AheadOfLastSync' error
    -- with the cached content.
    query p q indexer = do
        syncPoint <- lastSyncPointVia cachedIndexer indexer
        let cached = indexer ^. cache . at q
        let queryWithoutCache = queryVia cachedIndexer p q indexer
        case compare p syncPoint of
            LT -> queryWithoutCache
            EQ -> maybe queryWithoutCache pure cached
            GT -> maybe queryWithoutCache (throwError . AheadOfLastSync . Just) cached

-- ** Mixed indexer

data MixedIndexerConfig store event
    = MixedIndexerConfig
        { _configKeepInMemory :: Word
        -- ^ how many events are kept in memory after a flush
        , _configFlushSize    :: Word
        -- ^ how many events are sent on disk when we flush
        , _configInDatabase   :: store event
        -- ^ In database storage, usually for data that can't be rollbacked
        }

makeLenses 'MixedIndexerConfig

-- | An indexer that keepAtLeast '_configKeepInMemory' events in memory
-- and put the older one on disk by group of '_configFlushSize' events.
-- The query interface for this indexer will always go through the database first and then prune
-- results present in memory.
--
-- @mem@ the indexer that handle old events, when we need to remove stuff from memory
-- @store@ the indexer that handle the most recent events
newtype MixedIndexer store mem event
    = MixedIndexer { _mixedWrapper :: IndexWrapper (MixedIndexerConfig store) mem event}

mixedIndexer
    :: Word
    -- ^ how many events are kept in memory after a flush
    -> Word
    -- ^ flush size
    -> store event
    -> mem event
    -> MixedIndexer store mem event
mixedIndexer flushNb keepNb db
    = MixedIndexer . IndexWrapper (MixedIndexerConfig flushNb keepNb db)

makeLenses 'MixedIndexer

flushSize :: Lens' (MixedIndexer store mem event) Word
flushSize = mixedWrapper . wrapperConfig . configFlushSize

keepInMemory :: Lens' (MixedIndexer store mem event) Word
keepInMemory = mixedWrapper . wrapperConfig . configKeepInMemory

inMemory :: Lens' (MixedIndexer store mem event) (mem event)
inMemory = mixedWrapper . wrappedIndexer

inDatabase :: Lens' (MixedIndexer store mem event) (store event)
inDatabase = mixedWrapper . wrapperConfig . configInDatabase


-- | Flush all the in-memory events to the database, keeping track of the latest index
flush ::
    ( Monad m
    , IsIndex m event store
    , Flushable m mem
    , Traversable (Container mem)
    , Ord (Point event)
    ) => MixedIndexer store mem event ->
    m (MixedIndexer store mem event)
flush indexer = do
    let keep = indexer ^. keepInMemory
    (eventsToFlush, indexer') <- getCompose
        $ inMemory (Compose . flushMemory keep) indexer
    inDatabase (indexAll eventsToFlush) indexer'

instance
    ( Monad m
    , Ord (Point event)
    , Flushable m mem
    , Traversable (Container mem)
    , IsIndex m event store
    , IsIndex m event mem
    ) => IsIndex m event (MixedIndexer store mem) where

    index timedEvent indexer = let

        isFull
            = (indexer ^. flushSize + indexer ^. keepInMemory <)
            <$> indexer ^. inMemory . to currentLength

        flushIfFull full = if full then flush else pure

        in do
        full <- isFull
        indexer' <- flushIfFull full indexer
        indexVia inMemory timedEvent indexer'

instance IsSync event m mem => IsSync event m (MixedIndexer store mem) where
    lastSyncPoint = lastSyncPoint . view inMemory

instance
    ( Monad m
    , Rewindable m event store
    ) => Rewindable m event (MixedIndexer store ListIndexer) where

    rewind p indexer = do
        indexer' <- inMemory (rewind p) indexer
        if not $ null $ indexer' ^. inMemory . events
            then pure indexer'
            else inDatabase (rewind p) indexer'

instance
    ( ResumableResult m event query ListIndexer
    , Queryable m event query store )
    => Queryable m event query (MixedIndexer store ListIndexer) where

    query valid q indexer
        = resumeResult valid q
            (indexer ^. inMemory)
            (query valid q (indexer ^. inDatabase))

