{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
 @Marconi.Core@ re-exports most of the content of the sub-modules and in most scenario,
 most of the stuff you need to set up a chain-indexing solution.

 = Features

 Marconi provides the following features out of the box:

    * full in-memory indexers;
    * indexers backed by a simple file;
    * sqlite-indexers;
    * indexer transformers, that add capability (logging, caching...) to an indexer;
    * group of indexers, synchronised as a single indexer;
    * mixed-indexers that allows a different type of storage for recent events and older ones.

 = Terminology

    * An /indexer/ is a data structure that can store any type of /events/.
    * Each /event/ is emitted at a givent /point/ in time.
    * A /query/ is a request that the /indexer/ should be able to answer.
    * An /indexer instance/ corresponds to the use of an indexer for a specific type of events.
    * Most of the time, an /indexer instance/ requires the implementation of some typeclasses to
      specify its behaviour.
    * An /indexer transformer/ slightly alter the behaviour of an existing transformer.
      In general, an /indexer transformer/ should add its logic to one of the typeclasses
      that specify the behaviour of an indexer.
    * A /coordinator/ is a specific kind of indexer that pass events to a set of indexer it coordonates.
    * A /worker/ is a wrapper around an indexer, that hides the indexer types to a coordinator
      and handle locally the lifecycle of the indexer.
    * A /preprocessor/ is a stateful function that transforms events before they are sent to an
    indexer or to a worker.


 = Content

    * Base type classes to define an indexer, its query interface, and the required plumbing to handle
      rollback.
    * A full in-memory indexer (naive), a full SQLite indexer
      and an indexer that compose it with a SQL layer for persistence.
    * A coordinator for indexers, that can be exposed as an itdexer itself.
    * Some queries that can be applied to many indexers.
    * Several transformers for indexers:

        * Aggregate to maintain a fold value based on incoming events
        * Cache to keep in memory some query result, avoiding to hit the indexer
        * Catchup to speed up the synchronisation time of an indexer doing batch insertion
        * Delay to delay event processing for heavy computation.
        * Pruning, to compact data that can't be rollbacked.
        * Tracing, as a modifier to an existing indexer.
          (it allows us to opt-in for traces if we want, indexer by indexer)
        * Transform to change the input type of an indexer

  indexers can have different implementations (SQLite, in-memory...).
  @SQLite@ indexers are the most common ones. For specific scenarios, you may want to combine
  them with a mixed indexer or to go for other types of indexers.
  We may consider other database backend than @SQLite@ in the future.

 = TODO List (non-exhaustive)

    * Provide MonadState version of the functions that manipulates the indexer.
    * Add custom error handling at the worker and at the coordinator level,
      even if its likely that the coordinator can't do much aside distributing poison pills.
    * Add connections pools for queries for the SQLite indexers.

 = How-to

 == Define an indexer instance

 At this stage, in most of the cases, you want to define an `SQLiteIndexer`,
 which will store the incoming events into an SQLite database.
 Other types of indexers exist and we detail some of them as well below.

 == Define an indexer instance for 'SQLiteIndexer'

        1. You need to define a type for @event@ (the input of your indexer).
        As soon as it's done, define the 'Point' type instance for this event,
        'Point' is a way to know when the Point was emitted.
        It can be a time, a slot number, a block, whatever information that tracks when
        an event happen.
        If you follow these steps in order to build a 'MixedIndexer',
        just reuse the @event@ type you declared for the 'ListIndexer'.


        2. Define a @query@ type and the corresponding 'Result' type.
        Again, reuse the one declared for the 'ListIndexer' if you're building a 'MixedIndexer'.

        3. A 'SQLiteIndexer' will require a bit of type machinery to be able to index record.
        There are two cases,
        A simple one, where you only need one insert query to index your event,
        And a more complex one, where several inserts are needed.

            In the simple case, you can use 'singleInsertSQLiteIndexer' to create your indexer.
            It requires the definition of a @param@ type, a data representation of the Timed
            that SQLite can process (it should have a @ToRow@ instance).
            You also need to declare a 'InsertRecord' type instance that is a list of this @param@.

            In the complex case, you need to define a custom 'InsertRecord' type instance.
            And your own insertion function. This case is not covered yet in this tutorial.

            We also need to enable rollback events,
            'rollbackSQLiteIndexerWith' can save you from a bit of boilerplate.

        5. Define a @query@ type and the corresponding 'Result' type.
        If you plan to use a 'MixedIndexer' you probably want to reuse the query

        6. Then, for this query you need to define the 'Queryable' instance.
        There's no helper on this one, but you probably want to query the database and
        to aggregate the query result in your @Result@ type.

 === Define an indexer instance for 'ListIndexer'

        1. You need to define a type for @event@ (the input of your indexer).
        As soon as it's done, define the 'Point' type instance for this event,
        'Point' is a way to know when the Point was emitted.
        It can be a time, a slot number, a block, whatever information that tracks when
        an event happens.

        2. It's already enough to index `Timed` of the events you defined at step one
        of your indexer and to proceed to rollback.
        You can already test it, creating an indexer with `listIndexer`.

        3. Define a @query@ type and the corresponding 'Result' type.

        4. Then, for this query you need to define the 'Queryable' instance that
        corresponds to your indexer.

        5. The 'ListIndexer' is ready.

 === Define an indexer instance for a 'MixedIndexer'

 Follow in order the steps for the creation of a 'ListIndexer' (the in-memory part)
 and the ones for a 'SQLiteIndexer' (the on-disk part).

 Once it's done, you need to implement the 'AppendResult' for the in-memory part.
 It's purpose is to take the result of an on-disk query and to update it with on-memory events.

 You can choose different indexers for your in-memory part or on-disk part,
 but these choices are not documented yet.


 == Write a new indexer

    Most users probably /don't/ want to do this.

    A good reason is to add support for another backend
    (another database or another in-memory structure)

    The minimal typeclass that your indexer must implement/allow to implement are:

        * 'IsSync'
        * 'IsIndex'
        * 'Queryable'
        * 'Closeable' (if you plan to use it in a worker, and you probably plan to)
        * 'AppendResult' (if you plan to use it as the in-memory part of a 'MixedIndexer')

    Best practices is to implement as much as we can 'event'/'query' agnostic
    instances of the typeclasses of these module for the new indexer.
    When it's impossible to write a typeclass, think about how we can reduce the
    boilerplate with a helper function, and expose it.

    Try to provide a smart constructor to hide most of the complexity of your indexer.

    Then you also probably need to provide a helper to create workers for this indexer.
-}
module Marconi.Core (
  -- * Core types and typeclasses

  -- ** Core types

  --

  -- | Marconi's indexers relies on three main concepts:
  --
  --     1. @event@ the information we index;
  --     2. @indexer@ that stores relevant (for it) pieces of information
  --     from the @event@s;
  --     3. @query@ that defines what can be asked to an @indexer@.
  Point,
  Result,
  Timed (Timed),
  point,
  event,

  -- ** Core typeclasses
  HasGenesis (..),
  IsIndex (..),
  indexEither,
  indexAllEither,
  indexAllDescendingEither,
  Resetable (..),
  resumeFrom,
  IsSync (..),
  isAheadOfSync,
  Closeable (..),
  Queryable (..),
  queryEither,
  queryLatest,
  queryLatestEither,
  AppendResult (..),

  -- ** Errors
  IndexerError (..),
  _RollbackBehindHistory,
  _IndexerInternalError,
  _InvalidIndexer,
  _StopIndexer,
  _ResumingFailed,
  _IndexerCloseTimeoutError,
  _OtherIndexError,
  QueryError (..),
  _AheadOfLastSync,
  _NotStoredAnymore,
  _IndexerQueryError,
  _SlotNoBoundsInvalid,

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
  ListIndexer,
  mkListIndexer,
  events,
  latestPoint,

  -- ** On disk

  -- *** SQLite

  -- | An in-memory indexer that stores its events in a SQLite database.
  --
  -- We try to provide as much machinery as possible to minimize the work needed
  -- to create an indexer instance that uses a 'SQLiteIndexer'.
  -- Populating a constructor and defining the queries
  -- (and the corresponding 'Queryable' interface)
  -- should be enough to have an operational indexer.
  SQLiteIndexer (SQLiteIndexer),
  GetLastStablePointQuery (GetLastStablePointQuery, getLastStablePointQuery),
  SetLastStablePointQuery (SetLastStablePointQuery, getSetLastStablePointQuery),
  InsertPointQuery (InsertPointQuery),
  SQLiteDBLocation (Memory, Storage),
  -- | Start a new indexer or resume an existing SQLite indexer
  --
  -- The main difference with 'SQLiteIndexer' is
  -- that we set 'dbLastSync' thanks to the provided query
  mkSqliteIndexer,
  -- | A smart constructor for indexer that want to map an event to a single table.
  -- We just have to set the type family of `InsertRecord event` to `[param]` and
  -- then to provide the expected parameters.
  --
  -- It is monomorphic restriction of 'mkSqliteIndexer'
  mkSingleInsertSqliteIndexer,
  inMemoryDB,
  parseDBLocation,
  connection,
  SQLInsertPlan (SQLInsertPlan, planExtractor, planInsert),
  SQLRollbackPlan (SQLRollbackPlan, planRollback),
  defaultRollbackPlan,

  -- **** Reexport from SQLite
  ToRow (..),
  dbLastSync,
  querySQLiteIndexerWith,
  querySyncedOnlySQLiteIndexerWith,
  handleSQLErrors,
  SQLiteAggregateQuery (SQLiteAggregateQuery),
  aggregateConnection,
  mkSQLiteAggregateQuery,
  SQLiteSourceProvider (SQLiteSourceProvider),
  IsSourceProvider,
  HasDatabasePath (getDatabasePath),

  -- *** On file

  -- | An indexer that serialise the event to disk
  FileIndexer (FileIndexer),
  FileStorageConfig (FileStorageConfig),
  FileCleanup,
  withPartition,
  FileBuilder (FileBuilder),
  EventBuilder (EventBuilder),
  EventInfo (fileMetadata),
  mkFileIndexer,
  LastEventIndexer (LastEventIndexer),
  mkLastEventIndexer,
  LastEventConfig (LastEventConfig),
  GetLastQuery (GetLastQuery),

  -- ** Mixed indexer

  --

  -- | An indexer that uses two indexer internally:
  -- one for the most recents points, another for the older points.
  --
  -- The idea is that recent events are _volatile_ and can be rollbacked,
  -- so they must be easy to delete if needed.
  -- Older events are stable and can be store on disk to be persisted.
  MixedIndexer,
  -- | A smart constructor for 'MixedIndexer'.
  -- It doesn't sync up the last syncEvent of inMemory and in Database indexers.
  --
  -- As a consequence, if these indexers aren't empty, you probably want to
  -- modify the result of the constructor.
  mkMixedIndexer,
  -- | A smart constructor for a /standard/ 'MixedIndexer':
  -- an on-disk indexer and a 'ListIndexer' for the in-memory part.
  --
  -- Contrary to 'mkMixedIndexer',
  -- this smart constructor checks the content of the on-disk indexer
  -- to set the 'lastSyncPoint' correctly.
  standardMixedIndexer,
  inMemory,
  inDatabase,
  -- | A type class that give access to the configuration of a 'MixedIndexer'
  HasMixedConfig (flushEvery, keepInMemory),
  -- | The indexer of the most recent events must be able to send a part
  -- of its events to the other indexer when they are stable.
  --
  -- The number of events that are sent and the number of events kept in memory
  -- is controlled by the 'MixedIndexer'
  Flushable (..),

  -- ** LastPointIndexer
  LastPointIndexer,
  lastPointIndexer,

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
  WorkerM (..),
  Worker,
  WorkerIndexer (..),
  WorkerIndexerType,
  startWorker,
  createWorker,
  createWorkerHoist,
  createWorkerPure,
  createWorkerWithPreprocessing,
  ProcessedInput (..),

  -- *** Preprocessors

  -- | Preprocessors are used to alter the incoming the events sent to an indexer
  -- through a worker.
  --
  -- It allows to transform the content of a block or to silence some events.
  Preprocessor,
  mapEvent,
  mapMaybeEvent,
  traverseEvent,
  traverseMaybeEvent,
  scanEvent,
  scanEventM,
  scanMaybeEvent,
  scanMaybeEventM,
  preprocessor,
  preprocessorM,

  -- **** Resuming/draining
  Resume,
  withResume,

  -- ** Coordinator
  Coordinator,
  workers,
  tokens,
  channel,
  nbWorkers,
  mkCoordinator,
  processQueue,
  CloseSwitch (..),

  -- * Common queries

  --
  -- Queries that can be implemented for all indexers
  EventAtQuery (..),
  EventsFromQuery (..),
  EventsMatchingQuery (..),
  allEvents,
  calcStability,
  LatestEventsQuery (LatestEventsQuery),
  latestEvent,
  Stability (..),
  isStable,
  queryErrorWithStability,
  WithStability (WithStability, unWithStability),
  withStabilityM,
  withStability,
  withStabilityAt,

  -- * Indexer Transformers
  IndexerTrans (..),
  IndexerMapTrans (..),

  -- ** Logging
  IndexerEvent (..),

  -- *** Trace
  WithTrace,
  withTrace,
  withTraceM,
  HasTraceConfig (trace),

  -- *** Tracer
  WithTracer,
  withTracer,
  withTracerM,
  HasTracerConfig (tracer),

  -- ** Catchup
  WithCatchup,
  withCatchup,
  CatchupConfig (CatchupConfig),
  mkCatchupConfig,
  configCatchupEventHook,
  HasCatchupConfig (catchupBypassDistance, catchupBatchSize),
  CatchupEvent (Synced),

  -- *** SQLite
  createIndexTable,

  -- ** Delay
  WithDelay,
  withDelay,
  HasDelayConfig (delayCapacity),

  -- ** Pruning
  WithPruning,
  withPruning,
  nextPruning,
  stepsBeforeNext,
  currentDepth,
  Prunable (..),
  HasPruningConfig (securityParam, pruneEvery),

  -- ** Arbitrary action on events
  WithAction (..),
  WithActionConfig (..),
  withActionConfigAction,
  withActionWrapper,

  -- ** Streaming
  Streamable (streamFrom, streamTo),
  withStream,

  -- ** Caching
  WithCache,
  withCache,
  addCacheFor,
  HasCacheConfig (cache),

  -- ** Transforming input
  WithTransform,
  withTransform,
  HasTransformConfig (..),

  -- ** Transform an indexer into a fold
  WithFold,
  withFold,
  withFoldMap,
  getLastEventAtQueryValue,
  HasFold (fold),

  -- ** Index Wrapper

  --

  -- | Wrap an indexer with some extra information to modify its behaviour
  --
  -- The wrapper comes with some instances that relay the function to the wrapped indexer,
  -- without any extra behaviour.
  --
  -- An indexer transformer can be a newtype of 'IndexTransformer',
  -- reuse some of its instances with @deriving via@,
  -- and specifies its own instances when it wants to add logic in it.

  -- *** Derive via machinery
  IndexTransformer (IndexTransformer),
  wrappedIndexer,
  wrapperConfig,

  -- *** Helpers

  --

  -- | Via methods have two major utilities.
  --
  --     1. In some cases, we can't use deriving via to derive a typeclass,
  --     then, via method can help in the declaration of our typeclasses.
  --     2. Often, when an indexer transformer modify the default behaviour
  --     of an indexer typeclass, we need to call the some methods of the
  --     underlying indexer, via methods make it easier.
  pruneVia,
  pruningPointVia,
  rollbackVia,
  resetVia,
  indexVia,
  indexAllDescendingVia,
  setLastStablePointVia,
  lastStablePointVia,
  lastSyncPointVia,
  closeVia,
  queryVia,
  queryLatestVia,
) where

import Control.Monad.Except (MonadError (catchError, throwError))

import Marconi.Core.Class (
  AppendResult (..),
  Closeable (..),
  HasGenesis (..),
  IsIndex (..),
  IsSync (..),
  Queryable (..),
  Resetable (..),
  indexAllDescendingEither,
  indexAllEither,
  indexEither,
  isAheadOfSync,
  queryEither,
  queryLatest,
  queryLatestEither,
 )
import Marconi.Core.Coordinator (
  CloseSwitch (..),
  Coordinator,
  channel,
  mkCoordinator,
  nbWorkers,
  processQueue,
  tokens,
  workers,
 )
import Marconi.Core.Indexer.FileIndexer (
  EventBuilder (EventBuilder),
  EventInfo (fileMetadata),
  FileBuilder (FileBuilder),
  FileCleanup,
  FileIndexer (FileIndexer),
  FileStorageConfig (FileStorageConfig),
  mkFileIndexer,
  withPartition,
 )
import Marconi.Core.Indexer.LastEventIndexer (
  GetLastQuery (GetLastQuery),
  LastEventConfig (LastEventConfig),
  LastEventIndexer (LastEventIndexer),
  mkLastEventIndexer,
 )
import Marconi.Core.Indexer.LastPointIndexer (LastPointIndexer, lastPointIndexer)
import Marconi.Core.Indexer.ListIndexer (ListIndexer, events, latestPoint, mkListIndexer)
import Marconi.Core.Indexer.MixedIndexer (
  Flushable (..),
  HasMixedConfig (flushEvery, keepInMemory),
  MixedIndexer,
  inDatabase,
  inMemory,
  mkMixedIndexer,
  standardMixedIndexer,
 )
import Marconi.Core.Indexer.SQLiteAggregateQuery (
  HasDatabasePath (..),
  IsSourceProvider,
  SQLiteAggregateQuery (..),
  SQLiteSourceProvider (..),
  aggregateConnection,
  mkSQLiteAggregateQuery,
 )
import Marconi.Core.Indexer.SQLiteIndexer (
  GetLastStablePointQuery (GetLastStablePointQuery, getLastStablePointQuery),
  InsertPointQuery (InsertPointQuery),
  SQLInsertPlan (..),
  SQLRollbackPlan (..),
  SQLiteDBLocation (..),
  SQLiteIndexer (..),
  SetLastStablePointQuery (SetLastStablePointQuery, getSetLastStablePointQuery),
  ToRow (..),
  connection,
  dbLastSync,
  defaultRollbackPlan,
  handleSQLErrors,
  inMemoryDB,
  mkSingleInsertSqliteIndexer,
  mkSqliteIndexer,
  parseDBLocation,
  querySQLiteIndexerWith,
  querySyncedOnlySQLiteIndexerWith,
 )
import Marconi.Core.Preprocessor (
  Preprocessor,
  mapEvent,
  mapMaybeEvent,
  preprocessor,
  preprocessorM,
  scanEvent,
  scanEventM,
  scanMaybeEvent,
  scanMaybeEventM,
  traverseEvent,
  traverseMaybeEvent,
 )
import Marconi.Core.Preprocessor.Resume (
  Resume,
  withResume,
 )
import Marconi.Core.Query (
  EventAtQuery (..),
  EventsFromQuery (..),
  EventsMatchingQuery (..),
  LatestEventsQuery (..),
  Stability (..),
  WithStability (WithStability, unWithStability),
  allEvents,
  calcStability,
  isStable,
  latestEvent,
  queryErrorWithStability,
  withStability,
  withStabilityAt,
  withStabilityM,
 )
import Marconi.Core.Transformer.Class (IndexerMapTrans (..), IndexerTrans (..))
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (..),
  closeVia,
  indexAllDescendingVia,
  indexVia,
  lastStablePointVia,
  lastSyncPointVia,
  queryLatestVia,
  queryVia,
  resetVia,
  rollbackVia,
  setLastStablePointVia,
  wrappedIndexer,
  wrapperConfig,
 )
import Marconi.Core.Transformer.WithAction (
  WithAction (..),
  WithActionConfig (..),
  withActionConfigAction,
  withActionWrapper,
 )
import Marconi.Core.Transformer.WithCache (
  HasCacheConfig (cache),
  WithCache,
  addCacheFor,
  withCache,
 )
import Marconi.Core.Transformer.WithCatchup (
  CatchupConfig (CatchupConfig),
  CatchupEvent (Synced),
  HasCatchupConfig (catchupBatchSize, catchupBypassDistance),
  WithCatchup,
  configCatchupEventHook,
  mkCatchupConfig,
  withCatchup,
 )
import Marconi.Core.Transformer.WithCatchup.SQLite (createIndexTable)
import Marconi.Core.Transformer.WithDelay (
  HasDelayConfig (delayCapacity),
  WithDelay,
  withDelay,
 )
import Marconi.Core.Transformer.WithFold (
  HasFold (..),
  WithFold,
  getLastEventAtQueryValue,
  withFold,
  withFoldMap,
 )
import Marconi.Core.Transformer.WithPruning (
  HasPruningConfig (pruneEvery, securityParam),
  Prunable (..),
  WithPruning,
  currentDepth,
  nextPruning,
  pruneEvery,
  pruneVia,
  pruningPointVia,
  securityParam,
  stepsBeforeNext,
  withPruning,
 )
import Marconi.Core.Transformer.WithStream (
  withStream,
 )
import Marconi.Core.Transformer.WithStream.Streamable (
  Streamable (streamFrom, streamTo),
 )
import Marconi.Core.Transformer.WithTracer (
  HasTraceConfig (trace),
  HasTracerConfig (tracer),
  IndexerEvent (..),
  WithTrace,
  WithTracer,
  trace,
  tracer,
  withTrace,
  withTraceM,
  withTracer,
  withTracerM,
 )
import Marconi.Core.Transformer.WithTransform (
  HasTransformConfig (..),
  WithTransform,
  withTransform,
 )
import Marconi.Core.Type (
  IndexerError (..),
  Point,
  ProcessedInput (..),
  QueryError (..),
  Result,
  Timed (..),
  event,
  point,
  _AheadOfLastSync,
  _IndexerCloseTimeoutError,
  _IndexerInternalError,
  _IndexerQueryError,
  _InvalidIndexer,
  _NotStoredAnymore,
  _OtherIndexError,
  _ResumingFailed,
  _RollbackBehindHistory,
  _SlotNoBoundsInvalid,
  _StopIndexer,
 )
import Marconi.Core.Worker (
  Worker,
  WorkerIndexer (..),
  WorkerIndexerType,
  WorkerM (..),
  createWorker,
  createWorkerHoist,
  createWorkerPure,
  createWorkerWithPreprocessing,
  startWorker,
 )

{- | Try to rollback to a given point to resume the indexer.

 If we can't resume from here,
 either we allow reset and we restart the indexer from genesis,
 or we don't and we throw an error.
-}
resumeFrom
  :: (MonadError IndexerError m)
  => (Resetable m event indexer)
  => (IsIndex m event indexer)
  => (IsSync m event indexer)
  => (HasGenesis (Point event))
  => (Ord (Point event))
  => Point event
  -- ^ expected resume point
  -> Bool
  -- ^ do we allow reset?
  -> indexer event
  -- ^ the indexer to resume
  -> m (Point event, indexer event)
  -- ^ the indexer back to the provided given point
resumeFrom p allowReset indexer =
  let handleError = \case
        RollbackBehindHistory ->
          if allowReset
            then reset indexer
            else throwError RollbackBehindHistory
        err -> throwError err
   in do
        indexer' <- rollback p indexer `catchError` handleError
        resumePoint <- lastSyncPoint indexer'
        pure (resumePoint, indexer')
