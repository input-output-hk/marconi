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
 This module propose an alternative to the index implementation proposed in @Storable@.

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

        * Aggregate to maintain a fold value based on incoming events
        * Cache to keep in memory some query result, avoiding to hit the indexer
        * Catchup to speed up the synchronisation time of an indexer doing batch insertion
        * Delay to delay event processing for heavy computation.
        * Pruning, to compact data that can't be rollbacked.
        * Tracing, as a modifier to an existing indexer.
          (it allows us to opt-in for traces if we want, indexer by indexer)
        * Transform to change the input type of an indexer

  Contrary to the original Marconi design,
  indexers don't have a unique (in-memory/sqlite) implementation.
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

 === Define an indexer instance for 'ListIndexer'

        1. You need to define a type for @event@ (the input of your indexer).
        As soon as it's done, define the 'Point' type instance for this event,
        'Point' is a way to know when the Point was emitted.
        It can be a time, a slot number, a block, whatever information that tracks when
        an event happen.

        2. It's already enough to index `Timed` of the events you defined at step one
        of your indexer and to proceed to rollback.
        You can already test it, creating an indexer with `listIndexer`.

        3. Define a @query@ type and the corresponding 'Result' type.

        4. Then, for this query you need to define the 'Queryable' instance that
        corresponds to your indexer.

        5. The 'ListIndexer' is ready.

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

 === Define an indexer instance for a 'MixedIndexer'

 Follow in order the steps for the creation of a 'ListIndexer' (the in-memory part)
 and the ones for a 'SQLiteIndexer' (the on-disk part).

 Once it's done, you need to implement the 'AppendResult' for the in-memory part.
 It's purpose is to take the result of an on-disk query and to update it with on-memory events.

 You can choose different indexers for your in-memory part or on-disk part,
 but these choices are not documented yet.


 == Write a new indexer

    Most user probably /don't/ want to do this.

    A good reason is to add support for another backend
    (another database or another in-memory structure)

    The minimal typeclass that your indexer must implement/allow to implement are:

        * 'IsSync'
        * 'IsIndex'
        * 'AppendResult' (if you plan to use it as the in-memory part of a 'MixedIndexer')
        * 'Queryable'
        * 'Closeable' (if you plan to use it in a worker, and you probably plan to)

    Best practices is to implement as much as we can 'event'/'query' agnostic
    instances of the typeclasses of these module for the new indexer.
    When it's impossible to write a typeclass, think about how we can reduce the
    boilerplate with a helper function, and expose it.

    Try to provide a smart constructor to hide most of the complexity of your indexer.

    Then you also probably need to provide a helper to create workers for this indexer.
-}
module Marconi.Core.Experiment (
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
  QueryError (..),

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
  connection,
  SQLInsertPlan (SQLInsertPlan, planExtractor, planInsert),
  SQLRollbackPlan (SQLRollbackPlan, tableName, pointName, pointExtractor),

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
  FileBuilder (FileBuilder),
  EventBuilder (EventBuilder),
  mkFileIndexer,

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
  createWorker',
  createWorker,
  createWorkerPure,
  ProcessedInput (..),

  -- ***  Workers transformers

  -- | Transformers are used to alter the incoming the events sent to an indexer
  -- through a worker.
  --
  -- It allows to transform the content of a block or to silence some events.
  Transformer,
  mapEvent,
  mapMaybeEvent,
  traverseEvent,
  traverseMaybeEvent,
  scanEvent,
  scanEventM,
  scanMaybeEventM,
  transformer,
  transformerM,

  -- ** Coordinator
  Coordinator,
  workers,
  tokens,
  channel,
  nbWorkers,
  mkCoordinator,
  processQueue,

  -- * Common queries

  --
  -- Queries that can be implemented for all indexers
  EventAtQuery (..),
  EventsFromQuery (..),
  EventsMatchingQuery (..),
  allEvents,
  LatestEventsQuery (LatestEventsQuery),
  latestEvent,

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
  HasCatchupConfig (catchupBypassDistance, catchupBatchSize),

  -- ** Resuming/draining
  OrdPoint (comparePoint),
  PointCompare (..),
  WithResume,
  withResume,
  resumedIndexer,

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

import Marconi.Core.Experiment.Class (
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
import Marconi.Core.Experiment.Coordinator (
  Coordinator,
  channel,
  mkCoordinator,
  nbWorkers,
  processQueue,
  tokens,
  workers,
 )
import Marconi.Core.Experiment.Indexer.FileIndexer (
  EventBuilder (EventBuilder),
  FileBuilder (FileBuilder),
  FileIndexer (FileIndexer),
  FileStorageConfig (FileStorageConfig),
  mkFileIndexer,
 )
import Marconi.Core.Experiment.Indexer.LastPointIndexer (LastPointIndexer, lastPointIndexer)
import Marconi.Core.Experiment.Indexer.ListIndexer (ListIndexer, events, latestPoint, mkListIndexer)
import Marconi.Core.Experiment.Indexer.MixedIndexer (
  Flushable (..),
  HasMixedConfig (flushEvery, keepInMemory),
  MixedIndexer,
  inDatabase,
  inMemory,
  mkMixedIndexer,
  standardMixedIndexer,
 )
import Marconi.Core.Experiment.Indexer.SQLiteAggregateQuery (
  HasDatabasePath (..),
  IsSourceProvider,
  SQLiteAggregateQuery (..),
  SQLiteSourceProvider (..),
  aggregateConnection,
  mkSQLiteAggregateQuery,
 )
import Marconi.Core.Experiment.Indexer.SQLiteIndexer (
  GetLastStablePointQuery (GetLastStablePointQuery, getLastStablePointQuery),
  InsertPointQuery (InsertPointQuery),
  SQLInsertPlan (..),
  SQLRollbackPlan (..),
  SQLiteIndexer (..),
  SetLastStablePointQuery (SetLastStablePointQuery, getSetLastStablePointQuery),
  ToRow (..),
  connection,
  dbLastSync,
  handleSQLErrors,
  mkSingleInsertSqliteIndexer,
  mkSqliteIndexer,
  querySQLiteIndexerWith,
  querySyncedOnlySQLiteIndexerWith,
 )
import Marconi.Core.Experiment.Query (
  EventAtQuery (..),
  EventsFromQuery (..),
  EventsMatchingQuery (..),
  LatestEventsQuery (..),
  allEvents,
  latestEvent,
 )
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (..), IndexerTrans (..))
import Marconi.Core.Experiment.Transformer.IndexTransformer (
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
import Marconi.Core.Experiment.Transformer.WithCache (
  HasCacheConfig (cache),
  WithCache,
  addCacheFor,
  withCache,
 )
import Marconi.Core.Experiment.Transformer.WithCatchup (
  CatchupConfig (CatchupConfig),
  HasCatchupConfig (catchupBatchSize, catchupBypassDistance),
  WithCatchup,
  withCatchup,
 )
import Marconi.Core.Experiment.Transformer.WithDelay (
  HasDelayConfig (delayCapacity),
  WithDelay,
  withDelay,
 )
import Marconi.Core.Experiment.Transformer.WithFold (
  HasFold (..),
  WithFold,
  getLastEventAtQueryValue,
  withFold,
  withFoldMap,
 )
import Marconi.Core.Experiment.Transformer.WithPruning (
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
import Marconi.Core.Experiment.Transformer.WithResume (
  OrdPoint (comparePoint),
  PointCompare (..),
  WithResume,
  resumedIndexer,
  withResume,
 )
import Marconi.Core.Experiment.Transformer.WithTracer (
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
import Marconi.Core.Experiment.Transformer.WithTransform (
  HasTransformConfig (..),
  WithTransform,
  withTransform,
 )
import Marconi.Core.Experiment.Type (
  IndexerError (..),
  Point,
  ProcessedInput (..),
  QueryError (..),
  Result,
  Timed (..),
  event,
  point,
 )
import Marconi.Core.Experiment.Worker (
  Worker,
  WorkerIndexer (..),
  WorkerIndexerType,
  WorkerM (..),
  createWorker,
  createWorker',
  createWorkerPure,
  startWorker,
 )
import Marconi.Core.Experiment.Worker.Transformer (
  Transformer,
  mapEvent,
  mapMaybeEvent,
  scanEvent,
  scanEventM,
  scanMaybeEventM,
  transformer,
  transformerM,
  traverseEvent,
  traverseMaybeEvent,
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
