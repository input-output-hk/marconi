{- |
    Base typeclasses to index events.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Class (
  IsIndex (..),
  indexEither,
  indexAllEither,
  indexAllDescendingEither,
  Rollbackable (..),
  Resetable (..),
  Queryable (..),
  query',
  queryLatest,
  queryLatest',
  AppendResult (..),
  Closeable (..),
  IsSync (..),
  isAheadOfSync,
  HasGenesis (..),
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Data.Foldable (foldlM, foldrM)
import Marconi.Core.Experiment.Type (Point, QueryError, Result, Timed)

-- IsIndex

{- | The base class of an indexer, providing its key functionality:
 indexing events.

     * @indexer@ the indexer implementation type
     * @event@ the indexed events
     * @m@ the monad in which our indexer operates
-}
class Monad m => IsIndex m event indexer where
  -- | index an event at a given point in time
  index
    :: Eq (Point event)
    => Timed (Point event) event
    -> indexer event
    -> m (indexer event)

  -- | Index a bunch of event, associated to their point in time, in an indexer
  --
  -- The events must be sorted in ascending order (the most recent first)
  indexAll
    :: (Ord (Point event), Traversable f)
    => f (Timed (Point event) event)
    -> indexer event
    -> m (indexer event)
  indexAll = flip $ foldlM (flip index)

  -- | Index a bunch of event, associated to their point in time, in an indexer
  --
  -- The events must be sorted in descending order (the most recent first)
  indexAllDescending
    :: (Ord (Point event), Traversable f)
    => f (Timed (Point event) event)
    -> indexer event
    -> m (indexer event)
  indexAllDescending = flip $ foldrM index

  {-# MINIMAL index #-}

{- | Like @index@, but internalise its error in the result.

 It's useful when you don't want to internalise the error in the monad stack to handle it explicitly,
 it's often used when we target IO as we don't want to mess with @IOException@.
-}
indexEither
  :: ( IsIndex (ExceptT err m) event indexer
     , Eq (Point event)
     )
  => Timed (Point event) event
  -> indexer event
  -> m (Either err (indexer event))
indexEither evt = runExceptT . index evt

-- | Like @indexAll@, but internalise the error in the result.
indexAllEither
  :: ( IsIndex (ExceptT err m) event indexer
     , Traversable f
     , Ord (Point event)
     )
  => f (Timed (Point event) event)
  -> indexer event
  -> m (Either err (indexer event))
indexAllEither evt = runExceptT . indexAll evt

-- | Like @indexAllDescending@, but internalise the error in the result.
indexAllDescendingEither
  :: ( IsIndex (ExceptT err m) event indexer
     , Traversable f
     , Ord (Point event)
     )
  => f (Timed (Point event) event)
  -> indexer event
  -> m (Either err (indexer event))
indexAllDescendingEither evt = runExceptT . indexAllDescending evt

-- Rollback

{- | We can rollback an indexer to a previous `Point`

     * @indexer@ is the indexer implementation type
     * @event@ the indexer events
     * @m@ the monad in which our indexer operates
-}
class Rollbackable m event indexer where
  rollback :: Ord (Point event) => Point event -> indexer event -> m (indexer event)

{- | We can reset an indexer, clearing all its content

     * @indexer@ is the indexer implementation type
     * @event@ the indexer events
     * @m@ the monad in which our indexer operates
-}
class HasGenesis (Point event) => Resetable m event indexer where
  reset :: indexer event -> m (indexer event)

-- Queryable

{- | The indexer can answer a Query to produce the corresponding result of that query.

     * @indexer@ is the indexer implementation type
     * @event@ the indexer events
     * @query@ the type of query we want to answer
     * @m@ the monad in which our indexer operates
-}
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
class AppendResult m event query indexer where
  appendResult
    :: Ord (Point event)
    => Point event
    -> query
    -> indexer event
    -> m (Result query)
    -> m (Result query)

-- | We know how to close an indexer
class Closeable m indexer where
  close :: indexer event -> m ()

-- | We know how far an indexer went in the indexation of events
class IsSync m event indexer where
  -- | Last sync of the indexer
  lastSyncPoint :: indexer event -> m (Point event)

-- | Check if the given point is ahead of the last syncPoint of an indexer,
isAheadOfSync
  :: (Ord (Point event), IsSync m event indexer, Functor m)
  => Point event
  -> indexer event
  -> m Bool
isAheadOfSync p indexer = (p >) <$> lastSyncPoint indexer

-- | Any type that has a starting point
class HasGenesis t where
  -- The point before anything starts
  genesis :: t
