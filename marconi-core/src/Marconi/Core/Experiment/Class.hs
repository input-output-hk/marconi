{- |
    Base typeclasses to index events.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Class (
  IsIndex (..),
  indexIfJust,
  indexEither,
  indexAllEither,
  indexAllDescendingEither,
  Resetable (..),
  Queryable (..),
  queryEither,
  queryLatest,
  queryLatestEither,
  AppendResult (..),
  Closeable (..),
  IsSync (..),
  computeResumePoints,
  isAheadOfSync,
  HasGenesis (..),
) where

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Data.Foldable (foldlM, foldrM, minimumBy)
import Data.List.NonEmpty qualified as NonEmpty
import Marconi.Core.Experiment.Type (Point, QueryError, Result, Timed, point)

-- IsIndex

{- | The base class of an indexer, providing its key functionalities:
 indexing events and handling rollbacks.

     * @indexer@ the indexer implementation type
     * @event@ the indexed events
     * @m@ the monad in which our indexer operates

  Rules:

     * Rollback to last indexed point do nothing:
       @rollback p <=< index (Timed p evt) === index (Timed p evt)@;
     * Rollback eliminate intermediate indexing
       (precondition: none of the @point@ in @xs@ are equal to @p@):
       @rollback p <=< indexAll events <=< index (Timed p evt) === index (Timed p evt)@;
     * Rollback is idempotent: @rollback p <=< rollback p === rollback p@.
-}
class (Monad m) => IsIndex m event indexer where
  -- | index an event at a given point in time
  index
    :: (Eq (Point event))
    => Timed (Point event) (Maybe event)
    -> indexer event
    -> m (indexer event)

  -- | Index a bunch of event, associated to their point in time, in an indexer
  --
  -- The events must be sorted in ascending order (the most recent first)
  indexAll
    :: (Eq (Point event), Traversable f)
    => f (Timed (Point event) (Maybe event))
    -> indexer event
    -> m (indexer event)
  indexAll = flip $ foldlM (flip index)

  -- | Index a bunch of event, associated to their point in time, in an indexer
  --
  -- The events must be sorted in descending order (the most recent first)
  indexAllDescending
    :: (Eq (Point event), Traversable f)
    => f (Timed (Point event) (Maybe event))
    -> indexer event
    -> m (indexer event)
  indexAllDescending = flip $ foldrM index

  -- | Rollback to a previous point
  rollback :: (Ord (Point event)) => Point event -> indexer event -> m (indexer event)

  {-# MINIMAL index, rollback #-}

{- | If the event is @Nothing@, just updat the last sync event.
 Otherwise, store the event and update the last sync event.
-}
indexIfJust
  :: (IsIndex m event indexer)
  => (Timed (Point event) event -> indexer event -> m (indexer event))
  -- ^ the function that adds the event
  -> (Point event -> indexer event -> m (indexer event))
  -- ^ the function that update the last point, called in any case
  -> Timed (Point event) (Maybe event)
  -> indexer event
  -> m (indexer event)
indexIfJust appendEvent updateLatest timedEvent =
  case sequence timedEvent of
    Nothing -> updateLatest (timedEvent ^. point)
    Just e -> updateLatest (timedEvent ^. point) <=< appendEvent e

{- | Like @index@, but internalise its error in the result.

 It's useful when you don't want to internalise the error in the monad stack to handle it explicitly,
 it's often used when we target IO as we don't want to mess with @IOException@.
-}
indexEither
  :: ( IsIndex (ExceptT err m) event indexer
     , Eq (Point event)
     )
  => Timed (Point event) (Maybe event)
  -> indexer event
  -> m (Either err (indexer event))
indexEither evt = runExceptT . index evt

-- | Like @indexAll@, but internalise the error in the result.
indexAllEither
  :: ( IsIndex (ExceptT err m) event indexer
     , Traversable f
     , Ord (Point event)
     )
  => f (Timed (Point event) (Maybe event))
  -> indexer event
  -> m (Either err (indexer event))
indexAllEither evt = runExceptT . indexAll evt

-- | Like @indexAllDescending@, but internalise the error in the result.
indexAllDescendingEither
  :: ( IsIndex (ExceptT err m) event indexer
     , Traversable f
     , Ord (Point event)
     )
  => f (Timed (Point event) (Maybe event))
  -> indexer event
  -> m (Either err (indexer event))
indexAllDescendingEither evt = runExceptT . indexAllDescending evt

{- | We can reset an indexer, clearing all its content

     * @indexer@ is the indexer implementation type
     * @event@ the indexer events
     * @m@ the monad in which our indexer operates
-}
class (HasGenesis (Point event)) => Resetable m event indexer where
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
    :: (Ord (Point event))
    => Point event
    -> query
    -> indexer event
    -> m (Result query)

-- | Like 'queryEither, but internalise @QueryError@ in the result.
queryEither
  :: (Queryable (ExceptT (QueryError query) m) event query indexer)
  => (Ord (Point event))
  => Point event
  -> query
  -> indexer event
  -> m (Either (QueryError query) (Result query))
queryEither p q = runExceptT . query p q

-- | Like 'query', but use the latest point of the indexer instead of a provided one
queryLatest
  :: (Queryable m event query indexer)
  => (IsSync m event indexer)
  => (MonadError (QueryError query) m)
  => (Ord (Point event))
  => query
  -> indexer event
  -> m (Result query)
queryLatest q indexer = do
  p <- lastSyncPoint indexer
  query p q indexer

-- | Like 'queryEither', but use the latest point of the indexer instead of a provided one
queryLatestEither
  :: (Queryable (ExceptT (QueryError query) m) event query indexer)
  => (IsSync m event indexer)
  => (Monad m)
  => (Ord (Point event))
  => query
  -> indexer event
  -> m (Either (QueryError query) (Result query))
queryLatestEither q indexer = do
  p <- lastSyncPoint indexer
  queryEither p q indexer

-- | The indexer can take a result and complete it with its events
class AppendResult m event query indexer where
  appendResult
    :: (Ord (Point event))
    => Point event
    -> query
    -> indexer event
    -> m (Result query)
    -> m (Result query)

-- | We know how to close an indexer
class Closeable m indexer where
  close :: indexer event -> m ()

-- | We know how far an indexer went in the indexation of events.
class (Functor m) => IsSync m event indexer where
  -- | Last sync point of the indexer.
  lastSyncPoint :: indexer event -> m (Point event)

  -- | Last sync points of the indexer. By default, it wraps the point from 'lastSyncPoint' into a
  -- list. If you need more than one element, then you need to reimplement this function.
  lastSyncPoints :: Word -> indexer event -> m [Point event]

{- | Calculates the final resume points given list of resume points for multiple indexers.

This is a helper function to implement the @lastSyncPoints@ functon of 'IsSync' for any indexers
that handle several indexers (like 'Coordinator' and 'SQLiteAggregateQuery').

The logic is as follows.
We first find the resume points, identified as @ps@, of the indexer with the lowest overall point.
Then, we take the largest resume point of each indexer and, out of those, take the lowest point identified as @p@.
Finally, we return @ps@, but remove all points that are larger than @p@.

Here's an example:

>>> computeResumePoints [[0,1,2,3], [10], [2,3,4], [1,2]]
[0,1,2]
-}
computeResumePoints
  :: forall f point
   . (Traversable f, Ord point)
  => f [point]
  -- ^ Resume points of each indexer
  -> [point]
  -- ^ Common resume points shared by each indexer
computeResumePoints points =
  case mapM NonEmpty.nonEmpty points of
    Nothing -> []
    Just nonEmptyPoints ->
      let pointsWithLowestMinValue = minimumBy (\x y -> minimum x `compare` minimum y) nonEmptyPoints
          lowestPointOfMaxOfPoints = minimum $ fmap maximum points
       in NonEmpty.filter (<= lowestPointOfMaxOfPoints) $
            NonEmpty.sortBy (\x y -> y `compare` x) pointsWithLowestMinValue

-- | Check if the given point is ahead of the last syncPoint of an indexer
isAheadOfSync
  :: (Ord (Point event), IsSync m event indexer)
  => Point event
  -> indexer event
  -> m Bool
isAheadOfSync p indexer = (p >) <$> lastSyncPoint indexer

-- | Any type that has a starting point
class HasGenesis t where
  -- The point before anything starts
  genesis :: t
