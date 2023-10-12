{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    A set of queries that can be implemented by any indexer

    See "Marconi.Core" for documentation.
-}
module Marconi.Core.Query (
  EventAtQuery (EventAtQuery),
  EventsMatchingQuery (EventsMatchingQuery),
  allEvents,
  LatestEventsQuery (LatestEventsQuery),
  latestEvent,
  EventsFromQuery (EventsFromQuery),
  Stability (Stable, Volatile),
  withStability,
) where

import Control.Comonad (Comonad (duplicate, extract))
import Control.Lens (view)
import Control.Lens qualified as Lens
import Control.Lens.Operators ((^.), (^..), (^?))
import Control.Monad (when)
import Control.Monad.Except (MonadError (catchError, throwError), MonadIO (liftIO), runExceptT)
import Data.ByteString qualified as BS
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import Marconi.Core.Class (
  AppendResult (appendResult),
  IsSync (lastStablePoint),
  Queryable (query),
  isAheadOfSync,
 )
import Marconi.Core.Indexer.FileIndexer (
  FileIndexer,
  compareMeta,
  deserialiseEvent,
  eventBuilder,
  extractPoint,
  getDirectoryMetadata,
 )
import Marconi.Core.Indexer.FileIndexer qualified as FileIndexer
import Marconi.Core.Indexer.ListIndexer (ListIndexer, events)
import Marconi.Core.Type (
  Point,
  QueryError (AheadOfLastSync, IndexerQueryError, NotStoredAnymore),
  Result,
  Timed,
  event,
  point,
 )

-- | Get the event stored by the indexer at a given point in time
data EventAtQuery event = EventAtQuery
  deriving (Eq, Ord, Show)

-- | Represents whether an event is considered to stable or not.
data Stability a = Stable a | Volatile a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Control.Comonad.Comonad Stability where
  extract (Stable x) = x
  extract (Volatile x) = x
  duplicate (Stable x) = Stable (Stable x)
  duplicate (Volatile x) = Volatile (Volatile x)

class HasPoint e p where
  getPoint :: e -> p

instance HasPoint (Timed point event) point where
  getPoint = view point

{- | Given an indexer and some traversable of query results, and given the fact that there is a path
    from a query result to a `Point`, calculate the stability of all the query results.
-}
withStability
  :: forall m event indexer f g
   . ( Monad m
     , Ord (Point event)
     , IsSync m event indexer
     , Traversable f
     , HasPoint (g event) (Point event)
     )
  => indexer event
  -- ^ An indexer
  -> f (g event)
  -- ^ A traversable of query results
  -> m (f (Stability (g event)))
withStability idx res = do
  lsp <- lastStablePoint idx
  pure $ calcStability lsp <$> res
  where
    calcStability lsp e = do
      if getPoint e <= lsp
        then Stable e
        else Volatile e

{- | The result of EventAtQuery is always an event.
 The error cases are handled by the query interface.
 in time
-}
type instance Result (EventAtQuery event) = Maybe event

instance
  (MonadError (QueryError (EventAtQuery event)) m)
  => Queryable m event (EventAtQuery event) ListIndexer
  where
  query p EventAtQuery ix = do
    let isAtPoint e p' = e ^. point == p'
    aHeadOfSync <- isAheadOfSync p ix
    when aHeadOfSync $
      throwError $
        AheadOfLastSync Nothing
    pure $ ix ^? events . Lens.folded . Lens.filtered (`isAtPoint` p) . event

instance
  (MonadIO m, MonadError (QueryError (EventAtQuery event)) m)
  => Queryable m event (EventAtQuery event) (FileIndexer meta)
  where
  query p EventAtQuery ix = do
    aHeadOfSync <- isAheadOfSync p ix
    when aHeadOfSync $ throwError $ AheadOfLastSync Nothing
    content <- getDirectoryMetadata ix
    let resultContent =
          find ((p ==) . (ix ^. eventBuilder . extractPoint) . FileIndexer.fileMetadata) content
    case resultContent of
      Nothing -> pure Nothing
      Just eventFile -> do
        let resultFile = FileIndexer.path eventFile
            deserialise = ix ^. eventBuilder . deserialiseEvent $ FileIndexer.fileMetadata eventFile
        result <- liftIO $ BS.readFile resultFile
        case deserialise result of
          Left err -> throwError $ IndexerQueryError err
          Right res -> pure res

instance
  (MonadError (QueryError (EventAtQuery event)) m)
  => AppendResult m event (EventAtQuery event) ListIndexer
  where
  appendResult p q indexer result =
    let extractDbResult =
          result `catchError` \case
            -- If we find an incomplete result in the first indexer, complete it
            AheadOfLastSync (Just r) -> pure r
            -- For any other error, forward it
            inDatabaseError -> throwError inDatabaseError
     in query p q indexer `catchError` \case
          -- If we find an incomplete result in the first indexer, complete it
          NotStoredAnymore -> extractDbResult
          -- For any other error, forward it
          inMemoryError -> throwError inMemoryError

{- | Query an indexer to find all events that match a given predicate

 The result should return the most recent first
-}
newtype EventsMatchingQuery event = EventsMatchingQuery {predicate :: event -> Maybe event}

-- | Get all the events that are stored in the indexer
allEvents :: EventsMatchingQuery event
allEvents = EventsMatchingQuery Just

-- | The result of an @EventMatchingQuery@
type instance Result (EventsMatchingQuery event) = [Stability (Timed (Point event) event)]

instance
  (MonadError (QueryError (EventsMatchingQuery event)) m)
  => Queryable m event (EventsMatchingQuery event) ListIndexer
  where
  query p q ix =
    withStability ix =<< do
      let isBefore p' e = p' >= e ^. point
      let result =
            mapMaybe (traverse $ predicate q) $
              ix ^.. events . Lens.folded . Lens.filtered (isBefore p)
      res <- withStability ix result
      aheadOfSync <- isAheadOfSync p ix
      when aheadOfSync $ throwError . AheadOfLastSync $ Just res
      pure result

instance
  (MonadError (QueryError (EventsMatchingQuery event)) m)
  => AppendResult m event (EventsMatchingQuery event) ListIndexer
  where
  appendResult p q indexer result =
    let extractDbResult =
          result `catchError` \case
            -- If we find an incomplete result in the first indexer, complete it
            AheadOfLastSync (Just r) -> pure r
            -- For any other error, forward it
            inDatabaseError -> throwError inDatabaseError

        extractMemoryResult =
          query p q indexer `catchError` \case
            -- If we find an incomplete result in the first indexer, complete it
            NotStoredAnymore -> pure []
            -- For any other error, forward it
            inMemoryError -> throwError inMemoryError
     in do
          dbResult <- extractDbResult
          memoryResult <- extractMemoryResult
          pure $ memoryResult <> dbResult

-- | Get the 'nbOfEvents' last non empty events from the indexer before the point given in the query
newtype LatestEventsQuery event = LatestEventsQuery {nbOfEvents :: Word}

-- | Get the latest non empty event before the point given in the query
latestEvent :: LatestEventsQuery event
latestEvent = LatestEventsQuery 1

type instance Result (LatestEventsQuery event) = [Timed (Point event) event]

instance
  (MonadError (QueryError (LatestEventsQuery event)) m)
  => Queryable m event (LatestEventsQuery event) ListIndexer
  where
  query p q ix = do
    aHeadOfSync <- isAheadOfSync p ix
    when aHeadOfSync $ throwError $ AheadOfLastSync Nothing
    pure $
      take (fromIntegral $ nbOfEvents q) $
        filter (\x -> x ^. point <= p) $
          ix ^. events

instance
  (MonadIO m, MonadError (QueryError (LatestEventsQuery event)) m)
  => Queryable m event (LatestEventsQuery event) (FileIndexer meta)
  where
  query p q ix = do
    aHeadOfSync <- isAheadOfSync p ix
    when aHeadOfSync $ throwError $ AheadOfLastSync Nothing
    content <- getDirectoryMetadata ix
    let validCandidate eventFile =
          (ix ^. eventBuilder . extractPoint) (FileIndexer.fileMetadata eventFile) <= p
            && FileIndexer.hasContent eventFile
        resultFile =
          sortBy (compareMeta ix `on` FileIndexer.fileMetadata) $ filter validCandidate content
        extractEvents
          :: [Either a (Timed (Point event) (Maybe event))]
          -> Either a [Maybe (Timed (Point event) event)]
        extractEvents = fmap (fmap sequence) . sequence
    result <- traverse (runExceptT . FileIndexer.deserialiseTimedEvent ix) resultFile
    case extractEvents result of
      Left err -> throwError $ IndexerQueryError err
      Right res -> pure $ take (fromIntegral $ nbOfEvents q) $ catMaybes res

-- | Get the non empty events from the given point (excluded) to the one of the query (included)
newtype EventsFromQuery event = EventsFromQuery {startingPoint :: Point event}

type instance Result (EventsFromQuery event) = [Timed (Point event) event]

instance
  (MonadError (QueryError (EventsFromQuery event)) m)
  => Queryable m event (EventsFromQuery event) ListIndexer
  where
  query p q ix = do
    aHeadOfSync <- isAheadOfSync p ix
    when aHeadOfSync $ throwError $ AheadOfLastSync Nothing
    pure $ filter (\x -> x ^. point <= p && x ^. point > startingPoint q) $ ix ^. events

instance
  (MonadIO m, MonadError (QueryError (EventsFromQuery event)) m)
  => Queryable m event (EventsFromQuery event) (FileIndexer meta)
  where
  query p q ix = do
    aHeadOfSync <- isAheadOfSync p ix
    when aHeadOfSync $ throwError $ AheadOfLastSync Nothing
    content <- getDirectoryMetadata ix
    let validCandidate eventFile =
          let eventPoint = (ix ^. eventBuilder . extractPoint) (FileIndexer.fileMetadata eventFile)
           in eventPoint <= p && eventPoint > startingPoint q && FileIndexer.hasContent eventFile
        resultFiles =
          sortBy (compareMeta ix `on` FileIndexer.fileMetadata) $ filter validCandidate content
        extractEvents
          :: [Either a (Timed (Point event) (Maybe event))]
          -> Either a [Maybe (Timed (Point event) event)]
        extractEvents = fmap (fmap sequence) . sequence
    result <- traverse (runExceptT . FileIndexer.deserialiseTimedEvent ix) resultFiles
    case extractEvents result of
      Left err -> throwError $ IndexerQueryError err
      Right res -> pure (catMaybes res)

instance
  (MonadError (QueryError (EventsFromQuery event)) m)
  => AppendResult m event (EventsFromQuery event) ListIndexer
  where
  appendResult p q indexer result =
    let extractDbResult =
          result `catchError` \case
            -- If we find an incomplete result in the first indexer, complete it
            AheadOfLastSync (Just r) -> pure r
            -- For any other error, forward it
            inDatabaseError -> throwError inDatabaseError

        extractMemoryResult =
          query p q indexer `catchError` \case
            -- If we find an incomplete result in the first indexer, complete it
            NotStoredAnymore -> pure []
            -- For any other error, forward it
            inMemoryError -> throwError inMemoryError
     in do
          dbResult <- extractDbResult
          memoryResult <- extractMemoryResult
          pure $ memoryResult <> dbResult
