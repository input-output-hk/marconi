{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    An indexer that uses an indexer for recent events (usually in-memory)
    and another one for older events (usually on-disk).

    This indexer maintains a queue of flush points.
    Once we received enough blocks (@keepInMemory@ + @flushEvery@ events),
    we dequeue the oldest flush point and send all the in memory events that
    are older or equal to this flush point on disk.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Indexer.MixedIndexer (
  Flushable (..),
  MixedIndexer,
  mkMixedIndexer,
  standardMixedIndexer,
  inMemory,
  inDatabase,
  HasMixedConfig (..),
  flushEveryVia,
  keepInMemoryVia,
) where

import Control.Lens (Lens', makeLenses, view, (%~), (+~), (-~))
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Kind (Type)
import Data.Sequence (Seq (Empty, (:|>)), (<|))
import Data.Sequence qualified as Seq
import Marconi.Core.Experiment.Class (
  AppendResult (appendResult),
  Closeable (close),
  HasGenesis,
  IsIndex (index, indexAllDescending),
  IsSync (lastSyncPoint),
  Queryable (query),
  Rollbackable (rollback),
 )
import Marconi.Core.Experiment.Indexer.ListIndexer (ListIndexer, events, latest, mkListIndexer)
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (unwrapMap))
import Marconi.Core.Experiment.Transformer.IndexWrapper (
  IndexWrapper (IndexWrapper),
  IndexerTrans (unwrap),
  indexVia,
  wrappedIndexer,
  wrapperConfig,
 )
import Marconi.Core.Experiment.Type (Point, Timed, point)

-- | Define a way to flush old events out of a container
class Flushable m event indexer where
  -- | The events container used when you flush event
  type Container (indexer :: Type -> Type) :: Type -> Type

  -- | Clear the memory and return its content
  flushMemory
    :: Point event
    -- ^ Last point to flush
    -> indexer event
    -> m (Container indexer (Timed (Point event) event), indexer event)

instance (Applicative m, Ord (Point event)) => Flushable m event ListIndexer where
  type Container ListIndexer = []

  flushMemory p ix =
    let (freshest, oldest) = span ((> p) . view point) $ ix ^. events
     in pure (oldest, ix & events .~ freshest)

-- | Internal state of the mixed indexer
data MixedIndexerState store event = MixedIndexerState
  { _configFlushPoints :: Seq (Point event)
  -- ^ The next coming flush points
  , _configStepsBeforeNextFlushPointCreation :: Word
  -- ^ How many events before creation the next flush point
  , _configCurrentMemorySize :: Word
  -- ^ How many events are in the inMemory buffer
  -- (used to triggers a flush on _configKeepInMemory + _configFlushEvery)
  , _configInDatabase :: store event
  -- ^ In database storage, usually for data that can't be rollbacked
  }

-- | Exposed properties of the mixed indexer
data MixedIndexerConfig = MixedIndexerConfig
  { _configKeepInMemory :: Word
  -- ^ How many events are kept in memory after a flush
  , _configFlushEvery :: Word
  -- ^ How many Point do we wait until a flush (MUST be strictly positive)
  }

-- | The aggregation of all the @MixedIndexer@ propeties
data MixedIndexerProperties store event = MixedIndexerProperties
  { _stateProperties :: MixedIndexerState store event
  , _configProperties :: MixedIndexerConfig
  }

makeLenses ''MixedIndexerState
makeLenses ''MixedIndexerConfig
makeLenses ''MixedIndexerProperties

{- | An indexer that keepAtLeast '_configKeepInMemory' events in memory
 and put the older one on disk by group of '_configFlushSize' events.
 The query interface for this indexer will always go through the database first and then prune
 results present in memory.

 @mem@ the indexer that handles old events, when we need to remove stuff from memory
 @store@ the indexer that handle the most recent events
-}
newtype MixedIndexer store mem event = MixedIndexer
  { _mixedWrapper :: IndexWrapper (MixedIndexerProperties store) mem event
  }

mkMixedIndexer
  :: Word
  -- ^ how many events are kept in memory after a flush
  -> Word
  -- ^ flush size
  -> store event
  -> mem event
  -> MixedIndexer store mem event
mkMixedIndexer keepNb flushNb db =
  let properties =
        MixedIndexerProperties
          (MixedIndexerState mempty flushNb 0 db)
          (MixedIndexerConfig keepNb flushNb)
   in MixedIndexer . IndexWrapper properties

standardMixedIndexer
  :: (IsSync m event store)
  => (Monad m)
  => (HasGenesis (Point event))
  => Word
  -- ^ how many events are kept in memory after a flush
  -> Word
  -- ^ flush size
  -> store event
  -> m (MixedIndexer store ListIndexer event)
standardMixedIndexer keepNb flushNb db = do
  lSync <- lastSyncPoint db
  pure $ mkMixedIndexer keepNb flushNb db (mkListIndexer & latest .~ lSync)

makeLenses ''MixedIndexer

-- | The a lens to access the queue of flush points for the indexer
flushPoints :: Lens' (MixedIndexer store mem event) (Seq (Point event))
flushPoints = mixedWrapper . wrapperConfig . stateProperties . configFlushPoints

-- | A counter to decide when a point must be added to the queue
stepsBeforeNextFlushPointCreation :: Lens' (MixedIndexer store mem event) Word
stepsBeforeNextFlushPointCreation =
  mixedWrapper . wrapperConfig . stateProperties . configStepsBeforeNextFlushPointCreation

-- | A counter to decide when we must send data on memory.
currentMemorySize :: Lens' (MixedIndexer store mem event) Word
currentMemorySize = mixedWrapper . wrapperConfig . stateProperties . configCurrentMemorySize

class HasMixedConfig indexer where
  flushEvery :: Lens' (indexer event) Word
  keepInMemory :: Lens' (indexer event) Word

flushEveryVia :: (HasMixedConfig indexer) => Lens' s (indexer event) -> Lens' s Word
flushEveryVia l = l . flushEvery

keepInMemoryVia :: (HasMixedConfig indexer) => Lens' s (indexer event) -> Lens' s Word
keepInMemoryVia l = l . keepInMemory

-- Overlapping because we prefer this instance to any other instance
-- as it access the immediate config
instance {-# OVERLAPPING #-} HasMixedConfig (MixedIndexer store mem) where
  flushEvery = mixedWrapper . wrapperConfig . configProperties . configFlushEvery
  keepInMemory = mixedWrapper . wrapperConfig . configProperties . configKeepInMemory

-- Overlappable so that we always configure
-- the outmost instance in case of an overlap
instance
  {-# OVERLAPPABLE #-}
  (IndexerTrans t, HasMixedConfig indexer)
  => HasMixedConfig (t indexer)
  where
  flushEvery = flushEveryVia unwrap
  keepInMemory = keepInMemoryVia unwrap

-- Overlappable so that we always configure
-- the outmost instance in case of an overlap
instance
  {-# OVERLAPPABLE #-}
  (IndexerMapTrans t, HasMixedConfig indexer)
  => HasMixedConfig (t indexer output)
  where
  flushEvery = flushEveryVia unwrapMap
  keepInMemory = keepInMemoryVia unwrapMap

-- | access to the in memory part of the indexer
inMemory :: Lens' (MixedIndexer store mem event) (mem event)
inMemory = mixedWrapper . wrappedIndexer

-- | access to the on disk part of the indexer
inDatabase :: Lens' (MixedIndexer store mem event) (store event)
inDatabase = mixedWrapper . wrapperConfig . stateProperties . configInDatabase

-- | Decide whether or not we should flush at this step
checkFlush
  :: MixedIndexer store mem event
  -> Maybe (Point event, MixedIndexer store mem event)
checkFlush indexer =
  let nextFlushDepth = indexer ^. keepInMemory + indexer ^. flushEvery

      reachFlushPoint = indexer ^. currentMemorySize >= nextFlushDepth

      dequeueNextFlushPoint =
        case indexer ^. flushPoints of
          Empty -> Nothing
          recentFlushPoints :|> oldestFlushPoint ->
            let indexer' =
                  indexer
                    & flushPoints .~ recentFlushPoints
                    & currentMemorySize -~ indexer ^. flushEvery
             in Just (oldestFlushPoint, indexer')
   in guard reachFlushPoint *> dequeueNextFlushPoint

-- | Add the current point to the flush points and reset the counter
createFlushPoint
  :: Point event
  -> MixedIndexer store mem event
  -> MixedIndexer store mem event
createFlushPoint p indexer =
  let queueFlushPoint = flushPoints %~ (p <|)
      resetNextFlushPointCounter = stepsBeforeNextFlushPointCreation .~ (indexer ^. flushEvery)
   in indexer
        & queueFlushPoint
        & resetNextFlushPointCounter

-- | Adjust counter and decide whether a flush should happent or not
tick
  :: Point event
  -> MixedIndexer store mem event
  -> (Maybe (Point event), MixedIndexer store mem event)
tick p indexer =
  let countEvent = (currentMemorySize +~ 1) . (stepsBeforeNextFlushPointCreation -~ 1)

      adjustStep ix =
        if ix ^. stepsBeforeNextFlushPointCreation == 0
          then createFlushPoint p ix
          else ix

      indexer' = adjustStep $ countEvent indexer
   in maybe (Nothing, indexer') (first Just) $ checkFlush indexer'

-- | Flush all the in-memory events to the database, keeping track of the latest index
flush
  :: ( IsIndex m event store
     , Flushable m event mem
     , Traversable (Container mem)
     , Ord (Point event)
     )
  => Point event
  -> MixedIndexer store mem event
  -> m (MixedIndexer store mem event)
flush flushPoint indexer = do
  (eventsToFlush, indexer') <- getCompose $ inMemory (Compose . flushMemory flushPoint) indexer
  inDatabase (indexAllDescending $ fmap Just <$> eventsToFlush) indexer'

instance
  ( Ord (Point event)
  , Flushable m event mem
  , Traversable (Container mem)
  , IsIndex m event store
  , IsIndex m event mem
  )
  => IsIndex m event (MixedIndexer store mem)
  where
  index timedEvent indexer = do
    indexer' <- indexVia inMemory timedEvent indexer
    let (potentialFlushPoint, indexer'') = tick (timedEvent ^. point) indexer'
    case potentialFlushPoint of
      Nothing -> pure indexer''
      Just p -> flush p indexer''

instance (IsSync event m mem) => IsSync event m (MixedIndexer store mem) where
  lastSyncPoint = lastSyncPoint . view inMemory

instance
  ( Monad m
  , Rollbackable m event store
  )
  => Rollbackable m event (MixedIndexer store ListIndexer)
  where
  -- Rollback for the mixed indexer is a bit imprecise as we don't know how many
  -- blocks are dropped during the rollback.
  -- As a consequence, we consider that we have at most n block in memory,
  -- n being the length of the in memory indexer.
  rollback p indexer = do
    let removeFlushPointsAfterRollback p' = flushPoints %~ Seq.dropWhileL (> p')

    indexer' <- inMemory (rollback p) indexer
    let memorySize = length $ indexer' ^. inMemory . events -- todo don't rely on length
    indexer'' <-
      if memorySize == 0
        then inDatabase (rollback p) indexer'
        else pure indexer'

    let updateStateBeforeNextFlush =
          stepsBeforeNextFlushPointCreation
            .~ (indexer'' ^. keepInMemory + indexer'' ^. flushEvery) - fromIntegral memorySize

    pure $
      indexer''
        & updateStateBeforeNextFlush
        & removeFlushPointsAfterRollback p
        & currentMemorySize .~ fromIntegral memorySize

instance
  ( AppendResult m event query ListIndexer
  , Queryable m event query store
  )
  => Queryable m event query (MixedIndexer store ListIndexer)
  where
  query valid q indexer =
    appendResult
      valid
      q
      (indexer ^. inMemory)
      (query valid q (indexer ^. inDatabase))

instance
  ( Closeable m store
  , Closeable m mem
  , Monad m
  )
  => Closeable m (MixedIndexer store mem)
  where
  close indexer = do
    close $ indexer ^. inMemory
    close $ indexer ^. inDatabase
