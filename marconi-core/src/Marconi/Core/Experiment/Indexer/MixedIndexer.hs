{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    An indexer that uses an indexer for recent events (usually in-memory)
    and another one for older events (usually on-disk).

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

  -- | Check if there isn't space left in memory
  currentLength :: indexer event -> m Word

  -- | Clear the memory and return its content
  flushMemory
    :: Point event
    -- ^ Last point to flush
    -> indexer event
    -> m (Container indexer (Timed (Point event) event), indexer event)

instance (Applicative m, Ord (Point event)) => Flushable m event ListIndexer where
  type Container ListIndexer = []

  -- \| How many events are stored in the indexer
  currentLength ix = pure $ fromIntegral $ length (ix ^. events)

  -- \| Flush a given number of events from the indexer
  flushMemory p ix =
    let (freshest, oldest) = span ((> p) . view point) $ ix ^. events
     in pure (oldest, ix & events .~ freshest)

data MixedIndexerConfig store event = MixedIndexerConfig
  { _configKeepInMemory :: Word
  -- ^ How many events are kept in memory after a flush
  , _configFlushEvery :: Word
  -- ^ How many Point do we wait until a flush (MUST be strictly poisitive)
  , _configFlushPoints :: Seq (Point event)
  -- ^ The next coming flush points
  , _configStepsBeforeNextFlush :: Word
  -- ^ How many forward event before the next flush point
  , _configCurrentMemorySize :: Word
  -- ^ How many events are in the inMemory buffer
  , _configInDatabase :: store event
  -- ^ In database storage, usually for data that can't be rollbacked
  }

makeLenses 'MixedIndexerConfig

{- | An indexer that keepAtLeast '_configKeepInMemory' events in memory
 and put the older one on disk by group of '_configFlushSize' events.
 The query interface for this indexer will always go through the database first and then prune
 results present in memory.

 @mem@ the indexer that handles old events, when we need to remove stuff from memory
 @store@ the indexer that handle the most recent events
-}
newtype MixedIndexer store mem event = MixedIndexer {_mixedWrapper :: IndexWrapper (MixedIndexerConfig store) mem event}

mkMixedIndexer
  :: Word
  -- ^ how many events are kept in memory after a flush
  -> Word
  -- ^ flush size
  -> store event
  -> mem event
  -> MixedIndexer store mem event
mkMixedIndexer keepNb flushNb db =
  MixedIndexer . IndexWrapper (MixedIndexerConfig keepNb flushNb mempty flushNb 0 db)

standardMixedIndexer
  :: IsSync m event store
  => Monad m
  => HasGenesis (Point event)
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

flushPoints :: Lens' (MixedIndexer store mem event) (Seq (Point event))
flushPoints = mixedWrapper . wrapperConfig . configFlushPoints

stepsBeforeNextFlush :: Lens' (MixedIndexer store mem event) Word
stepsBeforeNextFlush = mixedWrapper . wrapperConfig . configStepsBeforeNextFlush

currentMemorySize :: Lens' (MixedIndexer store mem event) Word
currentMemorySize = mixedWrapper . wrapperConfig . configCurrentMemorySize

class HasMixedConfig indexer where
  flushEvery :: Lens' (indexer event) Word
  keepInMemory :: Lens' (indexer event) Word

flushEveryVia :: HasMixedConfig indexer => Lens' s (indexer event) -> Lens' s Word
flushEveryVia l = l . flushEvery

keepInMemoryVia :: HasMixedConfig indexer => Lens' s (indexer event) -> Lens' s Word
keepInMemoryVia l = l . keepInMemory

-- Overlapping because we prefer this instance to any other instance
-- as it access the immediate config
instance {-# OVERLAPPING #-} HasMixedConfig (MixedIndexer store mem) where
  flushEvery = mixedWrapper . wrapperConfig . configFlushEvery
  keepInMemory = mixedWrapper . wrapperConfig . configKeepInMemory

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

inMemory :: Lens' (MixedIndexer store mem event) (mem event)
inMemory = mixedWrapper . wrappedIndexer

inDatabase :: Lens' (MixedIndexer store mem event) (store event)
inDatabase = mixedWrapper . wrapperConfig . configInDatabase

flushAt
  :: MixedIndexer store mem event
  -> Maybe (Point event, MixedIndexer store mem event)
flushAt indexer =
  let nextFlushDepth = indexer ^. keepInMemory + indexer ^. flushEvery

      reachFlushPoint = indexer ^. currentMemorySize >= nextFlushDepth

      dequeueNextFlushPoint =
        case indexer ^. flushPoints of
          Empty -> Nothing
          xs :|> p ->
            let indexer' =
                  indexer
                    & flushPoints .~ xs
                    & currentMemorySize -~ indexer ^. flushEvery
             in Just (p, indexer')
   in guard reachFlushPoint *> dequeueNextFlushPoint

startNewStep
  :: Point event
  -> MixedIndexer store mem event
  -> MixedIndexer store mem event
startNewStep p indexer =
  indexer
    & flushPoints %~ (p <|)
    & stepsBeforeNextFlush .~ (indexer ^. flushEvery)

tick
  :: Point event
  -> MixedIndexer store mem event
  -> (Maybe (Point event), MixedIndexer store mem event)
tick p indexer =
  let countEvent = (currentMemorySize +~ 1) . (stepsBeforeNextFlush -~ 1)

      adjustStep ix =
        if ix ^. stepsBeforeNextFlush == 0
          then startNewStep p ix
          else ix

      indexer' = adjustStep $ countEvent indexer
   in maybe (Nothing, indexer') (first Just) $ flushAt indexer'

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
      Nothing -> pure indexer'
      Just p -> flush p indexer''

instance IsSync event m mem => IsSync event m (MixedIndexer store mem) where
  lastSyncPoint = lastSyncPoint . view inMemory

instance
  ( Monad m
  , Rollbackable m event store
  )
  => Rollbackable m event (MixedIndexer store ListIndexer)
  where
  rollback p indexer = do
    indexer' <- inMemory (rollback p) indexer
    if null $ indexer' ^. inMemory . events
      then inDatabase (rollback p) indexer'
      else pure indexer'

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
