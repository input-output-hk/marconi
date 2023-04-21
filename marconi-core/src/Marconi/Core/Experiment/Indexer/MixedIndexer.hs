{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    An indexer that uses an indexer for recent events (usually in-memory)
    and another one for older events (usually on-disk).

    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Indexer.MixedIndexer
    ( Flushable (..)
    , MixedIndexer
        , newMixedIndexer
        , standardMixedIndexer
        , inMemory
        , inDatabase
    ) where

import Control.Lens (Lens', makeLenses, to, view)
import Control.Lens.Operators ((&), (.~), (^.))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Marconi.Core.Experiment.Class (Closeable (close), HasGenesis, IsIndex (index, indexAll), IsSync (lastSyncPoint),
                                      Queryable (query), ResumableResult (resumeResult), Rollbackable (rollback))
import Marconi.Core.Experiment.Indexer.ListIndexer (ListIndexer, events, latest, listIndexer)
import Marconi.Core.Experiment.Transformer.IndexWrapper (IndexWrapper (IndexWrapper), indexVia, wrappedIndexer,
                                                         wrapperConfig)
import Marconi.Core.Experiment.Type (Point, TimedEvent)

-- | Define a way to flush old events out of a container
class Flushable m indexer where

    -- | The events container used when you flush event
    type family Container (indexer :: * -> *) :: * -> *

    -- | Check if there isn't space left in memory
    currentLength :: indexer event -> m Word

    -- | Clear the memory and return its content
    flushMemory
        :: Word
           -- ^ How many event do we keep
        -> indexer event
        -> m (Container indexer (TimedEvent event), indexer event)

instance Applicative m => Flushable m ListIndexer where

    type instance Container ListIndexer = []

    -- | How many events are stored in the indexer
    currentLength ix = pure $ fromIntegral $ length (ix ^. events)

    -- | Flush a given number of events from the indexer
    flushMemory n ix = let

        (freshest, oldest) = splitAt (fromIntegral n) $ ix ^. events

        in pure (oldest, ix & events .~ freshest)

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

newMixedIndexer
    :: Word
    -- ^ how many events are kept in memory after a flush
    -> Word
    -- ^ flush size
    -> store event
    -> mem event
    -> MixedIndexer store mem event
newMixedIndexer flushNb keepNb db
    = MixedIndexer . IndexWrapper (MixedIndexerConfig flushNb keepNb db)

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
standardMixedIndexer flushNb keepNb db = do
    lSync <- lastSyncPoint db
    pure $ newMixedIndexer flushNb keepNb db (listIndexer & latest .~ lSync)

makeLenses ''MixedIndexer

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

        isFull indexer'
            = (indexer' ^. flushSize + indexer' ^. keepInMemory <)
            <$> indexer' ^. inMemory . to currentLength

        flushIfFull full = if full then flush else pure

        in do
        indexer' <- indexVia inMemory timedEvent indexer
        full <- isFull indexer'
        flushIfFull full indexer'

instance IsSync event m mem => IsSync event m (MixedIndexer store mem) where
    lastSyncPoint = lastSyncPoint . view inMemory

instance
    ( Monad m
    , Rollbackable m event store
    ) => Rollbackable m event (MixedIndexer store ListIndexer) where

    rollback p indexer = do
        indexer' <- inMemory (rollback p) indexer
        if not $ null $ indexer' ^. inMemory . events
            then pure indexer'
            else inDatabase (rollback p) indexer'

instance
    ( ResumableResult m event query ListIndexer
    , Queryable m event query store )
    => Queryable m event query (MixedIndexer store ListIndexer) where

    query valid q indexer
        = resumeResult valid q
            (indexer ^. inMemory)
            (query valid q (indexer ^. inDatabase))

instance
    ( Closeable m store
    , Closeable m mem
    , Monad m
    )
    => Closeable m (MixedIndexer store mem) where

    close indexer = do
       close $ indexer ^. inMemory
       close $ indexer ^. inDatabase

