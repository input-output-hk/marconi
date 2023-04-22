{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE StrictData    #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
    A transformer that delay insertion of events into an indexer.

    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Transformer.WithDelay
    ( WithDelay
        , withDelay
        , delayedIndexer
        , delayCapacity
        , delayLength
        , delayBuffer
    ) where

import Control.Lens (Lens', makeLenses, view)
import Control.Lens.Operators ((%~), (&), (+~), (.~), (^.))
import Data.Sequence (Seq (Empty, (:|>)), (<|))
import Data.Sequence qualified as Seq

import Marconi.Core.Experiment.Class (Closeable, IsIndex (index), IsSync, Queryable, Resetable (reset),
                                      Rollbackable (rollback))
import Marconi.Core.Experiment.Transformer.IndexWrapper (IndexWrapper (IndexWrapper), indexVia, resetVia, rollbackVia,
                                                         wrappedIndexer, wrapperConfig)
import Marconi.Core.Experiment.Type (TimedEvent, point)

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
    instance Closeable m indexer => Closeable m (WithDelay indexer)

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

resetBuffer :: WithDelay indexer event -> WithDelay indexer event
resetBuffer = (delayLength .~ 0) . (delayBuffer .~ Seq.empty)

instance
    ( Monad m
    , Rollbackable m event indexer
    ) => Rollbackable m event (WithDelay indexer) where

    rollback p indexer = let

        rollbackWrappedIndexer p' = rollbackVia delayedIndexer p' indexer

        before =  Seq.dropWhileL ((> p) . view point) $ indexer ^. delayBuffer

        in if Seq.null before
           -- if we empty the delay buffer,
           -- some events in the wrapped indexer may need a rewrite
           then resetBuffer <$> rollbackWrappedIndexer p
           else pure $ indexer
                   & delayBuffer .~ before
                   & delayLength .~ fromIntegral (Seq.length before)

instance
    ( Applicative m
    , Resetable m event indexer
    ) => Resetable m event (WithDelay indexer) where
    reset indexer = do
        indexer' <- resetVia delayedIndexer indexer
        pure $ resetBuffer indexer'

