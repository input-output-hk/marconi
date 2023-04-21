{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
    A transformer that cache result of some queries

    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Transformer.WithPruning
    ( Prunable (..)
    , pruneVia
    , pruningPointVia
    , WithPruning
        , withPruning
        , prunedIndexer
        , securityParam
        , pruneEvery
        , nextPruning
        , stepsBeforeNext
        , currentDepth
    ) where

import Control.Lens (Getter, Lens', makeLenses, set, view)
import Control.Lens.Operators ((%~), (&), (+~), (-~), (.~), (^.))
import Control.Monad (guard, unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Trans (MonadTrans)
import Data.Bifunctor (Bifunctor (first))
import Data.Sequence (Seq (Empty, (:|>)), (<|))
import Data.Sequence qualified as Seq

import Marconi.Core.Experiment.Class (Closeable, HasGenesis, IsIndex (index), IsSync, Queryable, Resetable (reset),
                                      Rollbackable (rollback))
import Marconi.Core.Experiment.Indexer.MixedIndexer (MixedIndexer, inDatabase)
import Marconi.Core.Experiment.Transformer.IndexWrapper (IndexWrapper (IndexWrapper), indexVia, resetVia, rollbackVia,
                                                         wrappedIndexer, wrapperConfig)
import Marconi.Core.Experiment.Transformer.WithCache (WithCache, cachedIndexer)
import Marconi.Core.Experiment.Transformer.WithDelay (WithDelay, delayedIndexer)
import Marconi.Core.Experiment.Transformer.WithTracer (WithTracer, tracedIndexer)
import Marconi.Core.Experiment.Type (IndexerError (RollbackBehindHistory), Point, point)

-- | The indexer can prune old data.
-- The main purpose is to speed up query processing.
-- If the indexer is 'Rollbackable' and 'Prunable',
-- it can't 'rollback' behind the 'pruningPoint',
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


instance (Functor m, Prunable m event indexer)
    => Prunable m event (WithTracer m indexer) where

    prune = pruneVia tracedIndexer

    pruningPoint = pruningPointVia tracedIndexer

instance
    ( MonadTrans t
    , Monad m
    , Monad (t m)
    , Prunable (t m) event indexer
    ) => Prunable (t m) event (WithTracer m indexer) where

    prune = pruneVia tracedIndexer

    pruningPoint = pruningPointVia tracedIndexer


instance (Functor m, Prunable m event indexer)
    => Prunable m event (WithDelay indexer) where

    prune = pruneVia delayedIndexer

    pruningPoint = pruningPointVia delayedIndexer


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

deriving via (IndexWrapper PruningConfig indexer)
    instance Closeable m indexer => Closeable m (WithPruning indexer)

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


-- | The rollbackable instance for `WithPruning` is a defensive heuristic
-- that may provide a non optimal behaviour but ensure that we don't
-- mess up with the rollbackable events.
instance
    ( Monad m
    , MonadError IndexerError m
    , Prunable m event indexer
    , Rollbackable m event indexer
    , HasGenesis (Point event)
    , Ord (Point event)
    ) => Rollbackable m event (WithPruning indexer) where

    rollback p indexer = let

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
                <$> rollbackVia prunedIndexer p indexer

instance
    ( Monad m
    , Prunable m event indexer
    , Resetable m event indexer
    , HasGenesis (Point event)
    , Ord (Point event)
    ) => Resetable m event (WithPruning indexer) where

    reset indexer = do
       indexer' <- resetVia prunedIndexer indexer
       pure $ indexer'
           & nextPruning .~ mempty
           & currentDepth .~ 0


instance (Functor m, Prunable m event indexer)
    => Prunable m event (WithCache query indexer) where

    prune = pruneVia cachedIndexer

    pruningPoint = pruningPointVia cachedIndexer

instance (Functor m, Prunable m event store)
    => Prunable m event (MixedIndexer store mem) where

    prune = pruneVia inDatabase

    pruningPoint = pruningPointVia inDatabase
