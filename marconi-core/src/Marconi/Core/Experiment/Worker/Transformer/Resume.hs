{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compute resuming information at the worker level
module Marconi.Core.Experiment.Worker.Transformer.Resume (
  Resume,
  withResume,
) where

import Control.Lens ((?=), (^.))
import qualified Control.Lens as Lens
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadState, join, modify)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Marconi.Core.Experiment.Class as Class
import Marconi.Core.Experiment.Transformer.WithResume (
  OrdPoint (comparePoint),
  PointCompare (After, Before, Fork, Same),
  PointStatus (OngoingResuming, ResumingEnded),
 )
import Marconi.Core.Experiment.Type (
  IndexerError (ResumingFailed),
  Point,
  ProcessedInput (Index, IndexAllDescending, Rollback, StableAt, Stop),
  Timed,
  point,
 )
import Marconi.Core.Experiment.Worker.Transformer (Transformer, compactInputs, transformerM)

{- | A data structure that keeps track of the progress of the chainSync relatively to the state of
this indexer.
-}
data ResumeState event = ResumeState
  { _hasStarted :: Bool
  -- ^ A flag that indicates if we've passed the first rollback
  , _lastPassed :: Maybe (Point event)
  -- ^ If we've started to pass the rollbackable points of the indexer, this is the last one we've
  --   encountered
  , _lastPoints :: [Point event]
  -- ^ The unstable point stored for this indexer
  }

deriving instance (Show (Point event)) => Show (ResumeState event)

Lens.makeLenses ''ResumeState

{- | A type alias for a transformer that allows resuming.
We don't use a newtype to ease transformers composition.
-}
type Resume m event = Transformer m (Point event) event event

withResume
  :: ( MonadError IndexerError m
     , Ord (Point event)
     , OrdPoint (Point event)
     , Class.HasGenesis (Point event)
     )
  => [Point event]
  -- ^ The last points of an indexer.
  -- The points need to be sorted in chronological order.
  -- The first point at least must be an immutable point
  -- (if at least one point on chain is immutable).
  -> Resume m event
withResume history = transformerM resumeStep (pure $ ResumeState False Nothing history)

decidePointStatus :: (OrdPoint point) => point -> [point] -> PointStatus
decidePointStatus _ [] = ResumingEnded
decidePointStatus p (nextPoint : _) = OngoingResuming $ comparePoint p nextPoint

drainNextLastPoint :: ResumeState event -> ResumeState event
drainNextLastPoint r@(ResumeState _ _ []) = r
drainNextLastPoint (ResumeState _ _ (x : xs)) = ResumeState True (Just x) xs

-- | Clean the last point, usually because the volatile part of an indexer is no longer valid
wipeHistory :: ResumeState event -> ResumeState event
wipeHistory (ResumeState _ last' _) = ResumeState True last' []

{- | rollback the resuming indexer, and then potentially index an event.
It implies that we go out of the indexing mode.
-}
rollbackToAndInsert
  :: (MonadState (ResumeState event) m)
  => Point event
  -> Maybe (Timed (Point event) (Maybe event))
  -> m [ProcessedInput (Point event) event]
rollbackToAndInsert p potentialNext = do
  modify wipeHistory
  pure $ Rollback p : maybe [] (pure . Index) potentialNext

resumeStep
  :: ( Ord (Point event)
     , OrdPoint (Point event)
     , Class.HasGenesis (Point event)
     , MonadState (ResumeState event) m
     , MonadError IndexerError m
     )
  => ProcessedInput (Point event) event
  -> m [ProcessedInput (Point event) event]
resumeStep = \case
  IndexAllDescending timedEvents -> do
    fmap (compactInputs . join) . traverse resumeStep $
      Index <$> NonEmpty.toList (NonEmpty.reverse timedEvents)
  Index timedEvent -> do
    lastPassed' <- Lens.use lastPassed
    lastPoints' <- Lens.use lastPoints
    let invalidHistoryError =
          ResumingFailed
            "The impossible happened: Indexer history is not compatible with the ledger"
    case decidePointStatus (timedEvent ^. point) lastPoints' of
      -- We have exhausted the sync list, we're past the resume point and we can index normally
      ResumingEnded -> do
        pure [Index timedEvent]
      -- the point is before the next point we're waiting
      OngoingResuming Before -> do
        case lastPassed' of
          -- we haven't process any point yet, we're in a draining phase, so we skip the event
          -- TODO This part is not currently tested by the WithResume test. We need to do so.
          -- Ideally, this would be done when we support adding indexers dynamically using
          -- this transformer.
          Nothing -> do
            pure []
          -- we have processed an event, so the next one should match what we have
          -- as a consequence, we rollback and then index this new event.
          -- Here is an example of how this can happen:
          --   1. you got two blocks, one at slot 200, the next at 220
          --   2. the indexer stops, while offline the node rollbacks and replace the block at
          --   slot 220 by a new block, issued at 210
          --   3. on resume, you correctly received the block 200
          --   4. then you were expecting a block at slot 220 but received one with a earlier slot
          --   the only reason is that the node has handled a rollback, we do the same
          Just p -> do
            rollbackToAndInsert p (Just timedEvent)
      -- we found our last sync point, so we mark it as passed and we continue draining
      OngoingResuming Same -> do
        modify drainNextLastPoint $> []
      -- we found an unexpected next point, so we rollbackTo the previous one stop draining
      OngoingResuming After -> do
        case lastPassed' of
          Nothing -> throwError invalidHistoryError
          -- Here is an example of how this can happen (similar scenario to the 'Before' case):
          --   1. you got two blocks, one at slot 200, the next at 210
          --   2. the indexer stops, while offline the node rollbacks and replace the block at
          --   slot 210 by a new block, issued at 220
          --   3. on resume, you correctly received the block 200
          --   4. then you were expecting a block at slot 2'0 but received one with a more recent
          --   slot the only reason is that the node has handled a rollback, we do the same
          Just p -> rollbackToAndInsert p (Just timedEvent)
      -- we found an unexpected next point, so we rollbackTo the previous one stop draining
      OngoingResuming Fork -> do
        rollbackToAndInsert (fromMaybe Class.genesis lastPassed') (Just timedEvent)
  Rollback p -> do
    lastPoints' <- Lens.use lastPoints
    hasStarted' <- Lens.use hasStarted
    -- When we start, we look if the starting point is after the first point returned by our
    -- indexer.
    -- If so, we rollback to the given point, otherwise we try to keep the content we have.
    --
    let onInitialRollback = case decidePointStatus p lastPoints' of
          -- if 'lastPoints' is empty, it means that we're already at genesis, we keep it as is
          -- ResumingEnded -> pure indexer
          ResumingEnded -> do
            pure []
          -- the resuming starts before the lastPoints of this indexer, we start draining
          OngoingResuming Before -> do
            lastPassed ?= p
            pure []
          -- the resuming starts on the first stable point of the indexer, we pass it
          OngoingResuming Same -> do
            modify drainNextLastPoint
            pure []
          -- The resuming point selection shouldn't start with a fork
          OngoingResuming Fork -> do
            throwError $ ResumingFailed "The initial resume point is a fork"
          -- otherwise it means that the rollback is part of our resume points, so we do perform
          -- the rollback
          OngoingResuming After -> do
            throwError $ ResumingFailed "The initial resume point is later than what we know"
        -- If a rollback occurs after startup, we need to take it into account
        onLaterRollback = do
          rollbackToAndInsert p Nothing
     in if hasStarted' then onLaterRollback else onInitialRollback
  StableAt p -> pure [StableAt p]
  Stop -> pure [Stop]
