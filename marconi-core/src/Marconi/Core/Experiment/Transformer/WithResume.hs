{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
 Add a resuming mechanism to an indexer.

 Until we reach the last sync point, the indexer will compare the 'Point' with the one it has stored.

       * If the event is before any of these point, it is skipped
       * If it is the same as an existing point, we skip it
       * If it is a different point but at a time covered by the stored point, we rollback to it
         and resume indexing
       * If we pass the lastSyncPoint, we resume to a standard indexing.
-}
module Marconi.Core.Experiment.Transformer.WithResume (
  WithResume,
  withResume,
  HasResumePoints (resumePoints),
) where

import Control.Lens ((%~), (^.))
import Control.Lens qualified as Lens
import Control.Monad ((<=<))
import Control.Monad.Except (MonadError (throwError))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Marconi.Core.Experiment.Class qualified as Class
import Marconi.Core.Experiment.Transformer.IndexWrapper qualified as Wrapper
import Marconi.Core.Experiment.Type (
  IndexerError (InvalidIndexer, ResumingFailed),
  Point,
  Timed,
  point,
 )

{- | A data structure that keeps track of the progress of the chainSync relatively to the state of
this indexer.
-}
data ResumeState event = ResumeState
  { _stateHasStarted :: Bool
  -- ^ A flag that indicates if we've passed the first rollback
  , _stateLastPassed :: Maybe (Point event)
  -- ^ If we've started to pass the rollbackable points of the indexer, this is the last one we've
  --   encountered
  , _stateLastPoints :: [Point event]
  -- ^ The unstable point stored for this indexer
  }

Lens.makeLenses ''ResumeState

-- | An indexer transformer that adds the resuming capability to an indexer
newtype WithResume indexer event = WithResume {_resumeWrapper :: Wrapper.IndexWrapper ResumeState indexer event}

Lens.makeLenses 'WithResume

-- | A smart constructor for the @WithResume@ transformer
withResume
  :: (Applicative m)
  => (Word -> indexer event -> m [Point event])
  -- ^ A way to extracte the last points of an indexer
  -> Word
  -- ^ SecurityParam
  -> indexer event
  -- ^ the underlying indexer
  -> m (WithResume indexer event)
withResume getHistory securityParam indexer = do
  history <- getHistory securityParam indexer
  pure $ WithResume $ Wrapper.IndexWrapper (ResumeState False Nothing history) indexer

class HasResumePoints event indexer where
  resumePoints :: indexer event -> Maybe [Point event]

-- The overlapping instance means that this one take precedence over the other overlappable
-- instances.
-- As a consequence, if we use several @WithResume@ transformers on the same indexer
-- (please, don't do this, it's wrong) 'resumePoints' will target the configuration of the most
-- external one.
instance {-# OVERLAPPING #-} HasResumePoints event (WithResume indexer) where
  resumePoints = Just . Lens.view lastPoints

instance
  {-# OVERLAPPABLE #-}
  (Wrapper.IndexerTrans t, HasResumePoints event indexer)
  => HasResumePoints event (t indexer)
  where
  resumePoints = resumePoints . Lens.view Wrapper.unwrap

deriving via
  (Wrapper.IndexWrapper ResumeState indexer)
  instance
    (Class.IsSync m event indexer) => Class.IsSync m event (WithResume indexer)

deriving via
  (Wrapper.IndexWrapper ResumeState indexer)
  instance
    (Class.Closeable m indexer) => Class.Closeable m (WithResume indexer)

deriving via
  (Wrapper.IndexWrapper ResumeState indexer)
  instance
    (Class.Queryable m event query indexer) => Class.Queryable m event query (WithResume indexer)

resumeIndexer :: Lens.Lens' (WithResume indexer event) (indexer event)
resumeIndexer = resumeWrapper . Wrapper.wrappedIndexer

resumeConfig :: Lens.Lens' (WithResume indexer event) (ResumeState event)
resumeConfig = resumeWrapper . Wrapper.wrapperConfig

lastPoints :: Lens.Lens' (WithResume indexer event) [Point event]
lastPoints = resumeConfig . stateLastPoints

lastPassed :: Lens.Lens' (WithResume indexer event) (Maybe (Point event))
lastPassed = resumeConfig . stateLastPassed

hasStarted :: Lens.Lens' (WithResume indexer event) Bool
hasStarted = resumeConfig . stateHasStarted

instance Wrapper.IndexerTrans WithResume where
  type Config WithResume = ResumeState
  wrap cfg = WithResume . Wrapper.IndexWrapper cfg
  unwrap = resumeIndexer

-- | A comparison for points, quite similar to 'Ordering' but add a way to identify forks
data PointCompare
  = Fork
  | Before
  | Same
  | After

data PointStatus = ResumingEnded | OngoingResuming PointCompare

decidePointStatus :: (OrdPoint point) => point -> [point] -> PointStatus
decidePointStatus _ [] = ResumingEnded
decidePointStatus p (nextPoint : _) = OngoingResuming $ comparePoint p nextPoint

class OrdPoint point where
  comparePoint :: point -> point -> PointCompare

-- | clean the last point, usually because the volatile part of an indexer is no longer valid
wipeHistory :: ResumeState event -> ResumeState event
wipeHistory ResumeState{} = ResumeState True Nothing []

drainNextLastPoint :: ResumeState event -> ResumeState event
drainNextLastPoint r@(ResumeState _ _ []) = r
drainNextLastPoint (ResumeState _ _ (x : xs)) = ResumeState True (Just x) xs

{- | rollback the resuming indexer, and then potentially index an event.
It implies that we go out of the indexing mode.
-}
rollbackToAndInsert
  :: ( Class.IsIndex m event indexer
     , Ord (Point event)
     )
  => Point event
  -> Maybe (Timed (Point event) (Maybe event))
  -> WithResume indexer event
  -> m (WithResume indexer event)
rollbackToAndInsert p potentialNext indexer =
  maybe pure (Wrapper.indexVia Wrapper.unwrap) potentialNext
    <=< Wrapper.rollbackVia Wrapper.unwrap p
    $ indexer & resumeConfig %~ wipeHistory

instance
  ( Class.IsIndex m event indexer
  , Class.HasGenesis (Point event)
  , MonadError IndexerError m
  , OrdPoint (Point event)
  , Ord (Point event)
  )
  => Class.IsIndex m event (WithResume indexer)
  where
  index timedEvent indexer =
    let lastPassed' = fromMaybe Class.genesis $ indexer ^. lastPassed
        invalidHistory =
          InvalidIndexer
            "The impossible happened: Indexer history is not compatible with the ledger"
     in case decidePointStatus (timedEvent ^. point) (indexer ^. lastPoints) of
          -- We have exhausted the sync list, we're past the resume point and we can index normally
          ResumingEnded -> Wrapper.indexVia Wrapper.unwrap timedEvent indexer
          -- the point is before the next point we're waiting
          OngoingResuming Before -> case indexer ^. lastPassed of
            -- we haven't process any point yet, we're in a draining phase, so we skip the event
            Nothing -> pure indexer
            -- we have processed an event, so the next one should match what we have
            -- as a consequence, we rollback and then index this new event.
            -- Here is an example of how this can happen:
            --   1. you got two blocks, one at slot 200, the next at 220
            --   2. the indexer stops, while offline the node rollbacks and replace the block at
            --   slot 220 by a new block, issued at 210
            --   3. on resume, you correctly received the block 200
            --   4. then you were expecting a block at slot 220 but received one with a earlier slot
            --   the only reason is that the node has handled a rollback, we do the same
            Just p -> rollbackToAndInsert p (Just timedEvent) indexer
          -- we found our last sync point, so we mark it as passed and we continue draining
          OngoingResuming Same -> pure $ indexer & resumeConfig %~ drainNextLastPoint
          -- we found an unexpected next point, so we rollbackTo the previous one stop draining
          OngoingResuming After -> case indexer ^. lastPassed of
            Nothing -> throwError invalidHistory
            -- Here is an example of how this can happen (similar scenario to the 'Before' case):
            --   1. you got two blocks, one at slot 200, the next at 210
            --   2. the indexer stops, while offline the node rollbacks and replace the block at
            --   slot 210 by a new block, issued at 220
            --   3. on resume, you correctly received the block 200
            --   4. then you were expecting a block at slot 2'0 but received one with a more recent
            --   slot the only reason is that the node has handled a rollback, we do the same
            Just p -> rollbackToAndInsert p (Just timedEvent) indexer
          -- we found an unexpected next point, so we rollbackTo the previous one stop draining
          OngoingResuming Fork -> rollbackToAndInsert lastPassed' (Just timedEvent) indexer

  rollback p indexer =
    -- When we start, we look if the starting point is after the first point returned by our
    -- indexer.
    -- If so, we rollback to the given point, otherwise we try to keep the content we have.
    let onInitialRollback = case decidePointStatus p (indexer ^. lastPoints) of
          -- if 'lastPoints' is empty, it means that we're already at genesis, we keep it as is
          ResumingEnded -> pure indexer
          -- the resuming starts before the lastPoints of this indexer, we start draining
          OngoingResuming Before -> pure indexer
          -- the resuming starts on the first stable point of the indexer, we pass it
          OngoingResuming Same -> pure $ indexer & resumeConfig %~ drainNextLastPoint
          -- The resuming point selection shouldn't start with a fork
          OngoingResuming Fork -> throwError $ ResumingFailed "The initial resume point is a fork"
          -- otherwise it means that the rollback is part of our resume points, so we do perform
          -- the rollback
          OngoingResuming After -> rollbackToAndInsert p Nothing indexer
        -- If a rollback occurs after startup, we need to take it into account
        onLaterRollback = rollbackToAndInsert p Nothing indexer
     in if indexer ^. hasStarted then onInitialRollback else onLaterRollback
