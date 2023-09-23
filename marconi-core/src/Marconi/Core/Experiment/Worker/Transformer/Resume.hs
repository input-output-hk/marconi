{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compute resuming information at the worker level
module Marconi.Core.Experiment.Worker.Transformer.Resume (
  Resume,
  withResume,
) where

import Control.Lens ((.=), (^.))
import qualified Control.Lens as Lens
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadState, join)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Marconi.Core.Experiment.Class as Class
import Marconi.Core.Experiment.Type (
  IndexerError (ResumingFailed),
  Point,
  ProcessedInput (Index, IndexAllDescending, Rollback, StableAt, Stop),
  point,
 )
import Marconi.Core.Experiment.Worker.Transformer (Transformer, compactInputs, transformerM)

{- | A data structure that keeps track of the progress of the chainSync relatively to the state of
this indexer.
-}
data ResumeState event = ResumeState
  { _hasStarted :: Bool
  -- ^ A flag that indicates if we've passed the first rollback
  , _lastPoint :: Point event
  -- ^ The last indexed point of this indexer (should be stable)
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
     , Class.HasGenesis (Point event)
     )
  => Point event
  -- ^ The last point of an indexer.
  -> Resume m event
withResume history = transformerM resumeStep (pure $ ResumeState False history)

resumeStep
  :: ( Ord (Point event)
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
    hasStarted .= True
    lastPoint' <- Lens.use lastPoint
    if (timedEvent ^. point) > lastPoint'
      then pure [Index timedEvent]
      else pure []
  Rollback p -> do
    lastPoint' <- Lens.use lastPoint
    hasStarted' <- Lens.use hasStarted
    if
      | p > lastPoint' -> pure [Rollback p]
      | hasStarted' -> throwError $ ResumingFailed "There was a rollback on a stable point"
      | otherwise -> pure []
  StableAt p -> pure [StableAt p]
  Stop -> pure [Stop]
