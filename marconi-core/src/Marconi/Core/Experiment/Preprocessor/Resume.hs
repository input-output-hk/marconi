{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Resume indexing after a previous run: this preprocessor drain events up to the last stable
event indexed by an indexer
-}
module Marconi.Core.Experiment.Preprocessor.Resume (
  Resume,
  withResume,
) where

import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadState (get, put))
import Data.Foldable (Foldable (toList))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Marconi.Core.Experiment.Preprocessor (Preprocessor, preprocessorM)
import Marconi.Core.Experiment.Type (
  IndexerError (ResumingFailed),
  Point,
  ProcessedInput (Index, IndexAllDescending, Rollback, StableAt, Stop),
  Timed,
  point,
 )

{- | A data structure that keeps track of the progress of the chainSync relatively to the state of
this indexer.
-}
data ResumeState event
  = -- | The resuming hasn't started yet, we ignore any rollback and drain indexing befor the sync
    -- point
    ResumeStarts (Point event)
  | -- | We have drain at least one indexing operation, we continue to drain but when a rollback
    -- occurs, we must handle it
    ResumeOngoing (Point event)
  | -- Resuming is over, we forward any incoming
    ResumeDone

deriving instance (Show (Point event)) => Show (ResumeState event)

Lens.makeLenses ''ResumeState

{- | A type alias for a preprocessor that allows resuming.
We don't use a newtype to ease preprocessors composition.
-}
type Resume m event = Preprocessor m (Point event) event event

withResume
  :: ( MonadError IndexerError m
     , Ord (Point event)
     )
  => Point event
  -- ^ The last point of an indexer.
  -> Resume m event
withResume = preprocessorM (fmap toList . resumeStep) . (pure . ResumeStarts)

resumeStep
  :: ( Ord (Point event)
     , MonadState (ResumeState event) m
     , MonadError IndexerError m
     )
  => ProcessedInput (Point event) event
  -> m (Maybe (ProcessedInput (Point event) event))
resumeStep = \case
  IndexAllDescending timedEvents -> do
    currentState <- get
    case currentState of
      ResumeStarts p -> put (ResumeOngoing p) *> drainAllDescending p timedEvents
      ResumeOngoing p -> drainAllDescending p timedEvents
      ResumeDone -> pure . pure $ IndexAllDescending timedEvents
  Index timedEvent -> do
    currentState <- get
    case currentState of
      ResumeStarts p -> put (ResumeOngoing p) *> drainIndex p timedEvent
      ResumeOngoing p -> drainIndex p timedEvent
      ResumeDone -> pure . pure $ Index timedEvent
  Rollback p -> do
    currentState <- get
    case currentState of
      -- Ignore initial rollbacks
      ResumeStarts _ -> pure Nothing
      ResumeOngoing p' -> errorOnImmutableRollback p' p
      ResumeDone -> pure . pure $ Rollback p
  StableAt p -> pure . pure $ StableAt p
  Stop -> pure . pure $ Stop

errorOnImmutableRollback
  :: ( Ord (Point event)
     , MonadError IndexerError m
     )
  => Point event
  -> Point event
  -> m (Maybe (ProcessedInput (Point event) event))
errorOnImmutableRollback lastPoint rollbackTo
  | lastPoint == rollbackTo = pure Nothing
  | otherwise = throwError $ ResumingFailed "There was a rollback on a stable point"

drainAllDescending
  :: ( Ord (Point event)
     , MonadState (ResumeState event) m
     )
  => Point event
  -> NonEmpty (Timed (Point event) (Maybe event))
  -> m (Maybe (ProcessedInput (Point event) event))
drainAllDescending lastPoint timedEvents =
  case NonEmpty.span ((> lastPoint) . Lens.view point) timedEvents of
    ([], _) -> pure Nothing
    ([x], _) -> put ResumeDone $> Just (Index x)
    (x : xs, _) -> put ResumeDone $> Just (IndexAllDescending (x :| xs))

drainIndex
  :: ( Ord (Point event)
     , MonadState (ResumeState event) m
     )
  => Point event
  -> Timed (Point event) (Maybe event)
  -> m (Maybe (ProcessedInput (Point event) event))
drainIndex lastPoint timedEvent = do
  if (timedEvent ^. point) > lastPoint
    then put ResumeDone $> pure (Index timedEvent)
    else pure Nothing
