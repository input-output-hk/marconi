{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
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
  resumedIndexer,
  OrdPoint (comparePoint),
  PointStatus (..),
  PointCompare (..),
) where

import Control.Lens ((&), (.~), (^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (MonadError (throwError), MonadIO)
import Marconi.Core.Experiment.Class (IsSync (lastStablePoint))
import Marconi.Core.Experiment.Class qualified as Class
import Marconi.Core.Experiment.Indexer.SQLiteAggregateQuery (HasDatabasePath)
import Marconi.Core.Experiment.Transformer.Class (IndexerTrans (unwrap))
import Marconi.Core.Experiment.Transformer.IndexTransformer (setLastStablePointVia)
import Marconi.Core.Experiment.Transformer.IndexTransformer qualified as Wrapper
import Marconi.Core.Experiment.Type (
  IndexerError (ResumingFailed),
  Point,
  point,
 )

{- | A data structure that keeps track of the progress of the chainSync relatively to the state of
this indexer.
-}
data ResumeState event = ResumeState
  { _stateLastStablePoint :: Point event
  -- ^ The unstable point stored for this indexer
  , _stateHasStarted :: Bool
  }

deriving instance (Show (Point event)) => Show (ResumeState event)

Lens.makeLenses ''ResumeState

-- | An indexer transformer that adds the resuming capability to an indexer
newtype WithResume indexer event = WithResume {_resumeWrapper :: Wrapper.IndexTransformer ResumeState indexer event}

Lens.makeLenses 'WithResume

-- | A smart constructor for the @WithResume@ transformer
withResume
  :: (Applicative m, IsSync m event indexer)
  => -- \^ A way to extract the last points of an indexer.
  -- The points need to be sorted in chronological order.
  indexer event
  -- ^ the underlying indexer
  -> m (WithResume indexer event)
withResume indexer = do
  history <- lastStablePoint indexer
  pure $ WithResume $ Wrapper.IndexTransformer (ResumeState history False) indexer

deriving via
  (Wrapper.IndexTransformer ResumeState indexer)
  instance
    (Class.IsSync m event indexer) => Class.IsSync m event (WithResume indexer)

deriving via
  (Wrapper.IndexTransformer ResumeState indexer)
  instance
    (HasDatabasePath indexer) => HasDatabasePath (WithResume indexer)

deriving via
  (Wrapper.IndexTransformer ResumeState indexer)
  instance
    (Class.Closeable m indexer) => Class.Closeable m (WithResume indexer)

deriving via
  (Wrapper.IndexTransformer ResumeState indexer)
  instance
    (Class.Queryable m event query indexer) => Class.Queryable m event query (WithResume indexer)

resumedIndexer :: Lens.Lens' (WithResume indexer event) (indexer event)
resumedIndexer = resumeWrapper . Wrapper.wrappedIndexer

resumeState :: Lens.Lens' (WithResume indexer event) (ResumeState event)
resumeState = resumeWrapper . Wrapper.wrapperConfig

lastPoint :: Lens.Lens' (WithResume indexer event) (Point event)
lastPoint = resumeState . stateLastStablePoint

hasStarted :: Lens.Lens' (WithResume indexer event) Bool
hasStarted = resumeState . stateHasStarted

instance IndexerTrans WithResume where
  unwrap = resumedIndexer

-- | A comparison for points, quite similar to 'Ordering' but add a way to identify forks
data PointCompare
  = Fork
  | Before
  | Same
  | After

data PointStatus = ResumingEnded | OngoingResuming PointCompare

class OrdPoint point where
  comparePoint :: point -> point -> PointCompare

instance
  ( Class.IsIndex m event indexer
  , Class.HasGenesis (Point event)
  , MonadError IndexerError m
  , OrdPoint (Point event)
  , Ord (Point event)
  , MonadIO m
  )
  => Class.IsIndex m event (WithResume indexer)
  where
  index timedEvent indexer =
    if (timedEvent ^. point) > (indexer ^. lastPoint)
      then Wrapper.indexVia unwrap timedEvent (indexer & hasStarted .~ True)
      else pure indexer

  rollback p indexer
    | p > (indexer ^. lastPoint) = Wrapper.rollbackVia unwrap p indexer
    | indexer ^. hasStarted = throwError $ ResumingFailed "There was a rollback on a stable point"
    | otherwise = do pure indexer

  setLastStablePoint = setLastStablePointVia unwrap
