{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

{- |
    Workers are wrapper around indexers that hide there type parameters..

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Worker (
  WorkerIndexer,
  WorkerM (..),
  Worker,
  ProcessedInput (..),
  createWorker,
  createWorkerPure,
  createWorker',
  startWorker,
) where

import Control.Concurrent (MVar, QSemN, ThreadId)
import Control.Concurrent qualified as Con
import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))

import Control.Lens.Operators ((^.))
import Control.Monad (forever, void)
import Marconi.Core.Experiment.Class (
  Closeable (close),
  IsIndex (index),
  IsSync (lastSyncPoint),
  Rollbackable (rollback),
 )
import Marconi.Core.Experiment.Type (IndexerError, Point, Timed (Timed), event, point)

-- Type alias for the type classes that are required to build a worker for an indexer
type WorkerIndexer n event indexer =
  ( IsIndex n event indexer
  , IsSync n event indexer
  , Rollbackable n event indexer
  , Closeable n indexer
  )

data WorkerM m input point = forall indexer event n.
  ( WorkerIndexer n event indexer
  , Point event ~ point
  ) =>
  Worker
  { workerState :: MVar (indexer event)
  -- ^ the indexer controlled by this worker
  , transformInput :: input -> m event
  -- ^ used by the worker to check whether an input is a rollback or an event
  , hoistError :: forall a. n a -> ExceptT IndexerError m a
  -- ^ used by the worker to check whether an input is a rollback or an event
  , errorBox :: MVar IndexerError
  -- ^ a place where the worker places error that it can't handle,
  -- to notify the coordinator
  }

type Worker = WorkerM IO

-- | The different types of input of a worker
data ProcessedInput event
  = -- | A rollback happen and indexers need to go back to the given point in time
    Rollback (Point event)
  | -- | A new event has to be indexed
    Index (Timed (Point event) event)

-- Create workers

-- | create a worker for an indexer, retuning the worker and the @MVar@ it's using internally
createWorker'
  :: (MonadIO m, WorkerIndexer n event indexer)
  => (forall a. n a -> ExceptT IndexerError m a)
  -> (input -> m event)
  -> indexer event
  -> m (MVar (indexer event), WorkerM m input (Point event))
createWorker' hoist getEvent ix = do
  workerState <- liftIO $ Con.newMVar ix
  errorBox <- liftIO Con.newEmptyMVar
  pure (workerState, Worker workerState getEvent hoist errorBox)

-- | create a worker for an indexer that doesn't throw error
createWorkerPure
  :: (MonadIO m, WorkerIndexer m event indexer)
  => (input -> m event)
  -> indexer event
  -> m (MVar (indexer event), WorkerM m input (Point event))
createWorkerPure = createWorker' lift

-- | create a worker for an indexer that already throws IndexerError
createWorker
  :: (MonadIO m, WorkerIndexer (ExceptT IndexerError m) event indexer)
  => (input -> m event)
  -> indexer event
  -> m (MVar (indexer event), WorkerM m input (Point event))
createWorker = createWorker' id

mapIndex
  :: Applicative f
  => Point event ~ Point event'
  => (event -> f event')
  -> ProcessedInput event
  -> f (ProcessedInput event')
mapIndex _ (Rollback p) = pure $ Rollback p
mapIndex f (Index timedEvent) = Index . Timed (timedEvent ^. point) <$> f (timedEvent ^. event)

{- | The worker notify its coordinator that it's ready
 and starts waiting for new events and process them as they come
-}
startWorker
  :: MonadIO m
  => Ord (Point input)
  => TChan (ProcessedInput input)
  -> QSemN
  -> Worker input (Point input)
  -> m ThreadId
startWorker chan tokens (Worker ix transformInput hoistError errorBox) =
  let unlockCoordinator :: IO ()
      unlockCoordinator = do
        Con.signalQSemN tokens 1

      fresherThan :: Ord (Point event) => Timed (Point event) event -> Point event -> Bool
      fresherThan evt p = evt ^. point > p

      indexEvent timedEvent = Con.modifyMVar_ ix $ \indexer -> do
        result <- runExceptT $ do
          indexerLastPoint <- hoistError $ lastSyncPoint indexer
          if timedEvent `fresherThan` indexerLastPoint
            then hoistError $ index timedEvent indexer
            else pure indexer
        either (raiseError indexer) pure result

      raiseError indexer err = do
        -- We don't need to check if tryPutMVar succeed
        -- because if @errorBox@ is already full, our job is done anyway
        void $ Con.tryPutMVar errorBox err
        pure indexer

      handleRollback p = do
        Con.modifyMVar_ ix $ \indexer -> do
          result <- runExceptT $ hoistError $ rollback p indexer
          either (raiseError indexer) pure result

      swallowPill = do
        indexer <- Con.readMVar ix
        void $ runExceptT $ hoistError $ close indexer

      loop chan' = forever $ do
        input <- STM.atomically $ STM.readTChan chan'
        processedEvent <- mapIndex transformInput input
        case processedEvent of
          Rollback p -> handleRollback p
          Index e -> indexEvent e
        unlockCoordinator
   in liftIO $ do
        chan' <- STM.atomically $ STM.dupTChan chan
        Con.forkFinally (loop chan') (const swallowPill)
