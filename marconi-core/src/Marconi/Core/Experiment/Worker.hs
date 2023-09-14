{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

{- |
    Workers are wrapper around indexers that hide there type parameters.

    See 'Marconi.Core.Experiment' for documentation.
-}
module Marconi.Core.Experiment.Worker (
  WorkerIndexer (..),
  WorkerIndexerType,
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
import Control.Exception (SomeException (SomeException), catch, finally)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Core.Experiment.Class (
  Closeable (close),
  IsIndex (index, rollback),
  IsSync (lastSyncPoint),
 )
import Marconi.Core.Experiment.Type (
  IndexerError (OtherIndexError, StopIndexer),
  Point,
  Timed,
  point,
 )

-- | Worker which also provides direct access to the indexer hidden inside it.
data WorkerIndexer m input event indexer = WorkerIndexer
  { workerIndexerVar :: !(MVar (indexer event))
  , worker :: WorkerM m input (Point event)
  }

-- | Type alias for the type classes that are required to build a worker for an indexer
type WorkerIndexerType n event indexer =
  ( IsIndex n event indexer
  , IsSync n event indexer
  , Closeable n indexer
  )

{- | A worker hides the shape of an indexer and integrates the data needed to interact with a
coordinator.
-}
data WorkerM m input point = forall indexer event n.
  ( WorkerIndexerType n event indexer
  , Point event ~ point
  ) =>
  Worker
  { workerName :: Text
  -- ^ use to identify the worker in logs
  , workerState :: MVar (indexer event)
  -- ^ the indexer controlled by this worker
  , transformInput :: input -> m (Maybe event)
  -- ^ adapt the input event givent by the coordinator to the worker type
  , hoistError :: forall a. n a -> ExceptT IndexerError m a
  -- ^ adapt the monadic stack of the indexer to the one of the worker
  }

-- | A worker that operates in @IO@.
type Worker = WorkerM IO

-- | The different types of input of a worker
data ProcessedInput event
  = -- | A rollback happen and indexers need to go back to the given point in time
    Rollback (Point event)
  | -- | A new event has to be indexed
    Index (Timed (Point event) (Maybe event))
  | -- | Processing stops
    Stop

-- Create workers

-- | create a worker for an indexer, retuning the worker and the @MVar@ it's using internally
createWorker'
  :: (MonadIO f, WorkerIndexerType n event indexer)
  => (forall a. n a -> ExceptT IndexerError m a)
  -> Text
  -> (input -> m (Maybe event))
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorker' hoist name getEvent ix = liftIO $ do
  workerState <- Con.newMVar ix
  pure $ WorkerIndexer workerState $ Worker name workerState getEvent hoist

-- | create a worker for an indexer that doesn't throw error
createWorkerPure
  :: (MonadIO f, MonadIO m, WorkerIndexerType m event indexer)
  => Text
  -> (input -> m (Maybe event))
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorkerPure = createWorker' lift

-- | create a worker for an indexer that already throws IndexerError
createWorker
  :: (MonadIO f, WorkerIndexerType (ExceptT IndexerError m) event indexer)
  => Text
  -> (input -> m (Maybe event))
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorker = createWorker' id

mapIndex
  :: (Applicative f, Point event ~ Point event')
  => (event -> f (Maybe event'))
  -> ProcessedInput event
  -> f (ProcessedInput event')
mapIndex _ (Rollback p) = pure $ Rollback p
mapIndex f (Index timedEvent) =
  let mapEvent Nothing = pure Nothing
      mapEvent (Just e) = f e
   in Index <$> traverse mapEvent timedEvent
mapIndex _ Stop = pure Stop

{- | The worker notify its coordinator that it's ready
 and starts waiting for new events and process them as they come
-}
startWorker
  :: forall input m
   . (MonadIO m)
  => (Ord (Point input))
  => TChan (ProcessedInput input)
  -> MVar IndexerError
  -> QSemN
  -> QSemN
  -> Worker input (Point input)
  -> m ThreadId
startWorker chan errorBox endTokens tokens (Worker name ix transformInput hoistError) =
  let unlockCoordinator :: IO ()
      unlockCoordinator = Con.signalQSemN tokens 1

      notifyEndToCoordinator :: IO ()
      notifyEndToCoordinator = Con.signalQSemN endTokens 1

      fresherThan :: (Ord (Point event)) => Timed (Point event) (Maybe event) -> Point event -> Bool
      fresherThan evt p = evt ^. point > p

      indexEvent timedEvent = do
        Con.modifyMVar ix $ \indexer -> do
          result <- runExceptT $ do
            indexerLastPoint <- hoistError $ lastSyncPoint indexer
            if timedEvent `fresherThan` indexerLastPoint
              then hoistError $ index timedEvent indexer
              else pure indexer
          case result of
            Left err -> pure (indexer, Just err)
            Right res -> pure (res, Nothing)

      handleRollback :: Point input -> IO (Maybe IndexerError)
      handleRollback p = do
        Con.modifyMVar ix $ \indexer -> do
          result <- runExceptT $ hoistError $ rollback p indexer
          case result of
            Left err -> pure (indexer, Just err)
            Right res -> pure (res, Nothing)

      checkError :: IO (Maybe IndexerError)
      checkError = Con.tryReadMVar errorBox

      closeIndexer :: IO ()
      closeIndexer = do
        indexer <- Con.readMVar ix
        void $ runExceptT $ hoistError $ close indexer

      swallowPill :: IO ()
      swallowPill = finally closeIndexer notifyEndToCoordinator

      notifyCoordinatorOnError :: IndexerError -> IO ()
      notifyCoordinatorOnError e =
        -- We don't need to check if tryPutMVar succeed
        -- because if @errorBox@ is already full, our job is done anyway
        void $ Con.tryPutMVar errorBox e

      process = \case
        Rollback p -> handleRollback p
        Index e -> indexEvent e
        Stop -> pure $ Just $ OtherIndexError "Stop"

      safeProcessEvent :: ProcessedInput input -> IO (Maybe IndexerError)
      safeProcessEvent input = do
        processedInput <- mapIndex transformInput input
        process processedInput
          `catch` \(SomeException e) -> pure $ Just $ StopIndexer (Just $ name <> " " <> Text.pack (show e))

      loop :: TChan (ProcessedInput input) -> IO ()
      loop chan' =
        let loop' = do
              err <- checkError
              case err of
                Nothing -> do
                  event <- STM.atomically $ STM.readTChan chan'
                  result <- safeProcessEvent event
                  case result of
                    Nothing -> do
                      unlockCoordinator
                      loop'
                    Just err' -> do
                      notifyCoordinatorOnError err'
                      unlockCoordinator
                Just _ -> unlockCoordinator
         in loop'
   in liftIO $ do
        chan' <- STM.atomically $ STM.dupTChan chan
        Con.forkFinally (loop chan') (const swallowPill)
