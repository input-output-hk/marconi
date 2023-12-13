{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

{- |
    Workers are wrapper around indexers that hide there type parameters.

    See 'Marconi.Core' for documentation.
-}
module Marconi.Core.Worker (
  WorkerIndexer (..),
  WorkerIndexerType,
  WorkerM (..),
  Worker,
  createWorker,
  createWorkerPure,
  createWorkerHoist,
  createWorkerWithPreprocessing,
  startWorker,
) where

import Control.Concurrent (MVar, QSemN, ThreadId)
import Control.Concurrent qualified as Con
import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException (SomeException), catch, finally)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Strict (
  MonadIO (liftIO),
  MonadTrans (lift),
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Core.Class (
  Closeable (close),
  IsIndex (index, indexAllDescending, rollback, setLastStablePoint),
  IsSync,
 )
import Marconi.Core.Preprocessor (Preprocessor, mapMaybeEvent, runPreprocessor)
import Marconi.Core.Type (
  IndexerError (OtherIndexError, StopIndexer),
  Point,
  ProcessedInput (Index, IndexAllDescending, Rollback, StableAt, Stop),
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
  , transformInput :: Preprocessor (ExceptT IndexerError m) point input event
  -- ^ adapt the input event given by the coordinator to the worker type
  , hoistError :: forall a. n a -> ExceptT IndexerError m a
  -- ^ adapt the monadic stack of the indexer to the one of the worker
  }

-- | A worker that operates in @IO@.
type Worker = WorkerM IO

-- Create workers

-- | create a worker for an indexer, retuning the worker and the @MVar@ it's using internally
createWorkerHoist
  :: (MonadIO f, WorkerIndexerType n event indexer)
  => (forall a. n a -> ExceptT IndexerError m a)
  -> Text
  -> Preprocessor (ExceptT IndexerError m) (Point event) input event
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorkerHoist hoist name f ix = liftIO $ do
  workerState <- Con.newMVar ix
  pure $ WorkerIndexer workerState $ Worker name workerState f hoist

-- | create a worker for an indexer that already throws IndexerError
createWorkerWithPreprocessing
  :: (MonadIO f, WorkerIndexerType (ExceptT IndexerError m) event indexer)
  => Text
  -> Preprocessor (ExceptT IndexerError m) (Point event) input event
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorkerWithPreprocessing = createWorkerHoist id

-- | create a worker for an indexer that doesn't throw error
createWorkerPure
  :: (MonadIO f, MonadIO m, WorkerIndexerType m event indexer)
  => Text
  -> Preprocessor (ExceptT IndexerError m) (Point event) input event
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorkerPure = createWorkerHoist lift

-- | create a worker for an indexer that already throws IndexerError
createWorker
  :: (MonadIO f, WorkerIndexerType (ExceptT IndexerError m) event indexer)
  => Text
  -> (input -> Maybe event)
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorker name = createWorkerWithPreprocessing name . mapMaybeEvent

{- | The worker notify its coordinator that it's ready
 and starts waiting for new events and process them as they come
-}
startWorker
  :: forall input m
   . (MonadIO m)
  => (Ord (Point input))
  => TChan (ProcessedInput (Point input) input)
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

      indexEvent timedEvent = do
        Con.modifyMVar ix $ \indexer -> do
          result <- runExceptT $ hoistError $ index timedEvent indexer
          case result of
            Left err -> pure (indexer, Just err)
            Right res -> pure (res, Nothing)

      indexAllEventsDescending timedEvents = do
        Con.modifyMVar ix $ \indexer -> do
          result <- runExceptT $ hoistError $ indexAllDescending timedEvents indexer
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

      handleStableAt :: Point input -> IO (Maybe IndexerError)
      handleStableAt p = do
        Con.modifyMVar ix $ \indexer -> do
          result <- runExceptT $ hoistError $ setLastStablePoint p indexer
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
        IndexAllDescending es -> indexAllEventsDescending es
        StableAt p -> handleStableAt p
        Stop -> pure $ Just $ OtherIndexError "Stop"

      -- TODO replace with foldM on either
      processAll [] = pure Nothing
      processAll (x : xs) = do
        merr <- process x
        case merr of
          Nothing -> processAll xs
          Just _err -> pure merr

      displayError :: (Show a) => a -> Text
      displayError e = name <> ": " <> Text.pack (show e)

      onProcessError (SomeException e) =
        pure . Just . StopIndexer . Just $ displayError e

      safeProcessEvent f input = do
        processed <- runExceptT $ runPreprocessor f (pure input)
        case processed of
          Left err -> (,f) <$> onProcessError (SomeException err)
          Right (processedInputs, f') ->
            (,f') <$> processAll processedInputs `catch` onProcessError

      loop :: TChan (ProcessedInput (Point input) input) -> IO ()
      loop chan' =
        let loop' f = do
              err <- checkError
              case err of
                Nothing -> do
                  event <- STM.atomically $ STM.readTChan chan'
                  (result, f') <- safeProcessEvent f event
                  case result of
                    Nothing -> do
                      unlockCoordinator
                      loop' f'
                    Just err' -> do
                      notifyCoordinatorOnError err'
                      unlockCoordinator
                Just _ -> unlockCoordinator
         in loop' transformInput
   in liftIO $ do
        chan' <- STM.atomically $ STM.dupTChan chan
        Con.forkFinally (loop chan') (const swallowPill)
