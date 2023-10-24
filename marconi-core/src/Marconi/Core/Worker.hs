{-# LANGUAGE AllowAmbiguousTypes #-}
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
  startWorker,
  createWorkerWithPreprocessing,
  createWorkerHoist,
) where

import Control.Concurrent (MVar, QSemN, ThreadId)
import Control.Concurrent qualified as Con
import Control.Concurrent.STM (TChan, TVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception (SomeException (SomeException), catch, finally)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
data WorkerM m input point = forall indexer event.
  ( WorkerIndexerType m event indexer
  , Point event ~ point
  , Closeable m indexer
  ) =>
  Worker
  { workerName :: Text
  -- ^ use to identify the worker in logs
  , workerState :: MVar (indexer event)
  -- ^ the indexer controlled by this worker
  , transformInput :: Preprocessor m point input event
  -- ^ adapt the input event givent by the coordinator to the worker type
  }

-- | A worker that operates in @IO@.
type Worker = WorkerM IO

-- Create workers

-- | create a worker for an indexer, retuning the worker and the @MVar@ it's using internally
createWorkerHoistM
  :: (MonadIO f, WorkerIndexerType m event indexer)
  => Text
  -> Preprocessor m (Point event) input event
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorkerHoistM name f ix = liftIO $ do
  workerState <- Con.newMVar ix
  pure $ WorkerIndexer workerState $ Worker name workerState f

createWorkerHoist
  :: (WorkerIndexerType m event indexer)
  => Text
  -> Preprocessor m (Point event) input event
  -> MVar (indexer event)
  -> WorkerIndexer m input event indexer
createWorkerHoist name f workerState = do
  WorkerIndexer workerState $ Worker name workerState f

-- | create a worker for an indexer that already throws IndexerError
createWorkerWithPreprocessing
  :: (MonadIO f, WorkerIndexerType m event indexer)
  => Text
  -> Preprocessor m (Point event) input event
  -> indexer event
  -> f (WorkerIndexer m input event indexer)
createWorkerWithPreprocessing = createWorkerHoistM

-- | create a worker for an indexer that already throws IndexerError
createWorker
  :: (IsIndex m event indexer, IsSync m event indexer, Closeable m indexer)
  => Text
  -> (input -> Maybe event)
  -> MVar (indexer event)
  -> WorkerIndexer m input event indexer
createWorker name = createWorkerHoist name . mapMaybeEvent

{- | The worker notify its coordinator that it's ready
 and starts waiting for new events and process them as they come
-}
startWorker
  :: forall input m
   . (MonadIO m)
  => (Ord (Point input))
  => TChan (ProcessedInput (Point input) input)
  -> TVar [IndexerError]
  -> QSemN
  -> QSemN
  -> Worker input (Point input)
  -> m ThreadId
startWorker chan errorBox endTokens tokens (Worker name ix transformInput) =
  let unlockCoordinator :: IO ()
      unlockCoordinator = Con.signalQSemN tokens 1

      notifyEndToCoordinator :: IO ()
      notifyEndToCoordinator = Con.signalQSemN endTokens 1

      indexEvent timedEvent =
        Con.modifyMVar ix $ \indexer ->
          ( liftIO $ do
              res <- index timedEvent indexer
              pure (res, Nothing)
          )
            `catch` (\e -> pure (indexer, Just e))

      indexAllEventsDescending timedEvents =
        Con.modifyMVar ix $ \indexer ->
          ( liftIO $ do
              res <- indexAllDescending timedEvents indexer
              pure (res, Nothing)
          )
            `catch` (\e -> pure (indexer, Just e))

      handleRollback :: Point input -> IO (Maybe IndexerError)
      handleRollback p = do
        Con.modifyMVar ix $ \indexer ->
          ( liftIO $ do
              res <- rollback p indexer
              pure (res, Nothing)
          )
            `catch` (\e -> pure (indexer, Just e))

      handleStableAt :: Point input -> IO (Maybe IndexerError)
      handleStableAt p = do
        Con.modifyMVar ix $ \indexer ->
          ( liftIO $ do
              res <- setLastStablePoint p indexer
              pure (res, Nothing)
          )
            `catch` (\e -> pure (indexer, Just e))

      checkError :: IO [IndexerError]
      checkError = STM.readTVarIO errorBox

      closeIndexer :: IO ()
      closeIndexer = do
        indexer <- Con.readMVar ix
        close indexer

      swallowPill :: IO ()
      swallowPill = finally closeIndexer notifyEndToCoordinator `catch` notifyCoordinatorOnError

      notifyCoordinatorOnError :: IndexerError -> IO ()
      notifyCoordinatorOnError e =
        -- We don't need to check if tryPutMVar succeed
        -- because if @errorBox@ is already full, our job is done anyway
        void $ appendToTVar errorBox [e]

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

      runSafeProcessing f input = do
        (processedInputs, f') <- runPreprocessor f (pure input)
        (,f') <$> processAll processedInputs `catch` onProcessError

      withCatchProcessing f input =
        runSafeProcessing f input
          `catch` \(err :: SomeException) -> (,f) <$> onProcessError (SomeException err)

      safeProcessEvent = withCatchProcessing

      loop :: TChan (ProcessedInput (Point input) input) -> IO ()
      loop chan' =
        let loop' f = do
              err <- checkError
              case err of
                [] -> do
                  event <- STM.atomically $ STM.readTChan chan'
                  (result, f') <- safeProcessEvent f event
                  case result of
                    Nothing -> do
                      unlockCoordinator
                      loop' f'
                    Just err' -> do
                      notifyCoordinatorOnError err'
                      unlockCoordinator
                _ -> unlockCoordinator
         in loop' transformInput
   in liftIO $ do
        chan' <- STM.atomically $ STM.dupTChan chan
        Con.forkFinally (loop chan') (const swallowPill)

appendToTVar :: (Monoid a) => TVar a -> a -> IO ()
appendToTVar tvar newVal = do
  STM.atomically $
    STM.modifyTVar tvar $
      \currentVal -> currentVal <> newVal
