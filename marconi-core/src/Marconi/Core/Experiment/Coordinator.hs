{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    Coordinating the work of a list of workers.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Coordinator (
  Coordinator,
  workers,
  threadIds,
  tokens,
  channel,
  errorBox,
  nbWorkers,
  mkCoordinator,
  step,
  processQueue,
) where

import Control.Concurrent (MVar, QSemN, ThreadId)
import Control.Concurrent qualified as Con
import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM qualified as STM
import Control.Exception (throwIO)
import Control.Exception.Base (finally)
import Control.Lens (makeLenses)
import Control.Lens.Operators ((^.))
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (State)
import Data.Foldable (Foldable (toList))
import Data.Maybe (catMaybes)
import Marconi.Core.Experiment.Class (
  Closeable (close),
  IsIndex (index, indexAllDescending, rollback, setLastStablePoint),
  IsSync (lastStablePoint, lastSyncPoint),
 )
import Marconi.Core.Experiment.Preprocessor (Preprocessor, preprocessor, runPreprocessor)
import Marconi.Core.Experiment.Type (
  IndexerError (StopIndexer),
  Point,
  ProcessedInput (Index, IndexAllDescending, Rollback, StableAt, Stop),
  Timed,
 )
import Marconi.Core.Experiment.Worker (
  Worker,
  WorkerM (Worker),
  startWorker,
 )

{- | A coordinator synchronises the event processing of a list of indexers.
 A coordinator is itself is an indexer.
 It means that we can create a tree of indexer, with coordinators that partially process the data at each node,
 and with concrete indexers at the leaves.
-}
data Coordinator input = Coordinator
  { _workers :: [Worker input (Point input)]
  -- ^ the list of workers managed by this coordinator
  , _threadIds :: [ThreadId]
  -- ^ the thread ids of the workers
  , _tokens :: QSemN
  -- ^ used to synchronise the worker
  , _endTokens :: QSemN
  -- ^ used to check workers end
  , _channel :: TChan (ProcessedInput (Point input) input)
  -- ^ to dispatch input to workers
  , _errorBox :: MVar IndexerError
  -- ^ keep track of workers error
  , _nbWorkers :: Int
  -- ^ how many workers are we waiting for, should always be equal to @length workers@
  }

makeLenses 'Coordinator

-- | create a coordinator and starts its workers
mkCoordinator
  :: (Ord (Point input))
  => [Worker input (Point input)]
  -> IO (Coordinator input)
mkCoordinator workers' = do
  let startWorkers channel' errorBox' endTokens' tokens' =
        traverse (startWorker channel' errorBox' endTokens' tokens') workers'
  errorBox' <- Con.newEmptyMVar
  let nb = length workers'
  tokens' <- Con.newQSemN 0 -- starts empty, will be filled when the workers will start
  endTokens' <- Con.newQSemN 0 -- starts empty, will be filled when the workers will start
  channel' <- STM.newBroadcastTChanIO
  threadIds' <- startWorkers channel' errorBox' endTokens' tokens'
  pure $ Coordinator workers' threadIds' tokens' endTokens' channel' errorBox' nb

{- | Read a queue of events, processing them synchronously on each worker

 Note that this function silently throw an @IndexError@ if the event processing fails.
-}
processQueue
  :: forall indexer event s r
   . ( Ord (Point event)
     , IsIndex (ExceptT IndexerError IO) event indexer
     , Closeable IO indexer
     )
  => (Timed (Point event) (Maybe event) -> State s (Maybe (Point event)))
  -- ^ emit stable point based on incoming information
  -> s
  -> STM.TBQueue (ProcessedInput (Point event) event)
  -> Con.MVar (indexer event)
  -> IO r
processQueue f initialState q cBox =
  let attachStable :: Preprocessor IO (Point event) event event
      attachStable = flip preprocessor initialState $ \case
        Index timedEvent -> do
          stablePointM <- f timedEvent
          case stablePointM of
            Nothing -> pure [Index timedEvent]
            Just stablePoint -> pure [Index timedEvent, StableAt stablePoint]
        IndexAllDescending timedEvents -> do
          stablePoints <- catMaybes . toList <$> traverse f timedEvents
          case stablePoints of
            [] -> pure [IndexAllDescending timedEvents]
            xs -> pure [IndexAllDescending timedEvents, StableAt (maximum xs)]
        other -> pure [other]

      queueStep g = do
        e <- STM.atomically $ STM.readTBQueue q
        g' <- Con.modifyMVar cBox $ \c -> do
          (events, g') <- runPreprocessor g [e]
          mres <- runExceptT (foldM step c events)
          case mres of
            Left (err :: IndexerError) -> throwIO err
            Right res -> pure (res, g')
        queueStep g'
   in queueStep attachStable `finally` Con.withMVar cBox close

{- | A coordinator step
(send an input to its workers, wait for an ack of every worker before listening again)
-}
step
  :: ( Ord (Point input)
     , IsIndex m input indexer
     , MonadError IndexerError m
     )
  => indexer input
  -> ProcessedInput (Point input) input
  -> m (indexer input)
step indexer = \case
  Index e -> index e indexer
  IndexAllDescending es -> indexAllDescending es indexer
  Rollback p -> rollback p indexer
  StableAt p -> setLastStablePoint p indexer
  Stop -> throwError $ StopIndexer Nothing

waitWorkers :: Coordinator input -> IO ()
waitWorkers coordinator = Con.waitQSemN (coordinator ^. tokens) (coordinator ^. nbWorkers)

waitEnd :: Coordinator input -> IO ()
waitEnd coordinator = Con.waitQSemN (coordinator ^. endTokens) (coordinator ^. nbWorkers)

dispatchNewInput :: Coordinator input -> ProcessedInput (Point input) input -> IO ()
dispatchNewInput coordinator = STM.atomically . STM.writeTChan (coordinator ^. channel)

healthCheck
  :: (MonadIO m)
  => Coordinator input
  -> m (Maybe IndexerError)
healthCheck c = liftIO $ Con.tryReadMVar $ c ^. errorBox

safeDispatch
  :: (MonadIO m, MonadError IndexerError m)
  => ProcessedInput (Point event) event
  -> Coordinator event
  -> m (Coordinator event)
safeDispatch event coordinator = do
  liftIO $ do
    dispatchNewInput coordinator event
    waitWorkers coordinator
  errors <- healthCheck coordinator
  case errors of
    Just err -> throwError err
    Nothing -> pure coordinator

-- A coordinator can be consider as an indexer that forwards the input to its worker
instance (MonadIO m, MonadError IndexerError m) => IsIndex m event Coordinator where
  index = safeDispatch . Index
  rollback = safeDispatch . Rollback
  setLastStablePoint = safeDispatch . StableAt

instance
  (Ord (Point event), MonadIO m, MonadError IndexerError m)
  => IsSync m event Coordinator
  where
  lastSyncPoint indexer =
    let workerLastSyncPoint :: Worker event (Point event) -> m (Point event)
        workerLastSyncPoint (Worker _name state _f hoistError) = do
          ix <- liftIO $ Con.readMVar state
          res <- liftIO $ runExceptT $ hoistError $ lastSyncPoint ix
          case res of
            Left err -> throwError err
            Right res' -> pure res'
     in minimum <$> traverse workerLastSyncPoint (indexer ^. workers)

  lastStablePoint indexer =
    let workerLastSyncPoint :: Worker event (Point event) -> m (Point event)
        workerLastSyncPoint (Worker _name state _f hoistError) = do
          ix <- liftIO $ Con.readMVar state
          res <- liftIO $ runExceptT $ hoistError $ lastStablePoint ix
          case res of
            Left err -> throwError err
            Right res' -> pure res'
     in minimum <$> traverse workerLastSyncPoint (indexer ^. workers)

instance (MonadIO m) => Closeable m Coordinator where
  close coordinator = liftIO $ do
    dispatchNewInput coordinator Stop
    waitEnd coordinator

-- There is no point in providing a 'Queryable' interface for 'CoordinatorIndex' though,
-- as it's sole interest would be to get the latest synchronisation points,
-- but 'query' requires a 'Point' to provide a result.
