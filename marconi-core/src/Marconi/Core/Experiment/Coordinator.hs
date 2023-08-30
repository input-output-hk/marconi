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
import Control.Lens (makeLenses)
import Control.Lens.Operators ((^.))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Control.Exception (throwIO)
import Control.Exception.Base (finally)
import Control.Monad (forever)
import Marconi.Core.Experiment.Class (
  Closeable (close),
  IsIndex (index, rollback),
  IsSync (lastSyncPoint),
 )
import Marconi.Core.Experiment.Type (IndexerError (StopIndexer), Point)
import Marconi.Core.Experiment.Worker (
  ProcessedInput (Index, Rollback, Stop),
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
  , _channel :: TChan (ProcessedInput input)
  -- ^ to dispatch input to workers
  , _errorBox :: MVar IndexerError
  -- ^ keep track of workers error
  , _nbWorkers :: Int
  -- ^ how many workers are we waiting for, should always be equal to @length workers@
  }

-- TODO handwrite lenses to avoid invalid states
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
  :: ( Ord (Point event)
     , IsIndex (ExceptT IndexerError IO) event indexer
     , Closeable IO indexer
     )
  => STM.TBQueue (ProcessedInput event)
  -> Con.MVar (indexer event)
  -> IO r
processQueue q cBox =
  let queueStep = do
        e <- STM.atomically $ STM.readTBQueue q
        Con.modifyMVar_ cBox $ \c -> do
          mres <- runExceptT (step c e)
          case mres of
            Left (err :: IndexerError) -> throwIO err
            Right res -> pure res
   in forever queueStep `finally` Con.withMVar cBox close

{- | A coordinator step
(send an input to its workers, wait for an ack of every worker before listening again)
-}
step
  :: ( Ord (Point input)
     , IsIndex m input indexer
     , MonadError IndexerError m
     )
  => indexer input
  -> ProcessedInput input
  -> m (indexer input)
step indexer = \case
  Index e -> index e indexer
  Rollback p -> rollback p indexer
  Stop -> throwError $ StopIndexer Nothing

waitWorkers :: Coordinator input -> IO ()
waitWorkers coordinator = Con.waitQSemN (coordinator ^. tokens) (coordinator ^. nbWorkers)

waitEnd :: Coordinator input -> IO ()
waitEnd coordinator = Con.waitQSemN (coordinator ^. endTokens) (coordinator ^. nbWorkers)

dispatchNewInput :: Coordinator input -> ProcessedInput input -> IO ()
dispatchNewInput coordinator = STM.atomically . STM.writeTChan (coordinator ^. channel)

healthCheck
  :: (MonadIO m)
  => Coordinator input
  -> m (Maybe IndexerError)
healthCheck c = liftIO $ Con.tryReadMVar $ c ^. errorBox

safeDispatch
  :: (MonadIO m, MonadError IndexerError m)
  => ProcessedInput event
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

instance (MonadIO m) => Closeable m Coordinator where
  close coordinator = liftIO $ do
    dispatchNewInput coordinator Stop
    waitEnd coordinator

-- There is no point in providing a 'Queryable' interface for 'CoordinatorIndex' though,
-- as it's sole interest would be to get the latest synchronisation points,
-- but 'query' requires a 'Point' to provide a result.
