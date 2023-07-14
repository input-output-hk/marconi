{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    Coordinating the work of a list of workers.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Coordinator (
  Coordinator,
  lastSync,
  workers,
  threadIds,
  tokens,
  channel,
  nbWorkers,
  mkCoordinator,
  step,
) where

import Control.Concurrent (QSemN, ThreadId)
import Control.Concurrent qualified as Con
import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM qualified as STM
import Control.Lens (makeLenses)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, listToMaybe)

import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index),
  IsSync (lastSyncPoint),
  Rollbackable (rollback),
 )
import Marconi.Core.Experiment.Type (IndexerError, Point, point)
import Marconi.Core.Experiment.Worker (
  ProcessedInput (Index, Rollback),
  Worker,
  WorkerM (errorBox),
  startWorker,
 )

{- | A coordinator synchronises the event processing of a list of indexers.
 A coordinator is itself is an indexer.
 It means that we can create a tree of indexer, with coordinators that partially process the data at each node,
 and with concrete indexers at the leaves.
-}
data Coordinator input = Coordinator
  { _lastSync :: Point input
  -- ^ the last common sync point for the workers
  , _workers :: [Worker input (Point input)]
  -- ^ the list of workers managed by this coordinator
  , _threadIds :: [ThreadId]
  -- ^ the thread ids of the workers
  , _tokens :: QSemN
  -- ^ use to synchronise the worker
  , _channel :: TChan (ProcessedInput input)
  -- ^ to dispatch input to workers
  , _nbWorkers :: Int
  -- ^ how many workers are we waiting for, should always be equal to @length workers@
  }

-- TODO handwrite lenses to avoid invalid states
makeLenses 'Coordinator

-- | create a coordinator and starts its workers
mkCoordinator
  :: (HasGenesis (Point input))
  => (Ord (Point input))
  => [Worker input (Point input)]
  -> IO (Coordinator input)
mkCoordinator workers' =
  let startWorkers channel' tokens' = traverse (startWorker channel' tokens') workers'
   in do
        let nb = length workers'
        tokens' <- Con.newQSemN 0 -- starts empty, will be filled when the workers will start
        channel' <- STM.newBroadcastTChanIO
        threadIds' <- startWorkers channel' tokens'
        pure $ Coordinator genesis workers' threadIds' tokens' channel' nb

-- | A coordinator step (send an input to its workers, wait for an ack of every worker before listening again)
step
  :: ( Ord (Point input)
     , MonadIO m
     , MonadError IndexerError m
     )
  => Coordinator input
  -> ProcessedInput input
  -> m (Coordinator input)
step coordinator input = do
  case input of
    Index e -> index e coordinator
    Rollback p -> rollback p coordinator

waitWorkers :: Coordinator input -> IO ()
waitWorkers coordinator = Con.waitQSemN (coordinator ^. tokens) (coordinator ^. nbWorkers)

dispatchNewInput :: Coordinator input -> ProcessedInput input -> IO ()
dispatchNewInput coordinator = STM.atomically . STM.writeTChan (coordinator ^. channel)

healthCheck
  :: (MonadIO m)
  => Coordinator input
  -> m (Maybe IndexerError)
healthCheck c = do
  let ws = c ^. workers
  errors <- liftIO $ traverse Con.tryReadMVar $ errorBox <$> ws
  pure $ listToMaybe $ catMaybes errors

-- A coordinator can be consider as an indexer that forwards the input to its worker
instance (MonadIO m, MonadError IndexerError m) => IsIndex m event Coordinator where
  index timedEvent coordinator =
    let setLastSync e = coordinator & lastSync .~ (e ^. point)
     in do
          liftIO $ dispatchNewInput coordinator $ Index timedEvent
          liftIO $ waitWorkers coordinator
          errors <- healthCheck coordinator
          case errors of
            Just err -> close coordinator *> throwError err
            Nothing -> pure $ setLastSync timedEvent

instance (MonadIO m) => IsSync m event Coordinator where
  lastSyncPoint indexer = pure $ indexer ^. lastSync

-- | To rollback a coordinator, we try and rollback all the workers.
instance
  ( MonadIO m
  , MonadError IndexerError m
  )
  => Rollbackable m event Coordinator
  where
  rollback p =
    let setLastSync c = c & lastSync .~ p

        rollbackWorkers :: Coordinator event -> m (Coordinator event)
        rollbackWorkers c = do
          liftIO $ dispatchNewInput c $ Rollback p
          liftIO $ waitWorkers c
          errors <- healthCheck c
          case errors of
            Just err -> close c *> throwError err
            Nothing -> pure $ setLastSync c
     in rollbackWorkers

instance (MonadIO m) => Closeable m Coordinator where
  close coordinator = liftIO $ do
    traverse_ Con.killThread (coordinator ^. threadIds)

-- There is no point in providing a 'Queryable' interface for 'CoordinatorIndex' though,
-- as it's sole interest would be to get the latest synchronisation points,
-- but 'query' requires a 'Point' to provide a result.
