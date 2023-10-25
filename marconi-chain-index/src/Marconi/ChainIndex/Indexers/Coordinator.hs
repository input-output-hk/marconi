{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Helper to create a worker for a Coordinator
module Marconi.ChainIndex.Indexers.Coordinator (
  coordinatorWorker,
  standardCoordinator,
) where

import Cardano.BM.Tracing (Trace)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Data.Text (Text)
import Marconi.ChainIndex.Indexers.Orphans qualified ()
import Marconi.Core qualified as Core

standardCoordinator
  :: (Ord (Core.Point event))
  => Trace IO (Core.IndexerEvent (Core.Point event))
  -> [Core.Worker event (Core.Point event)]
  -> IO (Core.WithTrace IO Core.Coordinator event)
standardCoordinator logger = Core.withTraceM logger . Core.mkCoordinator

coordinatorWorker
  :: (MonadIO m, Ord (Core.Point b))
  => Text
  -> Trace IO (Core.IndexerEvent (Core.Point b))
  -> (a -> IO (Maybe b))
  -> [Core.Worker b (Core.Point b)]
  -> m (Core.WorkerIndexer IO a b (Core.WithTrace IO Core.Coordinator))
coordinatorWorker name logger extract workers = liftIO $ do
  coordinator <- standardCoordinator logger workers
  Core.createWorkerWithPreprocessing name (Core.traverseMaybeEvent $ lift . extract) coordinator
