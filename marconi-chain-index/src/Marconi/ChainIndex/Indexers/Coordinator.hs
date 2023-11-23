{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Helper to create a worker for a Coordinator
module Marconi.ChainIndex.Indexers.Coordinator (
  coordinatorWorker,
  standardCoordinator,
  syncStatsCoordinator,
) where

import Cardano.BM.Tracing (Trace)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Data.Text (Text)
import Marconi.Cardano.Core.Orphans qualified ()
import Marconi.Cardano.Core.Transformer.WithSyncLog (
  WithSyncStats,
  mkPrintBackend,
  mkPrometheusBackend,
  withSyncStats,
 )
import Marconi.Cardano.Core.Types (MarconiTrace)
import Marconi.Core qualified as Core

standardCoordinator
  :: (Ord (Core.Point event))
  => Trace IO (Core.IndexerEvent (Core.Point event))
  -> [Core.Worker event (Core.Point event)]
  -> IO (Core.WithTrace IO Core.Coordinator event)
standardCoordinator logger = Core.withTraceM logger . Core.mkCoordinator

syncStatsCoordinator
  :: (Ord (Core.Point event))
  => Trace IO (Core.IndexerEvent (Core.Point event))
  -> MarconiTrace IO
  -> [Core.Worker event (Core.Point event)]
  -> IO (WithSyncStats (Core.WithTrace IO Core.Coordinator) event)
syncStatsCoordinator logger prettyLogger workers = do
  prometheus <- mkPrometheusBackend
  fmap (withSyncStats [mkPrintBackend prettyLogger, prometheus])
    . Core.withTraceM logger
    . Core.mkCoordinator
    $ workers

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
