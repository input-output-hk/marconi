{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Helper to create a worker for a Coordinator
module Marconi.ChainIndex.Experimental.Indexers.Coordinator (
  coordinatorWorker,
) where

import Control.Concurrent (MVar)
import Control.Monad.Cont (MonadIO (liftIO))

import Data.Text (Text)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.Orphans qualified ()
import Marconi.Core.Experiment qualified as Core

coordinatorWorker
  :: (MonadIO m, Ord (Core.Point b), Core.HasGenesis (Core.Point b))
  => Text
  -> (WithDistance a -> IO (Maybe b))
  -> [Core.Worker b (Core.Point b)]
  -> m (MVar (Core.Coordinator b), Core.Worker (WithDistance a) (Core.Point b))
coordinatorWorker name extract workers = liftIO $ do
  coordinator <- Core.mkCoordinator workers
  Core.createWorker name extract coordinator
