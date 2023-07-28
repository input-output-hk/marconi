{-# LANGUAGE FlexibleContexts #-}

--

-- | Common worker configuration and creation
module Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardIndexer,
  catchupWorker,
  catchupWorkerWithFilter,
) where

import Cardano.Api qualified as C
import Control.Concurrent (MVar)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (ExceptT)
import Data.Word (Word64)
import Marconi.Core.Experiment qualified as Core

import Data.Text (Text)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Extract.WithDistance qualified as Distance

-- | An alias for an SQLiteWorker with catchup and transformation from a given type
type StandardIndexer event =
  Core.WithCatchup (Core.WithTransform Core.SQLiteIndexer event) (WithDistance event)

-- | Create a worker for the given indexer with some standard catchup values
catchupWorker
  :: ( MonadIO m
     , MonadIO n
     , Core.WorkerIndexer (ExceptT Core.IndexerError m) event indexer
     , Core.Point event ~ C.ChainPoint
     )
  => Text
  -> Core.CatchupConfig
  -> (input -> m (Maybe event))
  -> indexer event
  -> n
      ( MVar (Core.WithCatchup (Core.WithTransform indexer event) (WithDistance event))
      , Core.WorkerM m (WithDistance input) (Core.Point event)
      )
catchupWorker name = catchupWorkerWithFilter name Just

-- | Create a worker for the given indexer with some standard catchup values with extra filtering
catchupWorkerWithFilter
  :: ( MonadIO n
     , MonadIO m
     , Core.WorkerIndexer (ExceptT Core.IndexerError m) event indexer
     , Core.Point event ~ C.ChainPoint
     )
  => Text
  -> (event -> Maybe event)
  -> Core.CatchupConfig
  -> (input -> m (Maybe event))
  -> indexer event
  -> n
      ( MVar (Core.WithCatchup (Core.WithTransform indexer event) (WithDistance event))
      , Core.WorkerM m (WithDistance input) (Core.Point event)
      )
catchupWorkerWithFilter workerName eventFilter catchupConfig extract indexer =
  let chainPointDistance :: Core.Point (WithDistance a) -> WithDistance a -> Word64
      chainPointDistance _ = Distance.chainDistance
      mapEventUnderDistance = fmap sequence . traverse extract
   in Core.createWorker workerName mapEventUnderDistance $
        Core.withCatchup chainPointDistance catchupConfig $
          Core.withTransform id (eventFilter . Distance.getEvent) indexer
