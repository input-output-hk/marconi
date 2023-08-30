{-# LANGUAGE FlexibleContexts #-}

--

-- | Common worker configuration and creation
module Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardIndexer,
  StandardSQLiteIndexer,
  StandardWorkerConfig (..),
  catchupWorker,
  catchupWorkerWithFilter,
) where

import Cardano.Api qualified as C
import Cardano.BM.Tracing (Trace)
import Control.Concurrent (MVar)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (ExceptT)
import Data.Text (Text)
import Data.Word (Word64)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Extract.WithDistance qualified as Distance
import Marconi.Core.Experiment qualified as Core

-- | An alias for an indexer with catchup and transformation to perform filtering
type StandardIndexer m indexer event =
  Core.WithTrace m (Core.WithCatchup (Core.WithTransform indexer event)) (WithDistance event)

-- | An alias for an SQLiteWorker with catchup and transformation to perform filtering
type StandardSQLiteIndexer m event = StandardIndexer m Core.SQLiteIndexer event

data StandardWorkerConfig m input event = StandardWorkerConfig
  { workerName :: Text
  , catchupConfig :: Core.CatchupConfig
  , eventExtractor :: input -> m (Maybe event)
  , logger :: Trace m (Core.IndexerEvent C.ChainPoint)
  }

-- | Create a worker for the given indexer with some standard catchup values
catchupWorker
  :: ( MonadIO m
     , MonadIO n
     , Core.WorkerIndexer (ExceptT Core.IndexerError m) event indexer
     , Core.Point event ~ C.ChainPoint
     )
  => StandardWorkerConfig m input event
  -> indexer event
  -> n (MVar (StandardIndexer m indexer event), Core.WorkerM m (WithDistance input) (Core.Point event))
catchupWorker config = catchupWorkerWithFilter config Just

-- | Create a worker for the given indexer with some standard catchup values with extra filtering
catchupWorkerWithFilter
  :: ( MonadIO n
     , MonadIO m
     , Core.WorkerIndexer (ExceptT Core.IndexerError m) event indexer
     , Core.Point event ~ C.ChainPoint
     )
  => StandardWorkerConfig m input event
  -> (event -> Maybe event)
  -> indexer event
  -> n (MVar (StandardIndexer m indexer event), Core.WorkerM m (WithDistance input) (Core.Point event))
catchupWorkerWithFilter config eventFilter indexer =
  let chainPointDistance :: Core.Point (WithDistance a) -> WithDistance a -> Word64
      chainPointDistance _ = Distance.chainDistance
      mapEventUnderDistance = fmap sequence . traverse (eventExtractor config)
   in Core.createWorker (workerName config) mapEventUnderDistance $
        Core.withTrace (logger config) $
          Core.withCatchup chainPointDistance (catchupConfig config) $
            Core.withTransform id (eventFilter . Distance.getEvent) indexer
