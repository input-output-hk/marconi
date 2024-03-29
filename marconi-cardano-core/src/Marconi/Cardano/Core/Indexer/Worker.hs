{-# LANGUAGE FlexibleContexts #-}

-- | Common worker configuration and creation
module Marconi.Cardano.Core.Indexer.Worker (
  StandardIndexer,
  StandardSQLiteIndexer,
  StandardWorkerConfig (..),
  StandardWorker (..),
  mkStandardWorker,
  mkStandardWorkerWithFilter,
  mkStandardIndexer,
  mkStandardIndexerWithFilter,
) where

import Cardano.Api qualified as C
import Cardano.BM.Tracing (Trace)
import Control.Arrow ((<<<))
import Control.Concurrent (MVar)
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Data.Word (Word64)
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Extract.WithDistance qualified as Distance
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (SecurityParam)
import Marconi.Core qualified as Core

-- | An alias for an indexer with catchup and transformation to perform filtering
type StandardIndexer m indexer event =
  Core.WithTrace
    m
    (Core.WithCatchup (Core.WithTransform indexer event))
    (WithDistance (Maybe event))

-- | An alias for an SQLiteWorker with catchup and transformation to perform filtering
type StandardSQLiteIndexer m event = StandardIndexer m Core.SQLiteIndexer event

data StandardWorkerConfig m input event = StandardWorkerConfig
  { workerName :: Text
  , securityParamConfig :: !SecurityParam
  , catchupConfig :: Core.CatchupConfig
  , eventExtractor :: input -> m (Maybe event)
  , logger :: Trace m (Core.IndexerEvent C.ChainPoint)
  }

-- | Contains a worker for a given indexer and the `MVar` that is modified by this worker
data StandardWorker m input event indexer = StandardWorker
  { standardWorkerIndexerVar :: !(MVar (StandardIndexer m indexer event))
  , standardWorker :: Core.WorkerM m (WithDistance input) (Core.Point event)
  }

-- | Create a standard indexer from the given indexer with some standard catchup values.
mkStandardIndexer
  :: ( MonadIO m
     , Core.Point event ~ C.ChainPoint
     )
  => StandardWorkerConfig m a b
  -> indexer event
  -> StandardIndexer m indexer event
mkStandardIndexer config indexer =
  let chainPointDistance :: Core.Point (WithDistance a) -> WithDistance a -> Word64
      chainPointDistance _ = Distance.chainDistance
   in Core.withTrace (logger config) $
        Core.withCatchup chainPointDistance (catchupConfig config) $
          Core.withTransform id Distance.getEvent indexer

-- | Create a worker for the given indexer with some standard catchup values
mkStandardWorker
  :: ( MonadIO m
     , MonadIO n
     , Core.WorkerIndexerType (ExceptT Core.IndexerError m) event indexer
     , Core.IsSync n event indexer
     , Core.Point event ~ C.ChainPoint
     )
  => StandardWorkerConfig m input event
  -> indexer event
  -> n (StandardWorker m input event indexer)
mkStandardWorker config = mkStandardWorkerWithFilter config Just

-- | Create a standard indexer from the given indexer with some standard catchup values with extra filtering.
mkStandardIndexerWithFilter
  :: ( MonadIO m
     , Core.Point event ~ C.ChainPoint
     )
  => StandardWorkerConfig m a b
  -> (event -> Maybe event)
  -> indexer event
  -> StandardIndexer m indexer event
mkStandardIndexerWithFilter config eventFilter indexer =
  let chainPointDistance :: Core.Point (WithDistance a) -> WithDistance a -> Word64
      chainPointDistance _ = Distance.chainDistance
   in Core.withTrace (logger config) $
        Core.withCatchup chainPointDistance (catchupConfig config) $
          Core.withTransform id (eventFilter <=< Distance.getEvent) indexer

-- | Create a worker for the given indexer with some standard catchup values with extra filtering.
mkStandardWorkerWithFilter
  :: ( MonadIO n
     , MonadIO m
     , Core.WorkerIndexerType (ExceptT Core.IndexerError m) event indexer
     , Core.IsSync n event indexer
     , Ord (Core.Point event)
     , Core.Point event ~ C.ChainPoint
     )
  => StandardWorkerConfig m input event
  -> (event -> Maybe event)
  -> indexer event
  -> n (StandardWorker m input event indexer)
mkStandardWorkerWithFilter config eventFilter indexer = do
  let mapEventUnderDistance =
        Core.traverseMaybeEvent $ fmap Just . traverse (lift . eventExtractor config)
      transformedIndexer = mkStandardIndexerWithFilter config eventFilter indexer
  lastStable <- Core.lastStablePoint indexer
  let eventPreprocessing = Core.withResume lastStable <<< mapEventUnderDistance
  Core.WorkerIndexer idx worker <-
    Core.createWorkerWithPreprocessing (workerName config) eventPreprocessing transformedIndexer
  pure $ StandardWorker idx worker
