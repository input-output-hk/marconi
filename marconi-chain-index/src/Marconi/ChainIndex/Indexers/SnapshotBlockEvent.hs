{-# LANGUAGE FlexibleContexts #-}

module Marconi.ChainIndex.Indexers.SnapshotBlockEvent (
  snapshotBlockEventWorker,
  SnapshotBlockEvent (..),
  SnapshotBlockEventWorkerConfig (..),
  SnapshotBlockEventMetadata,
) where

import Cardano.Api qualified as C
import Control.Arrow ((<<<))
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorkerConfig (eventExtractor, logger, workerName),
 )
import Marconi.Cardano.Core.Types (BlockEvent, BlockRange)
import Marconi.Core qualified as Core

data SnapshotBlockEventWorkerConfig input = SnapshotBlockEventWorkerConfig
  { currentBlockNo :: input -> C.BlockNo
  , blockRange :: BlockRange
  }

data SnapshotBlockEventMetadata = TODO2

newtype SnapshotBlockEvent = SnapshotBlockEvent BlockEvent

type instance Core.Point SnapshotBlockEvent = C.ChainPoint

mkSnapshotBlockEventIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.FileIndexer SnapshotBlockEventMetadata SnapshotBlockEvent)
mkSnapshotBlockEventIndexer = undefined

snapshotBlockEventWorker
  :: forall input m n
   . (MonadIO m, MonadError Core.IndexerError m, MonadIO n)
  => StandardWorkerConfig n input SnapshotBlockEvent
  -> SnapshotBlockEventWorkerConfig input
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          n
          input
          SnapshotBlockEvent
          (Core.WithTrace n (Core.FileIndexer SnapshotBlockEventMetadata))
      )
snapshotBlockEventWorker standardWorkerConfig snapshotBlockEventWorkerConfig path = do
  indexer <- Core.withTrace (logger standardWorkerConfig) <$> mkSnapshotBlockEventIndexer path
  let preprocessor =
        Core.traverseMaybeEvent (lift . eventExtractor standardWorkerConfig)
          <<< inBlockRangePreprocessor
            (currentBlockNo snapshotBlockEventWorkerConfig)
            (blockRange snapshotBlockEventWorkerConfig)
  Core.createWorkerWithPreprocessing (workerName standardWorkerConfig) preprocessor indexer

inBlockRangePreprocessor
  :: (Monad m) => (a -> C.BlockNo) -> BlockRange -> Core.Preprocessor m C.ChainPoint a a
inBlockRangePreprocessor getBlockNo blockRange = undefined
