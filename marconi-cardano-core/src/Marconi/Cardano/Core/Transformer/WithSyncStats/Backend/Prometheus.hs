{-# LANGUAGE OverloadedStrings #-}

module Marconi.Cardano.Core.Transformer.WithSyncStats.Backend.Prometheus where

import Data.Time (NominalDiffTime)
import Marconi.Cardano.Core.Transformer.WithSyncStats (
  LastSyncStats (LastSyncStats),
  StatsBackend (StatsBackend),
  emptyLastSyncStats,
 )
import Prometheus qualified as P

{- | Creates a simple Prometheus backend which keeps track of the number of processed blocks and
		the number of rollbacks.

		Takes a @NominalDiffTime@ which determines how frequently we send stats to Prometheus.
-}
mkPrometheusBackend :: NominalDiffTime -> IO StatsBackend
mkPrometheusBackend timeBetween = do
  processedBlocksCounter <-
    P.register $
      P.gauge (P.Info "processed_blocks_counter" "Number of processed blocks")
  processedRollbacksCounter <-
    P.register $
      P.gauge (P.Info "processed_rollbacks_counter" "Number of processed rollbacks")
  pure $
    StatsBackend
      (updateMetrics processedBlocksCounter processedRollbacksCounter)
      timeBetween
      emptyLastSyncStats
  where
    updateMetrics :: P.Gauge -> P.Gauge -> LastSyncStats -> IO ()
    updateMetrics rollforwards rollbacks (LastSyncStats fw bw _ _ _) = do
      _ <- P.setGauge rollforwards (fromInteger . toInteger $ fw)
      _ <- P.setGauge rollbacks (fromInteger . toInteger $ bw)
      pure ()
