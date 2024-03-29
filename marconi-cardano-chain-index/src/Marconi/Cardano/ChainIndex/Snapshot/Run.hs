{-# LANGUAGE OverloadedStrings #-}

module Marconi.Cardano.ChainIndex.Snapshot.Run (
  run,
) where

import Cardano.Api qualified as C
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing qualified as BM
import Control.Exception (throwIO)
import Control.Monad (unless, (>=>))
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Marconi.Cardano.ChainIndex.CLI (parseSnapshotOptions)
import Marconi.Cardano.ChainIndex.CLI qualified as Cli
import Marconi.Cardano.ChainIndex.Indexers (buildIndexersForSnapshot)
import Marconi.Cardano.ChainIndex.SecurityParam qualified as SecurityParam
import Marconi.Cardano.Core.Extract.WithDistance qualified as Distance
import Marconi.Cardano.Core.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.Cardano.Core.Node.Client.Retry (withNodeConnectRetry)
import Marconi.Cardano.Core.Runner (
  RunIndexerConfig (RunIndexerConfig),
  runIndexerOnChainSync,
  withDistanceAndTipPreprocessor,
 )
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  ExtLedgerStateWorkerConfig (ExtLedgerStateWorkerConfig),
 )
import Marconi.Core.Transformer.WithCatchup qualified as Core (
  mkCatchupConfig,
 )
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

appName :: Text
appName = "marconi-chain-snapshot"

{- | Runs the sets of indexers which serialize the user-provided ranges of blocks on disk.
 It effectively creates snapshots of sub-chains which can be streamed back to Marconi,
 avoiding the need to run a live Cardano node.
 Note that in order to not skip any ranges the indexers are always started from genesis.
-}
run :: IO ()
run = do
  (trace, sb) <- defaultStdOutLogger appName BM.Info
  options <- parseSnapshotOptions
  nodeConfigPath <- getNodeConfigPath options sb trace

  let marconiTrace = mkMarconiTrace trace
      retryConfig = Cli.snapshotOptionsRetryConfig options
      networkId = Cli.snapshotOptionsNetworkId options
      socketPath = Cli.snapshotOptionsSocketPath options
      snapshotDir = Cli.snapshotOptionsSnapshotDir options
      volatileEpochStateSnapshotInterval = 100
      batchSize = 5000
      stopCatchupDistance = 100

  securityParam <-
    withNodeConnectRetry marconiTrace retryConfig socketPath $
      (runExceptT >=> either throwIO pure) $
        SecurityParam.querySecurityParam @Void networkId socketPath

  let extLedgerStateConfig =
        ExtLedgerStateWorkerConfig
          Distance.getEvent
          Distance.chainDistance
          nodeConfigPath
          volatileEpochStateSnapshotInterval
          securityParam
          (const $ curry Just)
      snapshotConfig =
        RunIndexerConfig
          marconiTrace
          withDistanceAndTipPreprocessor
          retryConfig
          securityParam
          networkId
          C.ChainPointAtGenesis
          socketPath

  blockRanges <- getBlockRanges options sb trace

  mSnapshotCoordinator <-
    runExceptT $
      buildIndexersForSnapshot
        securityParam
        (Core.mkCatchupConfig batchSize stopCatchupDistance)
        extLedgerStateConfig
        trace
        marconiTrace
        snapshotDir
        blockRanges
        nodeConfigPath
  snapshotCoordinator <-
    case mSnapshotCoordinator of
      Left err -> withLogFullError exitFailure sb trace $ Text.pack $ show err
      Right result -> pure result

  runIndexerOnChainSync snapshotConfig snapshotCoordinator
  where
    withLogFullError action sb trace msg = do
      logError trace msg
      BM.shutdown sb
      action

    getNodeConfigPath options sb trace =
      case Cli.snapshotOptionsNodeConfigPath options of
        Just cfg -> do
          exists <- doesFileExist cfg
          unless exists $
            withLogFullError exitFailure sb trace $
              Text.pack $
                "Config file does not exist at the provided path: " <> cfg
          pure cfg
        Nothing -> withLogFullError exitFailure sb trace "No node config path provided"

    getBlockRanges options sb trace =
      case Cli.snapshotOptionsBlockRanges options of
        [] ->
          withLogFullError
            exitFailure
            sb
            trace
            "Please provide at least one block range to snapshot."
        br -> pure br
