{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Snapshot.Run (
  run,
) where

import Cardano.Api qualified as C
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Trace (logError)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Marconi.Cardano.Core.Extract.WithDistance qualified as Distance
import Marconi.Cardano.Core.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.Cardano.Core.Node.Client.Retry (withNodeConnectRetry)
import Marconi.Cardano.Core.Runner (
  RunIndexerConfig (RunIndexerConfig),
  runIndexer,
  withDistanceAndTipPreprocessor,
 )
import Marconi.ChainIndex.Indexers (buildIndexersForSnapshot)
import Marconi.ChainIndex.Indexers.ExtLedgerStateCoordinator (
  ExtLedgerStateWorkerConfig (ExtLedgerStateWorkerConfig),
 )
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Transformer.WithCatchup qualified as Core (
  mkCatchupConfig,
 )
import System.Exit (exitFailure)

appName :: Text
appName = "marconi-chain-snapshot"

run :: IO ()
run = do
  (trace, sb) <- defaultStdOutLogger appName
  -- TODO:
  --   - parse --testnet-magic, --node-config-path, --socket-path from CL
  --   - parse [(BlockRange, FilePath)] from CL in some nice way
  --   - implicit parsing: RetryConfig
  let marconiTrace = mkMarconiTrace trace
      retryConfig = undefined
      networkId = undefined -- I believe this is the testnet-magic, see commonNetworkIdParser
      socketPath = undefined
      nodeConfigPath = undefined
      volatileEpochStateSnapshotInterval = 100 -- What is this?
      topDirectoryForResult = undefined -- I don't know about this, it looks like mkExtLedgerStateCoordinator create some FileIndexer which creates files in a certain way; if it's currently creating SQLite files, how does it know, in my case, to create .cbor files?
      batchSize = 5000
      stopCatchupDistance = 100
  securityParam <-
    withNodeConnectRetry marconiTrace retryConfig socketPath $
      Utils.toException $
        Utils.querySecurityParam @Void networkId socketPath

  -- Question: do I need a `buildIndexers` function?
  -- Answer: I think yes, because we need to wrap it into a BlockEvent coordinator (not sure yet what it does) and a SyncStats coordinator, which adds node sync logging to the underlying coordinator
  let extLedgerStateConfig =
        ExtLedgerStateWorkerConfig
          Distance.getEvent
          trace
          nodeConfigPath
          volatileEpochStateSnapshotInterval
          securityParam
      snapshotConfig =
        RunIndexerConfig
          marconiTrace
          withDistanceAndTipPreprocessor -- or withNoPreprocessor -- is this right?
          retryConfig
          securityParam
          networkId
          C.ChainPointAtGenesis -- we should always start from genesis
          socketPath
  mSnapshotCoordinator <-
    runExceptT $
      buildIndexersForSnapshot
        securityParam
        (Core.mkCatchupConfig batchSize stopCatchupDistance) -- hwat is this?
        extLedgerStateConfig
        trace
        marconiTrace
        topDirectoryForResult
  snapshotCoordinator <-
    case mSnapshotCoordinator of
      Left err -> withLogFullError exitFailure sb trace $ Text.pack $ show err
      Right result -> pure result
  runIndexer snapshotConfig snapshotCoordinator
  where
    withLogFullError action sb trace msg = do
      logError trace msg
      BM.shutdown sb
      action
