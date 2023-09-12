{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Run where

import Cardano.Api qualified as C
import Cardano.BM.Trace (logError, logInfo)
import Control.Monad.Except (runExceptT)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Experimental.Indexers (buildIndexers)
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Experimental.Logger (defaultStdOutLogger)
import Marconi.ChainIndex.Experimental.Runner qualified as Runner
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Experiment qualified as Core
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)

run :: Text -> IO ()
run appName = do
  o <- Cli.parseOptions
  let batchSize = 5000
      stopCatchupDistance = 100
      filteredAddresses = []
      filteredAssetIds = []
      includeScript = True
      socketPath = Cli.optionsSocketPath $ Cli.commonOptions o
      networkId = Cli.optionsNetworkId $ Cli.commonOptions o
      retryConfig = Cli.optionsRetryConfig $ Cli.commonOptions o
      preferredStartingPoint = Cli.optionsChainPoint $ Cli.commonOptions o
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  trace <- defaultStdOutLogger appName

  securityParam <- withNodeConnectRetry trace retryConfig socketPath $ do
    Utils.toException $ Utils.querySecurityParam @Void networkId socketPath

  mindexers <-
    runExceptT $
      buildIndexers
        securityParam
        (Core.CatchupConfig batchSize stopCatchupDistance)
        (Utxo.UtxoIndexerConfig filteredAddresses includeScript)
        (MintTokenEvent.MintTokenEventConfig filteredAssetIds)
        trace
        (Cli.optionsDbPath o)
  (indexerLastSyncPoints, _utxoQueryIndexer, indexers) <-
    ( case mindexers of
        Left err -> do
          logError trace $ Text.pack $ show err
          exitFailure
        Right result -> pure result
      )

  let startingPoints = getStartingPoints preferredStartingPoint indexerLastSyncPoints

  logInfo trace $ appName <> "-" <> Text.pack Cli.getVersion

  Runner.runIndexer
    trace
    securityParam
    retryConfig
    socketPath
    networkId
    startingPoints
    indexers

getStartingPoints :: C.ChainPoint -> [C.ChainPoint] -> NonEmpty C.ChainPoint
getStartingPoints preferredStartingPoint indexerLastSyncPoints =
  case preferredStartingPoint of
    C.ChainPointAtGenesis ->
      fromMaybe (NonEmpty.singleton C.ChainPointAtGenesis) $
        NonEmpty.nonEmpty indexerLastSyncPoints
    nonGenesisPreferedChainPoint -> NonEmpty.singleton nonGenesisPreferedChainPoint
