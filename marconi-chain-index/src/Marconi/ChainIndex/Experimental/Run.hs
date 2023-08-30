{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Run where

import Cardano.BM.Trace (logInfo)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Experimental.Indexers (buildIndexers)
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Experimental.Logger (defaultStdOutLogger)
import Marconi.ChainIndex.Experimental.Runner qualified as Runner
import Marconi.Core.Experiment qualified as Core
import System.Directory (createDirectoryIfMissing)

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
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  trace <- defaultStdOutLogger appName
  mindexers <-
    runExceptT $
      buildIndexers
        (Core.CatchupConfig batchSize stopCatchupDistance)
        (Utxo.UtxoIndexerConfig filteredAddresses includeScript)
        (MintTokenEvent.MintTokenEventConfig filteredAssetIds)
        trace
        (Cli.optionsDbPath o)
  let (_utxoQueryIndexer, indexers) = case mindexers of
        Left err -> error $ show err
        Right result -> result
  let retryConfig = Cli.optionsRetryConfig $ Cli.commonOptions o
  logInfo trace $ appName <> "-" <> Text.pack Cli.getVersion
  Runner.runIndexer
    trace
    retryConfig
    socketPath
    networkId
    (Cli.optionsChainPoint $ Cli.commonOptions o)
    indexers
