{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Experimental.Indexers (buildIndexers)
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Experimental.Runner qualified as Runner
import Marconi.Core.Experiment qualified as Core
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  putStrLn $ "marconi-chain-index-experimental " <> Cli.getVersion
  o <- Cli.parseOptions
  let batchSize = 5000
      stopCatchupDistance = 100
      filteredAddresses = []
      includeScript = True
      socketPath = Cli.optionsSocketPath $ Cli.commonOptions o
      networkId = Cli.optionsNetworkId $ Cli.commonOptions o
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  mindexers <-
    runExceptT $
      buildIndexers
        (Core.CatchupConfig batchSize stopCatchupDistance)
        (Utxo.UtxoIndexerConfig filteredAddresses includeScript)
        (Cli.optionsDbPath o)
  let (_utxoQueryIndexer, indexers) = case mindexers of
        Left err -> error $ show err
        Right result -> result
  Runner.runIndexer
    socketPath
    networkId
    (Cli.optionsChainPoint $ Cli.commonOptions o)
    "marconi-chain-index-experimental"
    indexers
