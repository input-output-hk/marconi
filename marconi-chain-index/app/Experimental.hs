{-# LANGUAGE OverloadedStrings #-}

import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Experimental.Indexers qualified as Indexers
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  o <- Cli.parseOptions
  createDirectoryIfMissing True (Cli.optionsDbPath o)

  Indexers.runIndexers
    (Cli.optionsSocketPath o)
    (Cli.optionsNetworkId o)
    (Cli.optionsChainPoint o)
    "marconi-chain-index-experimental"
    (Cli.optionsDbPath o)
