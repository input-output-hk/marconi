{-# LANGUAGE OverloadedStrings #-}

import Data.Void (Void)
import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Experimental.Indexers qualified as Indexers
import Marconi.ChainIndex.Utils qualified as Utils
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

main :: IO ()
main = do
  o <- Cli.parseOptions
  createDirectoryIfMissing True (Cli.optionsDbPath o)

  let socketPath = Cli.optionsSocketPath $ Cli.commonOptions o
      networkId = Cli.optionsNetworkId $ Cli.commonOptions o
  securityParam <- Utils.toException $ Utils.querySecurityParam @Void networkId socketPath
  indexers <-
    sequence
      [ fmap snd $ Indexers.utxoWorker (Cli.optionsDbPath o </> "utxo.db") securityParam
      ]

  Indexers.runIndexers
    socketPath
    networkId
    (Cli.optionsChainPoint $ Cli.commonOptions o)
    "marconi-chain-index-experimental"
    indexers
