{-# LANGUAGE TypeApplications #-}

module Marconi.Tutorial.Indexers where

import Cardano.BM.Trace (Trace)
import Data.Text (Text)
import Data.Void (Void)
import Marconi.ChainIndex.CLI qualified as CommonCLI
import Marconi.ChainIndex.Experimental.Runner qualified as Runner
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Experiment qualified as Core
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.Indexers.AddressCount qualified as AddressCount
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

runIndexers :: Trace IO Text -> CLI.Options -> IO ()
runIndexers trace o = do
  createDirectoryIfMissing True (CLI.optionsDbPath o)

  let socketPath = CommonCLI.optionsSocketPath $ CLI.commonOptions o
      networkId = CommonCLI.optionsNetworkId $ CLI.commonOptions o
      retryConfig = CommonCLI.optionsRetryConfig $ CLI.commonOptions o
      preferedStartingPoint = CommonCLI.optionsChainPoint $ CLI.commonOptions o
  securityParam <- withNodeConnectRetry trace retryConfig socketPath $ do
    Utils.toException $ Utils.querySecurityParam @Void networkId socketPath
  indexerWorkers <-
    sequence
      [ fmap Core.worker $
          AddressCount.addressCountWorker
            (CLI.optionsDbPath o </> "addresscount.db")
            securityParam
      ]

  indexers <- Core.mkCoordinator indexerWorkers

  Runner.runIndexer
    trace
    securityParam
    retryConfig
    socketPath
    networkId
    preferedStartingPoint
    indexers
