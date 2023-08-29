{-# LANGUAGE TypeApplications #-}

module Marconi.Tutorial.Indexers where

import Cardano.BM.Trace (Trace)
import Data.Text (Text)
import Data.Void (Void)
import Marconi.ChainIndex.CLI qualified as CommonCLI
import Marconi.ChainIndex.Experimental.Runner qualified as Runner
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.Indexers.AddressCount qualified as AddressCount
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

runIndexers :: Trace IO Text -> CLI.Options -> IO ()
runIndexers trace o = do
  createDirectoryIfMissing True (CLI.optionsDbPath o)

  let socketPath = CommonCLI.optionsSocketPath $ CLI.commonOptions o
      networkId = CommonCLI.optionsNetworkId $ CLI.commonOptions o
  securityParam <- Utils.toException $ Utils.querySecurityParam @Void networkId socketPath
  indexers <-
    sequence
      [ fmap snd $
          AddressCount.addressCountWorker
            (CLI.optionsDbPath o </> "addresscount.db")
            securityParam
      ]

  Runner.runIndexers
    trace
    (CommonCLI.optionsRetryConfig $ CLI.commonOptions o)
    (CommonCLI.optionsSocketPath $ CLI.commonOptions o)
    (CommonCLI.optionsNetworkId $ CLI.commonOptions o)
    (CommonCLI.optionsChainPoint $ CLI.commonOptions o)
    indexers
