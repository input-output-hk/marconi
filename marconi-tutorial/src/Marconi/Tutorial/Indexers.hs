{-# LANGUAGE TypeApplications #-}

module Marconi.Tutorial.Indexers where

import Data.Void (Void)
import Marconi.ChainIndex.CLI qualified as CommonCLI
import Marconi.ChainIndex.Experimental.Indexers qualified as Indexers
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.Indexers.AddressCount qualified as AddressCount
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

runIndexers :: CLI.Options -> IO ()
runIndexers o = do
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

  Indexers.runIndexers
    (CommonCLI.optionsSocketPath $ CLI.commonOptions o)
    (CommonCLI.optionsNetworkId $ CLI.commonOptions o)
    (CommonCLI.optionsChainPoint $ CLI.commonOptions o)
    "marconi-tutorial"
    indexers
