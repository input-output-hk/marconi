module Marconi.Tutorial.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Concurrent.Async (race_)
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.HttpServer qualified as HttpServer
import Marconi.Tutorial.Indexers qualified as Indexers

runApp :: IO ()
runApp = do
  o <- CLI.parseOptions

  traceConfig <- defaultConfigStdout
  withTrace traceConfig "marconi-tutorial" $ \trace -> do
    race_
      (Indexers.runIndexers trace o)
      (HttpServer.runHttpServer $ CLI.optionsHttpPort o)
