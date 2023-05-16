module Marconi.Tutorial.Run where

import Control.Concurrent.Async (race_)
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.HttpServer qualified as HttpServer
import Marconi.Tutorial.Indexers qualified as Indexers

runApp :: IO ()
runApp = do
  o <- CLI.parseOptions
  race_
    (Indexers.runIndexers o)
    (HttpServer.runHttpServer $ CLI.optionsHttpPort o)
