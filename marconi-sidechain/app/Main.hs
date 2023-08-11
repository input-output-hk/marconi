module Main where

import Control.Concurrent.Async (race_)
import Marconi.Sidechain.Api.HttpServer (runHttpServer)
import Marconi.Sidechain.Bootstrap (runSidechainIndexers)
import Marconi.Sidechain.CLI (parseCli)
import Marconi.Sidechain.Env (mkSidechainEnvFromCliArgs)

{- | Concurrently start:

 * JSON-RPC server
 * marconi indexer workers

 Exceptions in either thread will end the program
-}
main :: IO ()
main = do
  cliArgs <- parseCli
  rpcEnv <- mkSidechainEnvFromCliArgs cliArgs

  race_
    (runHttpServer rpcEnv) -- Start HTTP server
    (runSidechainIndexers cliArgs rpcEnv) -- Start the Sidechain indexers
