module Main where

import Control.Concurrent.Async (race_)
<<<<<<< HEAD
import Marconi.Sidechain.Api.HttpServer (runHttpServer)
import Marconi.Sidechain.Bootstrap (runSidechainIndexers)
import Marconi.Sidechain.CLI (parseCli)
import Marconi.Sidechain.Env (mkSidechainEnvFromCliArgs)
=======
import Marconi.Sidechain.Api.HttpServer qualified as Http
import Marconi.Sidechain.Api.Types (CliArgs (CliArgs, httpPort, targetAddresses, targetAssets))
import Marconi.Sidechain.Bootstrap (bootstrapIndexers, initializeSidechainEnv)
import Marconi.Sidechain.CLI (getVersion, parseCli)
>>>>>>> 32ba4a9b4 (Version works locally)

{- | Concurrently start:

 * JSON-RPC server
 * marconi indexer workers

 Exceptions in either thread will end the program
-}
main :: IO ()
main = do
  putStrLn $ "marconi-sidechain " <> getVersion
  cliArgs <- parseCli
  rpcEnv <- mkSidechainEnvFromCliArgs cliArgs

  race_
    (runHttpServer rpcEnv) -- Start HTTP server
    (runSidechainIndexers cliArgs rpcEnv) -- Start the Sidechain indexers
