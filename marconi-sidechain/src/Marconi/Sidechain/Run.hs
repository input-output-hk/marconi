{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Concurrent.Async (race_)
import Data.Text qualified as Text
import Data.Void (Void)
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Sidechain.Api.HttpServer (runHttpServer)
import Marconi.Sidechain.Bootstrap (runSidechainIndexers)
import Marconi.Sidechain.CLI (
  CliArgs (CliArgs, networkId, optionsRetryConfig, socketFilePath),
  getVersion,
  parseCli,
 )
import Marconi.Sidechain.Env (mkSidechainEnvFromCliArgs)

{- | Concurrently start:

 * JSON-RPC server
 * marconi indexer workers

 Exceptions in either thread will end the program
-}
run :: IO ()
run = do
  traceConfig <- defaultConfigStdout
  withTrace traceConfig "marconi-sidechain" $ \trace -> do
    logInfo trace $ "marconi-sidechain-" <> Text.pack getVersion

    cliArgs@CliArgs{socketFilePath, networkId, optionsRetryConfig} <- parseCli

    securityParam <- withNodeConnectRetry trace optionsRetryConfig socketFilePath $ do
      Utils.toException $ Utils.querySecurityParam @Void networkId socketFilePath

    rpcEnv <- mkSidechainEnvFromCliArgs securityParam cliArgs
    race_
      (runHttpServer rpcEnv) -- Start HTTP server
      (runSidechainIndexers trace cliArgs rpcEnv) -- Start the Sidechain indexers
