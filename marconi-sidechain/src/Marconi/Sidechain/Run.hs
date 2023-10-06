{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Monad.Reader (runReaderT)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Data.Void (Void)
import Marconi.ChainIndex.Error (IndexerError)
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Sidechain.Api.HttpServer (runHttpServer)
import Marconi.Sidechain.Bootstrap (runSidechainIndexers)
import Marconi.Sidechain.CLI (
  CliArgs (CliArgs, dbDir, networkId, optionsRetryConfig, socketFilePath),
  getVersion,
  parseCli,
 )
import Marconi.Sidechain.Concurrency (HandledAction (Handled, Unhandled), raceSignalHandled_)
import Marconi.Sidechain.Env (mkSidechainEnvFromCliArgs)
import System.Directory (createDirectoryIfMissing)
import Text.Pretty.Simple (pShowDarkBg)

{- | Concurrently start:

* JSON-RPC server
* marconi indexer workers

Exceptions in either thread will end the program

If the program is terminated with SIGINT or SIGTERM, exceptions in the marconi indexer workers
thread will be mapped to exit codes by 'Marconi.Sidechain.Error.toExit'
-}
run :: IO ()
run = do
  traceConfig <- defaultConfigStdout
  withTrace traceConfig "marconi-sidechain" $ \trace -> do
    logInfo trace $ "marconi-sidechain-" <> Text.pack getVersion

    cliArgs@CliArgs{dbDir, socketFilePath, networkId, optionsRetryConfig} <- parseCli

    logInfo trace . Text.toStrict $ pShowDarkBg cliArgs

    createDirectoryIfMissing True dbDir

    securityParam <- withNodeConnectRetry trace optionsRetryConfig socketFilePath $ do
      Utils.toException $ Utils.querySecurityParam @Void networkId socketFilePath

    rpcEnv <- mkSidechainEnvFromCliArgs securityParam cliArgs trace

    {- In an ideal world, we'd map both threads' errors to exit codes, but we'd need some machinery
    to determine which one takes priority, or something similar. Currently, we only care about
    timeouts in the sidechain, so this is fine for now. -}
    raceSignalHandled_
      (Unhandled (runReaderT runHttpServer rpcEnv))
      (Handled @(IndexerError Void) (runReaderT runSidechainIndexers rpcEnv))
