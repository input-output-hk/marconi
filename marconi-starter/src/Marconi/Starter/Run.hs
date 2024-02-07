{-# LANGUAGE TypeApplications #-}

module Marconi.Starter.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Concurrent.Async (race_)
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Void (Void)
import Marconi.Cardano.ChainIndex.CLI qualified as CommonCLI
import Marconi.Cardano.ChainIndex.SecurityParam qualified as SecurityParam
import Marconi.Cardano.Core.Logger (MarconiTrace, mkMarconiTrace)
import Marconi.Cardano.Core.Node.Client.Retry (withNodeConnectRetry)
import Marconi.Cardano.Core.Types (SecurityParam)
import Marconi.Starter.CLI qualified as CLI
import Marconi.Starter.Env (Env (Env))
import Marconi.Starter.HttpServer qualified as HttpServer
import Marconi.Starter.Indexers (buildIndexersEnv)
import Marconi.Starter.Indexers qualified as Indexers
import System.Directory (createDirectoryIfMissing)

runApp :: IO ()
runApp = do
  cliOptions <- CLI.parseOptions

  traceConfig <- defaultConfigStdout

  withTrace traceConfig "marconi-starter" $ \stdoutTrace -> do
    let marconiTrace = mkMarconiTrace stdoutTrace
    securityParam <- querySecuritParamWithRetry marconiTrace cliOptions
    liftIO $ createDirectoryIfMissing True (CLI.optionsDbPath cliOptions)
    indexersEnv <- buildIndexersEnv securityParam cliOptions
    let env = Env indexersEnv cliOptions marconiTrace securityParam
    race_
      (runReaderT Indexers.runIndexers env)
      (runReaderT HttpServer.runHttpServer env)

querySecuritParamWithRetry
  :: MarconiTrace IO
  -> CLI.Options
  -> IO SecurityParam
querySecuritParamWithRetry stdoutTrace cliOptions = do
  let retryConfig = CommonCLI.optionsRetryConfig $ CLI.commonOptions cliOptions
      networkId = CommonCLI.optionsNetworkId $ CLI.commonOptions cliOptions
      socketPath = CommonCLI.optionsSocketPath $ CLI.commonOptions cliOptions
  liftIO $
    withNodeConnectRetry stdoutTrace retryConfig socketPath $
      runExceptT (SecurityParam.querySecurityParam @Void networkId socketPath)
        >>= either throwIO pure
