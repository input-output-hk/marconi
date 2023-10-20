{-# LANGUAGE TypeApplications #-}

module Marconi.Tutorial.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Concurrent.Async (race_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Void (Void)
import Marconi.ChainIndex.CLI qualified as CommonCLI
import Marconi.ChainIndex.Logging (mkMarconiTrace)
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Types (MarconiTrace, SecurityParam)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.Env (Env (Env))
import Marconi.Tutorial.HttpServer qualified as HttpServer
import Marconi.Tutorial.Indexers (buildIndexersEnv)
import Marconi.Tutorial.Indexers qualified as Indexers
import System.Directory (createDirectoryIfMissing)

runApp :: IO ()
runApp = do
  cliOptions <- CLI.parseOptions

  traceConfig <- defaultConfigStdout

  withTrace traceConfig "marconi-tutorial" $ \stdoutTrace -> do
    let marconiTrace = mkMarconiTrace stdoutTrace
    securityParam <- querySecuritParamWithRetry marconiTrace cliOptions
    liftIO $ createDirectoryIfMissing True (CLI.optionsDbPath cliOptions)
    indexersEnv <- buildIndexersEnv securityParam cliOptions
    let env = Env indexersEnv cliOptions marconiTrace securityParam
    race_
      (runReaderT Indexers.runIndexers env)
      (runReaderT HttpServer.runHttpServer env)

querySecuritParamWithRetry
  :: MarconiTrace IO ann
  -> CLI.Options
  -> IO SecurityParam
querySecuritParamWithRetry stdoutTrace cliOptions = do
  let retryConfig = CommonCLI.optionsRetryConfig $ CLI.commonOptions cliOptions
      networkId = CommonCLI.optionsNetworkId $ CLI.commonOptions cliOptions
      socketPath = CommonCLI.optionsSocketPath $ CLI.commonOptions cliOptions
  liftIO $ withNodeConnectRetry stdoutTrace retryConfig socketPath $ do
    Utils.toException $ Utils.querySecurityParam @Void networkId socketPath
