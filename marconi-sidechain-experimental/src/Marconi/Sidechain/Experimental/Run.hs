{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Experimental.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Monad.Reader (runReaderT)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Data.Void (Void)
import Marconi.ChainIndex.Logger (mkMarconiTrace)
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Sidechain.Experimental.CLI (
  CliArgs (CliArgs, dbDir, networkId, optionsRetryConfig, socketFilePath),
  getVersion,
  parseCli,
 )
import Marconi.Sidechain.Experimental.Concurrency (
  HandledAction (Handled, Unhandled),
  raceSignalHandled_,
 )
import System.Directory (createDirectoryIfMissing)
import Text.Pretty.Simple (pShowDarkBg)

run :: IO ()
run = do
  traceConfig <- defaultConfigStdout
  withTrace traceConfig "marconi-sidechain" $ \trace -> do
    let marconiTrace = mkMarconiTrace trace

    cliArgs@CliArgs{dbDir, socketFilePath, networkId, optionsRetryConfig} <- parseCli

    logInfo trace $ "marconi-sidechain-" <> Text.pack getVersion
    logInfo trace . Text.toStrict $ pShowDarkBg cliArgs

    createDirectoryIfMissing True dbDir

    securityParam <- withNodeConnectRetry marconiTrace optionsRetryConfig socketFilePath $ do
      Utils.toException $ Utils.querySecurityParam @Void networkId socketFilePath

    error "TODO"

-- TODO: PLT-8076
-- rpcEnv <- mkSidechainEnvFromCliArgs securityParam cliArgs marconiTrace

-- raceSignalHandled_
--  (Unhandled (runReaderT runHttpServer rpcEnv))
--  (Handled (runReaderT runSidechainIndexers rpcEnv))
