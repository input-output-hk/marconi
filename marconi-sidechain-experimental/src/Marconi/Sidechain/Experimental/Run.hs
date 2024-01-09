{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Experimental.Run where

import Cardano.BM.Setup qualified as BM
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing qualified as BM
import Control.Exception (finally)
import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Marconi.Cardano.Core.Logger (defaultStdOutLogger)
import Marconi.Core (IndexerError)
import Marconi.Sidechain.Experimental.Api.HttpServer (runHttpServer)
import Marconi.Sidechain.Experimental.CLI (
  CliArgs (debugMode),
  getVersion,
  parseCli,
 )
import Marconi.Sidechain.Experimental.Concurrency (
  HandledAction (Handled, Unhandled),
  raceSignalHandled_,
 )
import Marconi.Sidechain.Experimental.Env (
  mkSidechainEnvFromCliArgs,
  querySecurityParamFromCliArgs,
  sidechainHttpServerConfig,
  sidechainRunIndexersConfig,
 )
import Marconi.Sidechain.Experimental.Indexers (runIndexers)
import Text.Pretty.Simple (pShowDarkBg)

run :: IO ()
run =
  do
    cliArgs <- parseCli
    let logLevel = if debugMode cliArgs then BM.Debug else BM.Info
    (trace, sb) <- defaultStdOutLogger "marconi-sidechain-experimental" logLevel

    logInfo trace $ "marconi-sidechain-" <> Text.pack getVersion
    logInfo trace . Text.toStrict $ pShowDarkBg cliArgs

    -- Create the 'SidechainEnv' from the CLI arguments,
    -- with some validity checks on arguments needed to create the environment.
    env <- mkSidechainEnvFromCliArgs trace sb cliArgs =<< querySecurityParamFromCliArgs trace cliArgs

    raceSignalHandled_
      (Unhandled (runReaderT runHttpServer (env ^. sidechainHttpServerConfig)))
      (Handled @IndexerError (runReaderT runIndexers (env ^. sidechainRunIndexersConfig)))
      `finally` BM.shutdown sb
