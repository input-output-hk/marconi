{-# LANGUAGE NamedFieldPuns #-}

module Marconi.Sidechain.Experimental.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (defaultConfigStdout)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Marconi.ChainIndex.Logger (mkMarconiTrace)
import Marconi.Sidechain.Experimental.CLI (
  CliArgs (CliArgs, dbDir, networkId, optionsRetryConfig, socketFilePath),
  getVersion,
  parseCli,
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
    error "TODO"
