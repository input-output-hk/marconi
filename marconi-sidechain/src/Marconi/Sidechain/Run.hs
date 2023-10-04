{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Run where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (defaultConfigStdout)
import Control.Concurrent.Async (race_)
import Control.Exception (catch, throwIO)
import Control.Monad.Reader (runReaderT)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Data.Void (Void)
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Sidechain.Api.HttpServer (runHttpServer)
import Marconi.Sidechain.Bootstrap (runSidechainIndexers)
import Marconi.Sidechain.CLI (
  CliArgs (CliArgs, dbDir, networkId, optionsRetryConfig, socketFilePath),
  getVersion,
  parseCli,
 )
import Marconi.Sidechain.Env (mkSidechainEnvFromCliArgs)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Text.Pretty.Simple (pShowDarkBg)

-- See note 4e8b9e02-fae4-448b-8b32-1eee50dd95ab

import Control.Concurrent (MVar, putMVar, tryTakeMVar)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (
  newEmptyMVar,
  takeMVar,
  tryPutMVar,
 )
import Data.Foldable (traverse_)
import Data.Functor (void)
import Marconi.ChainIndex.Indexers (IndexerException (TimeoutException))
import System.Posix.Signals (
  Handler (CatchOnce),
  Signal,
  installHandler,
  sigINT,
  sigTERM,
 )

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

    cliArgs@CliArgs{dbDir, socketFilePath, networkId, optionsRetryConfig} <- parseCli

    logInfo trace . Text.toStrict $ pShowDarkBg cliArgs

    createDirectoryIfMissing True dbDir

    securityParam <- withNodeConnectRetry trace optionsRetryConfig socketFilePath $ do
      Utils.toException $ Utils.querySecurityParam @Void networkId socketFilePath

    rpcEnv <- mkSidechainEnvFromCliArgs securityParam cliArgs trace

    mvar <- newEmptyMVar

    let timeoutExceptionHandler (e :: IndexerException) = do
          putMVar mvar (syncExceptionToExit e)
          throwIO e
        action =
          race_
            (runReaderT runHttpServer rpcEnv) -- Start HTTP server
            ( runReaderT
                runSidechainIndexers
                rpcEnv
                `catch` timeoutExceptionHandler
            ) -- Start the Sidechain indexers
    res <- withExitHandling action
    case res of
      Right _ -> pure ()
      Left cint -> do
        var' <- tryTakeMVar mvar
        let exitCode = case var' of
              Just i -> (ExitFailure i)
              Nothing -> (ExitFailure (signalToExit $ fromIntegral cint))
        exitWith exitCode

-- See note 4e8b9e02-fae4-448b-8b32-1eee50dd95ab

{- | Ensure that @SIGTERM@ is handled gracefully, because it's how containers are stopped.

 @action@ will receive an 'AsyncCancelled' exception if @SIGTERM@ is received by the process.

 Typical use:

 > main :: IO ()
 > main = withGracefulTermination_ $ do

 Note that although the Haskell runtime handles @SIGINT@ it doesn't do anything with @SIGTERM@.
 Therefore, when running as PID 1 in a container, @SIGTERM@ will be ignored unless a handler is
 installed for it.
-}
withExitHandling :: IO () -> IO (Either Signal ())
withExitHandling action = do
  termVar <- newEmptyMVar
  let terminate terminationType = void $ tryPutMVar termVar terminationType
      waitForTermination = takeMVar termVar
      signals = [sigINT, sigTERM]

  traverse_ (\signal -> installHandler signal (CatchOnce (terminate signal)) Nothing) signals
  race
    waitForTermination
    action

signalToExit :: Int -> Int
signalToExit signal =
  case signal of
    2 -> 130
    15 -> 143
    _ -> 4

syncExceptionToExit :: IndexerException -> Int
syncExceptionToExit ex =
  case ex of
    TimeoutException -> 124
