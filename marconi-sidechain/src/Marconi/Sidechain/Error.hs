{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Marconi.Sidechain.Error (
  QueryExceptions (IndexerInternalError, QueryError, UnexpectedQueryResult, UntrackedPolicy),
  HasExitCode (toExitCode),
  withSignalHandling,
  signalToExit,
) where

import Cardano.Api qualified as C
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (Exception)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Void (Void)
import Marconi.ChainIndex.Error (IndexerError (Timeout))
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.Core.Storable (StorableQuery)
import System.Posix (Handler (CatchOnce), Signal, installHandler, sigINT, sigTERM)

data QueryExceptions
  = QueryError !Text
  | UntrackedPolicy C.PolicyId (Maybe C.AssetName)
  | UnexpectedQueryResult !(StorableQuery UtxoHandle)
  | IndexerInternalError !Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- * Mapping of exceptions to exit codes

class HasExitCode e where
  toExitCode :: e -> Int

instance HasExitCode (IndexerError Void) where
  toExitCode = indexerExceptionToExit

indexerExceptionToExit :: IndexerError Void -> Int
indexerExceptionToExit ex =
  case ex of
    Timeout _ -> 124
    _ -> 1

-- * Signal handling

{- | Takes an action and runs it while waiting for either a `SIGINT` or a `SIGTERM` to be thrown.
	If one is thrown, it's swallowed, but the value is returned to be interpreted by the caller of
	this function.
-}
withSignalHandling :: IO a -> IO (Either Signal a)
withSignalHandling action = do
  termVar <- newEmptyMVar
  let terminate terminationType = void $ tryPutMVar termVar terminationType
      waitForTermination = takeMVar termVar
      signals = [sigINT, sigTERM]

  traverse_ (\signal -> installHandler signal (CatchOnce (terminate signal)) Nothing) signals
  race
    waitForTermination
    action

{- | Maps a signal to an exit code.

		Maps SIGINT (2) to 130
		Maps SIGTERM (15) to 143
		Maps all other signals to 137 (SIGKILL)
-}
signalToExit :: Int -> Int
signalToExit signal =
  case signal of
    2 -> 130
    15 -> 143
    _ -> 137
