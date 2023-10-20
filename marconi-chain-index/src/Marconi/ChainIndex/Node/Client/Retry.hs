{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Node.Client.Retry where

import Cardano.BM.Trace (
  logError,
  logWarning,
 )
import Control.Concurrent (
  threadDelay,
 )
import Control.Exception (Handler (Handler), catches, throwIO)
import Data.Word (Word64)
import Marconi.ChainIndex.Types (MarconiTrace, RetryConfig, baseTimeBeforeNextRetry, maybeMaxWaitTime)
import Network.Mux.Trace (MuxError (MuxError), MuxErrorType (MuxBearerClosed, MuxIOException))
import Prettyprinter (
  pretty,
  (<+>),
 )
import System.Exit (exitFailure)
import System.IO.Error (isDoesNotExistError)

data RetryState = RetryState
  { totalWaitTime :: !Word64
  , secondsBeforeNextRetry :: !Word64
  }

withNodeConnectRetry :: forall a ann. MarconiTrace IO ann -> RetryConfig -> FilePath -> IO a -> IO a
withNodeConnectRetry stdoutTrace retryConfig socketPath action = do
  let initialRetryState = RetryState 0 (baseTimeBeforeNextRetry retryConfig)
  runActionWithRetries stdoutTrace initialRetryState
  where
    runActionWithRetries :: MarconiTrace IO ann -> RetryState -> IO a
    runActionWithRetries trace retryState = do
      catches
        action
        [ Handler $
            \case
              -- Exception thrown when the socket file path does not exist.
              -- This means that the cardano-node has not been started.
              e | isDoesNotExistError e -> do handleCantConnectToNodeException trace retryState
              e -> throwIO e
        , Handler $
            \case
              -- Exception thrown when the cardano-node was stopped while we are reading from the
              -- socket file.
              MuxError (MuxIOException _) _ -> handleCantConnectToNodeException trace retryState
              MuxError MuxBearerClosed _ -> handleCantConnectToNodeException trace retryState
              e -> throwIO e
        ]

    handleCantConnectToNodeException :: MarconiTrace IO ann -> RetryState -> IO a
    handleCantConnectToNodeException trace retryState = do
      if maybe True (totalWaitTime retryState <) $ maybeMaxWaitTime retryConfig
        then do
          logWarning trace $
            "Could not connect to Cardano node: socket file"
              <+> pretty socketPath
              <+> "does not exist. Retrying in"
              <+> pretty (secondsBeforeNextRetry retryState)
                <> "s ..."

          threadDelay $ fromIntegral $ secondsBeforeNextRetry retryState * 1_000_000
          runActionWithRetries
            trace
            ( retryState
                { totalWaitTime = totalWaitTime retryState + secondsBeforeNextRetry retryState
                , secondsBeforeNextRetry = secondsBeforeNextRetry retryState * 2
                }
            )
        else do
          logError trace $
            "Could not connect to Cardano node: socket file"
              <+> pretty socketPath
              <+> "does not exist."
          exitFailure
