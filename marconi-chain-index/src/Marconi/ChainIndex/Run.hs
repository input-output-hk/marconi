{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Run (run) where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (defaultConfigStdout)
import Data.Text qualified as Text
import Marconi.ChainIndex.CLI qualified as CLI
import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Indexers qualified as Indexers
import Marconi.ChainIndex.Types (UtxoIndexerConfig (UtxoIndexerConfig))
import System.Directory (createDirectoryIfMissing)

{- | the worker don't have a hook to notify the query part
 (we use a monoid because some hooks return unit while other returns a list of events)
-}
noHook :: (Monoid m) => a -> IO m
noHook = const $ pure mempty

run :: IO ()
run = do
  o <- Cli.parseOptions
  createDirectoryIfMissing True (Cli.optionsDbPath o)
  let utxoIndexerConfig@(UtxoIndexerConfig maybeTargetAddresses _) =
        Cli.mkUtxoIndexerConfig o
      maybeTargetAssets = Cli.optionsTargetAssets o
      indexers =
        [ (Indexers.utxoWorker noHook utxoIndexerConfig, Cli.utxoDbPath o)
        , (Indexers.addressDatumWorker noHook maybeTargetAddresses, Cli.addressDatumDbPath o)
        , (Indexers.scriptTxWorker noHook, Cli.scriptTxDbPath o)
        , (Indexers.mintBurnWorker noHook maybeTargetAssets, Cli.mintBurnDbPath o)
        ]
          <> case Cli.optionsNodeConfigPath o of
            Just configPath ->
              [(Indexers.epochStateWorker configPath noHook, Cli.epochStateDbPath o)]
            Nothing -> []

  traceConfig <- defaultConfigStdout
  let retryConfig = Cli.optionsRetryConfig $ Cli.commonOptions o
  withTrace traceConfig "marconi-chain-index" $ \trace -> do
    logInfo trace $ Text.pack $ "marconi-chain-index-" <> CLI.getVersion
    Indexers.runIndexers
      trace
      retryConfig
      (Cli.optionsSocketPath $ Cli.commonOptions o)
      (Cli.optionsNetworkId $ Cli.commonOptions o)
      (Cli.optionsChainPoint $ Cli.commonOptions o)
      (Cli.optionsMinIndexingDepth $ Cli.commonOptions o)
      (Cli.optionsFailsIfResync o)
      indexers
