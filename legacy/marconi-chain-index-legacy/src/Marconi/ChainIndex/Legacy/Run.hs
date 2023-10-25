{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Legacy.Run (run) where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (defaultConfigStdout)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Data.Void (Void)
import Marconi.ChainIndex.Legacy.CLI qualified as CLI
import Marconi.ChainIndex.Legacy.CLI qualified as Cli
import Marconi.ChainIndex.Legacy.Indexers qualified as Indexers
import Marconi.ChainIndex.Legacy.Logging (mkMarconiTrace)
import Marconi.ChainIndex.Legacy.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Legacy.Types (
  RunIndexerConfig (RunIndexerConfig),
  UtxoIndexerConfig (UtxoIndexerConfig),
 )
import Marconi.ChainIndex.Legacy.Utils qualified as Utils
import System.Directory (createDirectoryIfMissing)
import Text.Pretty.Simple (pShowDarkBg)

{- | the worker don't have a hook to notify the query part
(we use a monoid because some hooks return unit while other returns a list of events)
-}
noHook :: (Monoid m) => a -> IO m
noHook = const $ pure mempty

run :: IO ()
run = do
  traceConfig <- defaultConfigStdout
  withTrace traceConfig "marconi-chain-index-legacy" $ \trace -> do
    let marconiTrace = mkMarconiTrace trace
    logInfo trace $ Text.pack $ "marconi-chain-index-" <> CLI.getVersion

    o <- Cli.parseOptions

    logInfo trace . Text.toStrict $ pShowDarkBg o

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

    let retryConfig = Cli.optionsRetryConfig $ Cli.commonOptions o

    let socketPath = Cli.optionsSocketPath $ Cli.commonOptions o
        networkId = Cli.optionsNetworkId $ Cli.commonOptions o

    securityParam <- withNodeConnectRetry marconiTrace retryConfig socketPath $ do
      Utils.toException $ Utils.querySecurityParam @Void networkId socketPath

    Indexers.runIndexers
      ( RunIndexerConfig
          marconiTrace
          retryConfig
          securityParam
          networkId
          (Cli.optionsChainPoint $ Cli.commonOptions o)
          socketPath
      )
      (Cli.optionsMinIndexingDepth $ Cli.commonOptions o)
      (Cli.optionsFailsIfResync o)
      indexers
