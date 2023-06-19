{-# LANGUAGE OverloadedStrings #-}

module Main where

import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Indexers qualified as Indexers
import Marconi.ChainIndex.Types (UtxoIndexerConfig (UtxoIndexerConfig))
import System.Directory (createDirectoryIfMissing)

{- | the worker don't have a hook to notify the query part
 (we use a monoid because some hooks return unit while other returns a list of events)
-}
noHook :: Monoid m => a -> IO m
noHook = const $ pure mempty

main :: IO ()
main = do
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

  Indexers.runIndexers
    (Cli.optionsSocketPath o)
    (Cli.optionsNetworkId o)
    (Cli.optionsChainPoint o)
    (Cli.optionsMinIndexingDepth o)
    "marconi-chain-index"
    indexers
