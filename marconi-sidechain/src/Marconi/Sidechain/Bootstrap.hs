{- |
 This module bootstraps the marconi-sidechain JSON RPC server, it acts as a glue conntecting the
 JSON-RPC, HttpServer, marconiIndexer, and marconi cache
-}
module Marconi.Sidechain.Bootstrap where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty)
import Marconi.ChainIndex.Indexers (epochStateWorker, mintBurnWorker, runIndexers, utxoWorker)
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle)
import Marconi.ChainIndex.Indexers.MintBurn (MintBurnHandle)
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.ChainIndex.Types (
  TargetAddresses,
  UtxoIndexerConfig (UtxoIndexerConfig),
  epochStateDbName,
  mintBurnDbName,
  ucEnableUtxoTxOutRef,
  ucTargetAddresses,
  utxoDbName,
 )
import Marconi.Core.Storable (State)
import Marconi.Sidechain.Api.Query.Indexers.EpochState qualified as EpochState
import Marconi.Sidechain.Api.Query.Indexers.MintBurn qualified as MintBurn
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as AddressUtxo
import Marconi.Sidechain.Api.Types (
  CliArgs,
  SidechainEnv (SidechainEnv),
  SidechainIndexers (SidechainIndexers),
 )
import Marconi.Sidechain.Api.Types qualified as CLI
import Network.Wai.Handler.Warp (Port, defaultSettings, setPort)
import System.FilePath ((</>))

{- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
 this is just a wrapper for the bootstrapHttp in json-rpc package
-}
initializeSidechainEnv
  :: Maybe Port
  -> Maybe TargetAddresses
  -> Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -> IO SidechainEnv
initializeSidechainEnv maybePort targetAddresses targetAssets = do
  let httpsettings = maybe defaultSettings (flip setPort defaultSettings) maybePort
  sidechainIndexers <-
    SidechainIndexers
      <$> AddressUtxo.initializeEnv targetAddresses
      <*> EpochState.initializeEnv
      <*> MintBurn.initializeEnv targetAssets
  pure $ SidechainEnv httpsettings sidechainIndexers

-- |  Marconi cardano blockchain indexer
bootstrapIndexers
  :: CliArgs
  -> SidechainEnv
  -> IO ()
bootstrapIndexers args env = do
  let addressUtxoCallback :: State UtxoHandle -> IO ()
      addressUtxoCallback =
        atomically
          . AddressUtxo.updateEnvState (env ^. CLI.sidechainEnvIndexers . CLI.sidechainAddressUtxoIndexer)
  let epochStateCallback :: State EpochStateHandle -> IO ()
      epochStateCallback =
        atomically
          . EpochState.updateEnvState
            (env ^. CLI.sidechainEnvIndexers . CLI.sidechainEpochStateIndexer . CLI.epochStateIndexerEnvIndexer)
  let mintBurnCallback :: State MintBurnHandle -> IO ()
      dbPath = CLI.dbDir args
      mintBurnCallback =
        atomically
          . MintBurn.updateEnvState
            (env ^. CLI.sidechainEnvIndexers . CLI.sidechainMintBurnIndexer . CLI.mintBurnIndexerEnvIndexer)
      utxoIndexerConfig =
        UtxoIndexerConfig
          { ucTargetAddresses = CLI.targetAddresses args
          , ucEnableUtxoTxOutRef = False --  we do not save scriptRef for sidechain
          }
      indexers =
        [
          ( utxoWorker addressUtxoCallback utxoIndexerConfig
          , Just $ dbPath </> utxoDbName
          )
        ,
          ( epochStateWorker (CLI.nodeConfigPath args) epochStateCallback
          , Just $ dbPath </> epochStateDbName
          )
        ,
          ( mintBurnWorker mintBurnCallback $ CLI.targetAssets args
          , Just $ dbPath </> mintBurnDbName
          )
        ]
  runIndexers
    (CLI.socket args)
    (CLI.networkId args)
    C.ChainPointAtGenesis
    (CLI.minIndexingDepth args)
    "marconi-sidechain"
    indexers
