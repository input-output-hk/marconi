-- |
-- This module bootstraps the marconi-sidechain JSON RPC server, it acts as a glue conntecting the
-- JSON-RPC, HttpServer, marconiIndexer, and marconi cache
--
module Marconi.Sidechain.Bootstrap where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Marconi.ChainIndex.Indexers (epochStateWorker, mintBurnWorker, runIndexers, utxoWorker)
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle)
import Marconi.ChainIndex.Indexers.MintBurn (MintBurnHandle)
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.ChainIndex.Types (TargetAddresses, epochStateDbName, mintBurnDbName, utxoDbName)
import Marconi.Core.Storable (State, StorableEvent)
import Marconi.Sidechain.Api.Query.Indexers.EpochState qualified as EpochState
import Marconi.Sidechain.Api.Query.Indexers.MintBurn qualified as MintBurn
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as AddressUtxo
import Marconi.Sidechain.Api.Types (CliArgs (CliArgs), SidechainEnv (SidechainEnv),
                                    SidechainIndexers (SidechainIndexers), epochStateIndexerEnvIndexer,
                                    mintBurnIndexerEnvIndexer, sidechainAddressUtxoIndexer, sidechainEnvIndexers,
                                    sidechainEpochStateIndexer, sidechainMintBurnIndexer)
import Network.Wai.Handler.Warp (Port, defaultSettings, setPort)
import System.FilePath ((</>))

-- | Bootstraps the JSON-RPC  http server with appropriate settings and marconi cache
-- this is just a wrapper for the bootstrapHttp in json-rpc package
initializeSidechainEnv
    :: Maybe Port
    -> Maybe TargetAddresses
    -> IO SidechainEnv
initializeSidechainEnv maybePort targetAddresses = do
    let httpsettings = maybe defaultSettings (flip setPort defaultSettings ) maybePort
    sidechainIndexers <-
        SidechainIndexers
            <$> AddressUtxo.initializeEnv targetAddresses
            <*> EpochState.initializeEnv
            <*> MintBurn.initializeEnv
    pure $ SidechainEnv httpsettings sidechainIndexers

-- |  Marconi cardano blockchain indexer
bootstrapIndexers
    :: CliArgs
    -> SidechainEnv
    -> IO ()
bootstrapIndexers (CliArgs socketPath nodeConfigPath dbPath _ networkId minIndexingDepth targetAddresses) env = do
  let addressUtxoCallback :: State UtxoHandle -> IO ()
      addressUtxoCallback =
          atomically
        . AddressUtxo.updateEnvState (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
  let epochStateCallback :: (State EpochStateHandle, StorableEvent EpochStateHandle) -> IO ()
      epochStateCallback =
          atomically
        . EpochState.updateEnvState (env ^. sidechainEnvIndexers . sidechainEpochStateIndexer . epochStateIndexerEnvIndexer)
        . fst
  let mintBurnCallback :: State MintBurnHandle -> IO ()
      mintBurnCallback =
          atomically
        . MintBurn.updateEnvState
            (env ^. sidechainEnvIndexers . sidechainMintBurnIndexer . mintBurnIndexerEnvIndexer)
  let indexers =
          [ ( utxoWorker addressUtxoCallback targetAddresses
            , Just $ dbPath </> utxoDbName
            )
          , ( epochStateWorker nodeConfigPath epochStateCallback
            , Just $ dbPath </> epochStateDbName
            )
          , ( mintBurnWorker mintBurnCallback
            , Just $ dbPath </> mintBurnDbName
            )
          ]
  runIndexers
    socketPath
    networkId
    C.ChainPointAtGenesis
    minIndexingDepth
    "marconi-sidechain"
    indexers
