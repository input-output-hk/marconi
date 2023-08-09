{- |
 This module bootstraps the marconi-sidechain JSON RPC server, it acts as a glue conntecting the
 JSON-RPC, HttpServer, marconiIndexer, and marconi cache
-}
module Marconi.Sidechain.Bootstrap where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Marconi.ChainIndex.Indexers (epochStateWorker, mintBurnWorker, runIndexers, utxoWorker)
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle)
import Marconi.ChainIndex.Indexers.MintBurn (MintBurnHandle)
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.ChainIndex.Types (
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
import Marconi.Sidechain.CLI (
  CliArgs,
 )
import Marconi.Sidechain.CLI qualified as CLI
import Marconi.Sidechain.Env (
  SidechainEnv,
  epochStateIndexerEnvIndexer,
  mintBurnIndexerEnvIndexer,
  sidechainAddressUtxoIndexer,
  sidechainEpochStateIndexer,
  sidechainIndexersEnv,
  sidechainMintBurnIndexer,
 )
import System.FilePath ((</>))

-- | Run Sidechain indexers
runSidechainIndexers
  :: CliArgs
  -> SidechainEnv
  -> IO ()
runSidechainIndexers cliArgs env = do
  let addressUtxoCallback :: State UtxoHandle -> IO ()
      addressUtxoCallback =
        atomically
          . AddressUtxo.updateEnvState (env ^. sidechainIndexersEnv . sidechainAddressUtxoIndexer)
  let epochStateCallback :: State EpochStateHandle -> IO ()
      epochStateCallback =
        atomically
          . EpochState.updateEnvState
            (env ^. sidechainIndexersEnv . sidechainEpochStateIndexer . epochStateIndexerEnvIndexer)
  let mintBurnCallback :: State MintBurnHandle -> IO ()
      dbPath = CLI.dbDir cliArgs
      mintBurnCallback =
        atomically
          . MintBurn.updateEnvState
            (env ^. sidechainIndexersEnv . sidechainMintBurnIndexer . mintBurnIndexerEnvIndexer)
      utxoIndexerConfig =
        UtxoIndexerConfig
          { ucTargetAddresses = CLI.targetAddresses cliArgs
          , ucEnableUtxoTxOutRef = False --  we do not save scriptRef for sidechain
          }
      indexers =
        [
          ( utxoWorker addressUtxoCallback utxoIndexerConfig
          , Just $ dbPath </> utxoDbName
          )
        ,
          ( epochStateWorker (CLI.nodeConfigPath cliArgs) epochStateCallback
          , Just $ dbPath </> epochStateDbName
          )
        ,
          ( mintBurnWorker mintBurnCallback $ CLI.targetAssets cliArgs
          , Just $ dbPath </> mintBurnDbName
          )
        ]
  runIndexers
    (CLI.socketFilePath cliArgs)
    (CLI.networkId cliArgs)
    C.ChainPointAtGenesis
    (CLI.minIndexingDepth cliArgs)
    (CLI.optionsFailsIfResync cliArgs)
    "marconi-sidechain"
    indexers
