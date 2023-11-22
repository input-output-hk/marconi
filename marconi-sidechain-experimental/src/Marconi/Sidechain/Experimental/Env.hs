{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Experimental.Env where

import Cardano.Api qualified as C
import Cardano.BM.Backend.Switchboard qualified as BM
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Trace (Trace, logError)
import Control.Lens (makeLenses)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (toJSON)
import Data.List.NonEmpty qualified as NEList
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Marconi.ChainIndex.Api.Types qualified as ChainIndex.Types
import Marconi.ChainIndex.Indexers.EpochState (
  EpochStateWorkerConfig (EpochStateWorkerConfig),
  NodeConfig (NodeConfig),
 )
import Marconi.ChainIndex.Indexers.MintTokenEvent (MintTokenEventConfig (MintTokenEventConfig))
import Marconi.ChainIndex.Indexers.Utxo (UtxoIndexerConfig (UtxoIndexerConfig))
import Marconi.ChainIndex.Logger (mkMarconiTrace)
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Runner qualified as ChainIndex.Runner
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.ChainIndex.Utils qualified as ChainIndex.Utils
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.Types (
  SidechainHttpServerConfig (SidechainHttpServerConfig),
 )
import Marconi.Sidechain.Experimental.CLI (
  CliArgs (
    CliArgs,
    dbDir,
    httpPort,
    networkId,
    nodeConfigPath,
    optionsChainPoint,
    optionsRetryConfig,
    socketFilePath,
    targetAddresses,
    targetAssets
  ),
  startFromChainPoint,
 )
import Marconi.Sidechain.Experimental.Indexers (
  SidechainBuildIndexersConfig (SidechainBuildIndexersConfig),
  SidechainRunIndexersConfig (SidechainRunIndexersConfig),
  sidechainBuildIndexers,
  updateRunIndexerConfigWithLastStable,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)

{- TYPE -}

{- | Sidechain application config, including the configurations for running the indexers and for
 - running the http server. Construct with 'mkSidechainEnvFromCliArgs' to ensure shared components
 - have the same values, such as the security parameter.
-}
data SidechainEnv = SidechainEnv
  { _sidechainHttpServerConfig :: !SidechainHttpServerConfig
  , _sidechainRunIndexersConfig :: !SidechainRunIndexersConfig
  }

makeLenses ''SidechainEnv

{- CONSTRUCTORS -}

querySecurityParamFromCliArgs :: (MonadIO m) => Trace IO Text -> CliArgs -> m SecurityParam
querySecurityParamFromCliArgs trace CliArgs{..} =
  liftIO $
    withNodeConnectRetry (mkMarconiTrace trace) optionsRetryConfig socketFilePath $
      ChainIndex.Utils.toException $
        ChainIndex.Utils.querySecurityParam @Void networkId socketFilePath

{- | Create the 'SidechainEnv' from the CLI arguments,
with some validity checks on arguments needed to create the environment.

The SecurityParam is an argument as it must be queried from a running node.
Separating the parameter query from the rest of the logic allows for easier testing.
-}
mkSidechainEnvFromCliArgs
  :: Trace IO Text
  -> BM.Switchboard Text
  -- ^ Switchboard from iohk-monitoring, for sending shutdown.
  -> CliArgs
  -> SecurityParam
  -> IO SidechainEnv
mkSidechainEnvFromCliArgs trace sb cliArgs@CliArgs{..} securityParam = do
  -- Local utility copied from Marconi.ChainIndex.Run.run
  -- See note there for motivation.
  let exitWithLogFullError :: Text -> IO a
      exitWithLogFullError msg = do
        logError trace msg
        BM.shutdown sb
        exitFailure

  let marconiTrace = mkMarconiTrace trace

  -- Check whether the nodeConfigPath exists and if not fail.
  nodePathExists <- doesFileExist nodeConfigPath
  unless nodePathExists $
    exitWithLogFullError $
      Text.pack $
        "Config file does not exist at the provided path: "
          <> nodeConfigPath

  -- Create the db directory if needed
  createDirectoryIfMissing True dbDir

  -- Indexer config
  let
    filteredAddresses =
      maybe
        []
        (map C.AddressShelley . NEList.toList . NESet.toList)
        targetAddresses
    -- Hard-coded with the same values as chain-index
    catchupConfig = Core.mkCatchupConfig 5000 100
    -- Snapshot config hard-coded as in chain-index
    epochStateConfig = EpochStateWorkerConfig (NodeConfig nodeConfigPath) 100
    mintBurnConfig = MintTokenEventConfig targetAssets
    -- Explicitly does not include the script.
    utxoConfig = UtxoIndexerConfig filteredAddresses False

    runIndexerConfig lastSyncPoint =
      ChainIndex.Runner.RunIndexerConfig
        marconiTrace
        ChainIndex.Runner.withDistanceAndTipPreprocessor
        optionsRetryConfig
        securityParam
        networkId
        -- Note this will be updated relative to the latest sync point,
        -- in runSidechainIndexers.
        (startFromChainPoint optionsChainPoint lastSyncPoint)
        socketFilePath

    -- Used in sidechainBuildIndexers but not passed to the SidechainEnv
    buildIndexersConfig =
      SidechainBuildIndexersConfig
        trace
        securityParam
        catchupConfig
        dbDir
        epochStateConfig
        mintBurnConfig
        utxoConfig

  -- Build the indexers, returning workers and latest sync
  -- This is needed to build the http config.
  (indexerLastStablePoint, queryables, coordinator) <-
    either (exitWithLogFullError . Text.pack . show) pure =<< sidechainBuildIndexers buildIndexersConfig

  let
    runIndexersConfig =
      updateRunIndexerConfigWithLastStable indexerLastStablePoint $
        SidechainRunIndexersConfig (runIndexerConfig indexerLastStablePoint) coordinator

  -- Http config
  let
    httpConfig =
      ChainIndex.Types.HttpServerConfig
        trace
        httpPort
        securityParam
        filteredAddresses
        (toJSON cliArgs)
        queryables

    sidechainHttpConfig = SidechainHttpServerConfig httpConfig targetAssets

  pure (SidechainEnv sidechainHttpConfig runIndexersConfig)
