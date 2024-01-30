{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Env where

import Cardano.Api qualified as C
import Cardano.BM.Backend.Switchboard qualified as BM
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Trace (Trace, logError)
import Control.Exception (throwIO)
import Control.Lens (makeLenses, (^.))
import Control.Monad (guard, unless, (>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (toJSON)
import Data.List.NonEmpty qualified as NEList
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Marconi.Cardano.ChainIndex.Api.Types qualified as ChainIndex.Types
import Marconi.Cardano.ChainIndex.Indexers qualified as Indexers
import Marconi.Cardano.ChainIndex.SecurityParam qualified as SecurityParam
import Marconi.Cardano.Core.Extract.WithDistance (chainDistance, getEvent)
import Marconi.Cardano.Core.Logger (mkMarconiTrace)
import Marconi.Cardano.Core.Node.Client.Retry (withNodeConnectRetry)
import Marconi.Cardano.Core.Runner qualified as ChainIndex.Runner
import Marconi.Cardano.Core.Types (SecurityParam)
import Marconi.Cardano.Indexers.EpochNonce qualified as EpochNonce
import Marconi.Cardano.Indexers.EpochSDD qualified as EpochSDD
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  ExtLedgerStateEvent (extLedgerState),
  ExtLedgerStateWorkerConfig (ExtLedgerStateWorkerConfig),
 )
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator qualified as ExtLedgerState
import Marconi.Cardano.Indexers.MintTokenEvent (MintTokenEventConfig (MintTokenEventConfig))
import Marconi.Cardano.Indexers.Utxo (UtxoIndexerConfig (UtxoIndexerConfig), trackedAddresses)
import Marconi.Core qualified as Core
import Marconi.Sidechain.Api.Types (
  SidechainHttpServerConfig (SidechainHttpServerConfig),
 )
import Marconi.Sidechain.CLI (
  CliArgs (
    CliArgs,
    batchSize,
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
import Marconi.Sidechain.Indexers (
  SidechainBuildIndexersConfig (SidechainBuildIndexersConfig),
  SidechainRunIndexersConfig (SidechainRunIndexersConfig),
  sidechainBuildIndexers,
  sidechainBuildIndexersUtxoConfig,
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
      (runExceptT >=> either throwIO pure) $
        SecurityParam.querySecurityParam @Void networkId socketFilePath

-- | Build the 'SidechainBuildIndexersConfig' from CLI arguments.
mkSidechainBuildIndexersConfig
  :: Trace IO Text -> CliArgs -> SecurityParam -> SidechainBuildIndexersConfig
mkSidechainBuildIndexersConfig trace CliArgs{..} securityParam =
  let
    filteredAddresses =
      maybe
        []
        (map C.AddressShelley . NEList.toList . NESet.toList)
        targetAddresses
    -- Hard-coded with the same values as chain-index
    catchupConfig = Core.mkCatchupConfig batchSize 100
    -- Extract useful information from the Ledger event and blocks
    extLedgerStateAsEvent previousLedgerStateEvent ledgerStateEvent _blockEvent = do
      previousEpochNo <- ExtLedgerState.getEpochNo $ extLedgerState previousLedgerStateEvent
      epochNo <- ExtLedgerState.getEpochNo $ extLedgerState ledgerStateEvent
      guard $ epochNo /= previousEpochNo
      let sdd = EpochSDD.getEpochSDD ledgerStateEvent
          nonce = EpochNonce.getEpochNonce ledgerStateEvent
      pure $ Indexers.EpochEvent epochNo sdd nonce
    -- Snapshot config hard-coded as in chain-index
    epochStateConfig =
      ExtLedgerStateWorkerConfig
        getEvent
        chainDistance
        nodeConfigPath
        100000
        securityParam
        extLedgerStateAsEvent
    mintBurnConfig = MintTokenEventConfig targetAssets
    -- Explicitly does not include the script.
    utxoConfig = UtxoIndexerConfig filteredAddresses False
   in
    SidechainBuildIndexersConfig
      trace
      securityParam
      catchupConfig
      dbDir
      epochStateConfig
      mintBurnConfig
      utxoConfig

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
  -- Local utility copied from Marconi.Cardano.ChainIndex.Run.run
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
    buildIndexersConfig = mkSidechainBuildIndexersConfig trace cliArgs securityParam

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
        (buildIndexersConfig ^. sidechainBuildIndexersUtxoConfig . trackedAddresses)
        (toJSON cliArgs)
        queryables

    sidechainHttpConfig = SidechainHttpServerConfig httpConfig targetAssets

  pure (SidechainEnv sidechainHttpConfig runIndexersConfig)
