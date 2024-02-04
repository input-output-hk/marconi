{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Testing utilities that have no home elsewhere.
module Spec.Marconi.Sidechain.Utils where

import Cardano.Api qualified as C
import Cardano.BM.Tracing qualified as BM
import Control.Exception (throwIO)
import Control.Lens (set, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as A
import Data.Functor ((<&>))
import Marconi.Cardano.ChainIndex.Api.Types qualified as ChainIndex.Types
import Marconi.Cardano.ChainIndex.CLI (StartingPoint (StartFromGenesis))
import Marconi.Cardano.Core.Logger (defaultStdOutLogger)
import Marconi.Cardano.Core.Types qualified as Core.Types
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Core qualified as Core
import Marconi.Sidechain.Api.Types (
  SidechainHttpServerConfig (SidechainHttpServerConfig),
 )
import Marconi.Sidechain.CLI (CliArgs (CliArgs, targetAssets))
import Marconi.Sidechain.Env (
  mkSidechainBuildIndexersConfig,
 )
import Marconi.Sidechain.Indexers qualified as Indexers
import System.Environment (getEnv)
import Test.Marconi.Cardano.ChainIndex.Indexers qualified as Test.Indexers

{- | Utility for testing JSON RPC handlers, mainly.
 - Construct the 'SidechainHttpServerConfig' and indexers in the same way as 'mkSidechainEnvFromCliArgs',
 - including some hard-coded parameters in 'mkSidechainBuildIndexersConfig',
 - except using @Test.Indexers.'buildIndexers'@. We need to expose the underlying indexers for direct indexing with
 - randomly generated events. Fixes the security parameter to 0 since this assumes no rollbacks are
 - tested.
-}
mkTestSidechainConfigsFromCliArgs
  :: (MonadIO m) => CliArgs -> m (SidechainHttpServerConfig, Test.Indexers.TestBuildIndexersResult)
mkTestSidechainConfigsFromCliArgs cliArgs = do
  (trace, _) <- liftIO $ defaultStdOutLogger "marconi-sidechain-experimental-test" BM.Info
  let
    -- Fixing security param at 0. No rollbacks.
    securityParam = 0
    -- Set the catchup params to those appropriate for indexing few events.
    config =
      set Indexers.sidechainBuildIndexersCatchupConfig (Core.mkCatchupConfig 1 0) $
        mkSidechainBuildIndexersConfig trace cliArgs securityParam
  res <-
    liftIO . runExceptT $
      Test.Indexers.buildIndexers
        (config ^. Indexers.sidechainBuildIndexersSecurityParam)
        (config ^. Indexers.sidechainBuildIndexersCatchupConfig)
        (config ^. Indexers.sidechainBuildIndexersUtxoConfig)
        (config ^. Indexers.sidechainBuildIndexersMintTokenEventConfig)
        (config ^. Indexers.sidechainBuildIndexersEpochStateConfig)
        (config ^. Indexers.sidechainBuildIndexersDbPath)

  buildIndexersConfig <- either (liftIO . throwIO) pure res

  let
    httpConfig =
      ChainIndex.Types.HttpServerConfig
        trace
        3_000
        0
        (config ^. Indexers.sidechainBuildIndexersUtxoConfig . Utxo.trackedAddresses)
        (A.toJSON cliArgs)
        (buildIndexersConfig ^. Test.Indexers.testBuildIndexersResultQueryables)
    sidechainHttpConfig = SidechainHttpServerConfig httpConfig (targetAssets cliArgs)

  pure (sidechainHttpConfig, buildIndexersConfig)

{- | Dummy CLI arguments from which to create a 'SidechainEnv' used in testing via
'mkSidechainEnvFromCliArgs'. Fields can be updated as needed for different tests.
-}
initTestingCliArgs :: CliArgs
initTestingCliArgs =
  CliArgs
    False
    ""
    ""
    -- dbPath "" uses temporary files
    ""
    3000
    C.Mainnet
    1000
    Nothing
    Nothing
    retryConfig
    StartFromGenesis
  where
    retryConfig = Core.Types.RetryConfig 1 (Just 16)

getNodeConfigPath :: IO FilePath
getNodeConfigPath =
  getEnv "CARDANO_NODE_CONFIG"
    <&> (++ "/cardano-node/mainnet/config.json")
