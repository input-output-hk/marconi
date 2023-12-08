{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Spec.Marconi.Sidechain.Experimental.Utils where

import Cardano.Api qualified as C
import Cardano.BM.Trace (Trace)
import Control.Concurrent (threadDelay)
import Control.Concurrent qualified as IO
import Control.Exception (bracket, throwIO)
import Control.Lens (folded, set, (^.), (^..))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.List qualified as L
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEList
import Data.Maybe (mapMaybe)
import Data.Monoid (getLast)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog.Extras.Internal.Plan qualified as H
import Hedgehog.Extras.Stock qualified as OS
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Core.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.Cardano.Core.Types (RetryConfig (RetryConfig), TargetAddresses)
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types (
  AddressUtxoResult (AddressUtxoResult, txId, txIx, value),
  GetUtxosFromAddressResult (unAddressUtxosResult),
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Wrappers (ValueWrapper (unValueWrapper))
import Marconi.ChainIndex.Api.Types qualified as ChainIndex.Types
import Marconi.ChainIndex.CLI (StartingPoint (StartFromGenesis))
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig (..))
import Marconi.Sidechain.Experimental.CLI (CliArgs (CliArgs, targetAssets))
import Marconi.Sidechain.Experimental.Env (
  SidechainEnv,
  mkSidechainBuildIndexersConfig,
  mkSidechainEnvFromCliArgs,
 )
import Marconi.Sidechain.Experimental.Indexers (sidechainBuildIndexers)
import Marconi.Sidechain.Experimental.Indexers qualified as Indexers
import Network.JsonRpc.Types (JsonRpcErr)
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath qualified as IO
import System.FilePath.Posix ((</>))
import System.IO qualified as IO
import System.Process qualified as IO
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain
import Test.Gen.Marconi.Cardano.Indexers qualified as Test.Indexers

{- QUERY TEST UTILS -}

{- | Wrapper for building the indexers, indexing the mockchain events, querying via a handler,
and closing the indexers.
-}
queryHandlerWithIndexers
  :: Mockchain.MockchainWithInfoAndDistance C.BabbageEra
  -> IO (SidechainHttpServerConfig, Test.Indexers.TestBuildIndexersResult)
  -> ReaderHandler SidechainHttpServerConfig (Either (JsonRpcErr String) result)
  -> IO result
queryHandlerWithIndexers chain buildAction queryAction = bracket buildAction (Test.Indexers.closeIndexers . snd) $
  \(httpConfig, indexersConfig) -> do
    Test.Indexers.indexAllWithMockchain indexersConfig chain
    threadDelay 500_000
    runReaderT (runExceptT queryAction) httpConfig
      >>= either throwIO pure
      >>= either (fail . show) pure

{- | Dummy CLI arguments from which to create a 'SidechainEnv' used in testing via
'mkSidechainEnvFromCliArgs'. Fields can be updated as needed for different tests.
-}
initTestingCliArgs :: CliArgs
initTestingCliArgs =
  CliArgs
    ""
    -- TODO: PLT-8634 this needs to be valid since ExtLedgerStateCoordinator builder calls readGenesisFile.
    "../config/cardano-node/mainnet/config.json"
    -- dbPath "" uses temporary dbs
    ""
    8080
    C.Mainnet
    Nothing
    Nothing
    retryConfig
    StartFromGenesis
  where
    retryConfig = RetryConfig 1 (Just 16)

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
  (trace, _) <- liftIO $ defaultStdOutLogger "marconi-sidechain-experimental-test"
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
        trace
        (mkMarconiTrace trace)
        (config ^. Indexers.sidechainBuildIndexersDbPath)

  buildIndexersConfig <- either (liftIO . throwIO) pure res

  let
    httpConfig =
      ChainIndex.Types.HttpServerConfig
        trace
        8080
        0
        (config ^. Indexers.sidechainBuildIndexersUtxoConfig . Utxo.trackedAddresses)
        (A.toJSON cliArgs)
        (buildIndexersConfig ^. Test.Indexers.testBuildIndexersResultQueryables)
    sidechainHttpConfig = SidechainHttpServerConfig httpConfig (targetAssets cliArgs)

  pure (sidechainHttpConfig, buildIndexersConfig)

-- | Get the addresses from a timed 'UtxoEvent' with distance.
addressesFromTimedUtxoEvent
  :: Core.Timed C.ChainPoint (WithDistance (Maybe Utxo.UtxoEvent)) -> [C.AddressAny]
addressesFromTimedUtxoEvent = L.nub . getAddrs
  where
    getAddrs :: Core.Timed C.ChainPoint (WithDistance (Maybe Utxo.UtxoEvent)) -> [C.AddressAny]
    getAddrs e = e ^. Core.event ^.. folded . folded . folded . Utxo.address

addressAnysToTargetAddresses :: [C.AddressAny] -> Maybe TargetAddresses
addressAnysToTargetAddresses = NESet.nonEmptySet . Set.fromList . mapMaybe op
  where
    op (C.AddressShelley addr) = Just addr
    op _ = Nothing

{- | Create uniform actual/expected results from the GetUtxosFromAddressResult query, for comparison
 - with equality. This is to give better counterexample reporting with '==='.
   It is the caller's job to ensure the query in fact did use the provided address, since that is not in the result.
   Utxos are considered equal here if they are associated with the same address (assumed),
   have the same @C.'TxIn'@ and the same 'value'.
-}
uniformGetUtxosFromAddressResult
  :: C.AddressAny
  -> GetUtxosFromAddressResult
  -> [Utxo.UtxoEvent]
  -> ([(C.TxIn, C.Value)], [(C.TxIn, C.Value)])
uniformGetUtxosFromAddressResult target result inputs = (sortUniqueOnTxIn actual, sortUniqueOnTxIn expected)
  where
    sortUniqueOnTxIn = List.sortOn fst . List.nub
    actual = map (\x -> (C.TxIn (txId x) (txIx x), unValueWrapper $ value x)) $ unAddressUtxosResult result
    blockUtxosToExpected :: Utxo.UtxoEvent -> [(C.TxIn, C.Value)]
    blockUtxosToExpected = map (\x -> (x ^. Utxo.txIn, x ^. Utxo.value)) . NEList.filter (\x -> x ^. Utxo.address == target)
    expected = concatMap blockUtxosToExpected inputs

{- GOLDEN TEST UTILS -}

-- Capture handle contents in a separate thread (e.g. stderr)
captureHandleContents :: IO.Handle -> IO BSL.ByteString
captureHandleContents handle = do
  mvar <- IO.newEmptyMVar
  _ <- IO.forkIO $ do
    contents <- BSL.hGetContents handle
    IO.putMVar mvar contents
  IO.takeMVar mvar

-- -- | Compute the path to the binary given a package name or an environment variable override.
binFlex
  :: String
  -- ^ Package name
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> IO FilePath
  -- ^ Path to executable
binFlex pkg binaryEnv = do
  maybeEnvBin <- liftIO $ IO.lookupEnv binaryEnv
  case maybeEnvBin of
    Just envBin -> return envBin
    Nothing -> binDist pkg

addExeSuffix :: String -> String
addExeSuffix s =
  if ".exe" `L.isSuffixOf` s
    then s
    else s <> if OS.isWin32 then ".exe" else ""

-- -- | Find the nearest plan.json going upwards from the current directory.
findDefaultPlanJsonFile :: IO FilePath
findDefaultPlanJsonFile = IO.getCurrentDirectory >>= go
  where
    go :: FilePath -> IO FilePath
    go d = do
      let file = d </> "dist-newstyle/cache/plan.json"
      exists <- IO.doesFileExist file
      if exists
        then return file
        else do
          let parent = IO.takeDirectory d
          if parent == d
            then return "dist-newstyle/cache/plan.json"
            else go parent

-- -- | Discover the location of the plan.json file.
planJsonFile :: IO String
planJsonFile = do
  maybeBuildDir <- liftIO $ IO.lookupEnv "CABAL_BUILDDIR"
  case maybeBuildDir of
    Just buildDir -> return $ ".." </> buildDir </> "cache/plan.json"
    Nothing -> findDefaultPlanJsonFile

-- -- | Consult the "plan.json" generated by cabal to get the path to the executable corresponding.
-- -- to a haskell package.  It is assumed that the project has already been configured and the
-- -- executable has been built.
binDist
  :: String
  -- ^ Package name
  -> IO FilePath
  -- ^ Path to executable
binDist pkg = do
  contents <- BSL.readFile =<< planJsonFile

  case A.eitherDecode contents of
    Right plan -> case L.filter matching (plan & H.installPlan) of
      (component : _) -> case component & H.binFile of
        Just bin -> return $ addExeSuffix (T.unpack bin)
        Nothing -> error $ "missing bin-file in: " <> show component
      [] -> error $ "Cannot find exe:" <> pkg <> " in plan"
    Left message -> error $ "Cannot decode plan: " <> message
  where
    matching :: H.Component -> Bool
    matching component = case H.componentName component of
      Just name -> name == "exe:" <> T.pack pkg
      Nothing -> False

{- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
corresponding to the executable, an environment variable pointing to the executable,
and an argument list.

'flex' means that the environment determines how the process is launched:
When the environment variable is set, the `binaryEnv` describes the location of the executable
to use. This applies to marconi's CI nix environment but not the local shell.
When the environment variable is not available, the `pkg` describes the name of the binary to
launch. It will be found instead by consulting the "plan.json" generated by cabal.
It is assumed that the project has already been configured and the executable has been built.
-}
procFlex
  :: String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> IO IO.CreateProcess
  -- ^ Captured stdout
procFlex pkg binaryEnv arguments = do
  bin <- binFlex pkg binaryEnv
  return
    (IO.proc bin arguments)
      { IO.env = getLast mempty
      , IO.cwd = getLast mempty
      , -- this allows sending signals to the created processes, without killing the test-suite process
        IO.create_group = True
      }
