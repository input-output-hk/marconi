{-# LANGUAGE FlexibleContexts #-}

module Spec.Marconi.Sidechain.Experimental.Utils where

import Cardano.Api qualified as C
import Cardano.BM.Trace (Trace)
import Control.Concurrent qualified as IO
import Control.Exception (throwIO)
import Control.Lens (folded, (^.), (^..))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEList
import Data.Maybe (mapMaybe)
import Data.Monoid (getLast)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog.Extras.Internal.Plan qualified as H
import Hedgehog.Extras.Stock qualified as OS
import Marconi.Cardano.Core.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.Cardano.Core.Types (RetryConfig (RetryConfig), TargetAddresses)
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.CLI (StartingPoint (StartFromGenesis))
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig (..))
import Marconi.Sidechain.Experimental.CLI (CliArgs (CliArgs, dbDir, targetAddresses))
import Marconi.Sidechain.Experimental.Env (
  SidechainEnv,
  mkSidechainBuildIndexersConfig,
  mkSidechainEnvFromCliArgs,
 )
import Marconi.Sidechain.Experimental.Indexers qualified as Indexers
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath qualified as IO
import System.FilePath.Posix ((</>))
import System.IO qualified as IO
import System.Process qualified as IO
import Test.Gen.Marconi.Cardano.Indexers qualified as Test.Indexers

{- QUERY TEST UTILS -}

{- | Dummy CLI arguments from which to create a 'SidechainEnv' used in testing via
'mkSidechainEnvFromCliArgs'. Fields can be updated as needed for different tests.
-}
initTestingCliArgs :: CliArgs
initTestingCliArgs =
  CliArgs
    "cardano-node.socket"
    ""
    "."
    8080
    C.Mainnet
    Nothing
    Nothing
    retryConfig
    StartFromGenesis
  where
    retryConfig = RetryConfig 1 (Just 16)

{- | Combines mkSidechainBuildIndexersConfig and sidechainBuildIndexers into one,
but using the 'Test.Indexers.buildIndexers' instead so as to expose the individual
indexers for the purpose of indexing generated events.

  A convenience for testing indexers using the same inputs as what the CLI sees.
-}
buildTestIndexersFromCliArgs
  :: (MonadIO m)
  => Trace IO Text
  -> CliArgs
  -> m Test.Indexers.TestBuildIndexersResult
buildTestIndexersFromCliArgs trace cliArgs = do
  let
    -- Fixing security param at 0. No rollbacks.
    config = mkSidechainBuildIndexersConfig trace cliArgs 0

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

  liftIO $ either throwIO pure res

-- TODO: PLT-8634 conveniece function for building indexers that can easily be indexed with
-- generated events. useful for testing handlers.
mkTestSidechainHttpServerConfigFromCliArgs
  :: (MonadIO m) => CliArgs -> m (SidechainHttpServerConfig, Test.Indexers.TestBuildIndexersResult)
mkTestSidechainHttpServerConfigFromCliArgs = undefined

-- TODO: PLT-8634 this can be deleted i think

{- | Quick-start version of 'mkSidechainEnvFromCliArgs' for use in testing.
Security parameter set to 0, meaning rollbacks are not supported.
-}
mkTestSidechainEnvFromCliArgs :: (MonadIO m) => CliArgs -> m SidechainEnv
mkTestSidechainEnvFromCliArgs cliArgs =
  liftIO $
    defaultStdOutLogger "marconi-sidechain-experimental-test"
      >>= \(trace, sb) -> mkSidechainEnvFromCliArgs trace sb cliArgs 0

-- | List of unique addresses from a list of timed 'UtxoEvent'.
addressesFromTimedUtxoEvents :: [Core.Timed C.ChainPoint (Maybe Utxo.UtxoEvent)] -> [C.AddressAny]
addressesFromTimedUtxoEvents = L.nub . concatMap getAddrs
  where
    getAddrs :: Core.Timed C.ChainPoint (Maybe Utxo.UtxoEvent) -> [C.AddressAny]
    getAddrs e = e ^. Core.event ^.. folded . folded . Utxo.address

addressAnysToTargetAddresses :: [C.AddressAny] -> Maybe TargetAddresses
addressAnysToTargetAddresses = NESet.nonEmptySet . Set.fromList . mapMaybe op
  where
    op (C.AddressShelley addr) = Just addr
    op _ = Nothing

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
