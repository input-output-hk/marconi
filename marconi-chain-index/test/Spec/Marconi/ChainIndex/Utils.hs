{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Utils where

import Control.Concurrent qualified as IO
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.List qualified as L
import Data.Monoid (getLast)
import Data.Text qualified as T
import Hedgehog.Extras.Internal.Plan qualified as H
import Hedgehog.Extras.Stock qualified as OS
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath qualified as IO
import System.FilePath.Posix ((</>))
import System.IO qualified as IO
import System.Process qualified as IO

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
