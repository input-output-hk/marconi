{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Marconi.ChainIndex.Logger (defaultStdOutLogger)
import Network.JsonRpc.Client.Types ()
import Spec.Marconi.Sidechain.Experimental.CLI qualified as CLI
import System.Directory (getTemporaryDirectory)
import System.IO.Temp (withTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)

-- Tests are run in a temporary directory for later use with Warp.testWithApplication,
-- as tests analogous to those in marconi-sidechain are added.
main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "marconi-sidechain" $ \tempDir -> do
    (trace, sb) <- defaultStdOutLogger "marconi-sidechain-experimental-test"
    defaultMain tests

tests :: TestTree
tests = testGroup "marconi-sidechain-experimental" [CLI.tests]
