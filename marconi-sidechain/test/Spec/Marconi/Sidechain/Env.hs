{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Sidechain.Env where

import Cardano.BM.Trace (logInfo)
import Control.Lens ((^.))
import Marconi.Sidechain.CLI qualified as CLI
import Marconi.Sidechain.Env (SidechainEnv, sidechainCliArgs, sidechainTrace)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

tests :: SidechainEnv -> TestTree
tests env =
  testGroup
    "marconi-sidechain Env Specs"
    [ testCase "sidechainCliArgs" $ do
        let cliArgs = env ^. sidechainCliArgs
        assertFileExists $ CLI.nodeConfigPath cliArgs
        assertDirectoryExists $ CLI.dbDir cliArgs
        assertDirectoryExists $ takeDirectory $ CLI.socketFilePath cliArgs
    , testCase "sidechainTrace" $ do
        let trace = env ^. sidechainTrace
        -- For now, we just check that it doesn't throw
        logInfo trace "Successful trace"
    ]

assertFileExists :: FilePath -> Assertion
assertFileExists fp = doesFileExist fp >>= assertBool ("File " <> fp <> " should exist")

assertDirectoryExists :: FilePath -> Assertion
assertDirectoryExists fp = doesDirectoryExist fp >>= assertBool ("Directory " <> fp <> " should exist")
