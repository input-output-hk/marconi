{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Network.JsonRpc.Client.Types ()
import Spec.Marconi.Sidechain.Experimental.CLI qualified as CLI
import Spec.Marconi.Sidechain.Experimental.CLIInputValidation qualified as CLIInputValidation
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "marconi-sidechain-experimental"
    [ CLI.tests
    , CLIInputValidation.tests
    ]
