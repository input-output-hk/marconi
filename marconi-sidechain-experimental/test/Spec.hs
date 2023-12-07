{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo qualified as PastAddressUtxo
import Spec.Marconi.Sidechain.Experimental.CLI qualified as CLI
import Spec.Marconi.Sidechain.Experimental.CLIInputValidation qualified as CLIInputValidation
import Spec.Marconi.Sidechain.Experimental.Routes qualified as Routes
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "marconi-sidechain-experimental"
    [ PastAddressUtxo.tests
    , CLI.tests
    , CLIInputValidation.tests
    , Routes.tests
    ]
