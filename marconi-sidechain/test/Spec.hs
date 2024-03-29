{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Spec.Marconi.Sidechain.Api.JsonRpc.Endpoint.BurnTokenEvent qualified as BurnTokenEvent
import Spec.Marconi.Sidechain.Api.JsonRpc.Endpoint.PastAddressUtxo qualified as PastAddressUtxo
import Spec.Marconi.Sidechain.CLI qualified as CLI
import Spec.Marconi.Sidechain.CLIInputValidation qualified as CLIInputValidation
import Spec.Marconi.Sidechain.Routes qualified as Routes
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "marconi-sidechain"
    [ BurnTokenEvent.tests
    , PastAddressUtxo.tests
    , CLI.tests
    , CLIInputValidation.tests
    , Routes.tests
    ]
