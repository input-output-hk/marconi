{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.Marconi.Cardano.ChainIndex.Api.Routes qualified as Api.Routes
import Spec.Marconi.Cardano.ChainIndex.Api.Routes qualified as Routes
import Spec.Marconi.Cardano.ChainIndex.CLI qualified as CLI
import Spec.Marconi.Cardano.ChainIndex.CLIInputValidation qualified as CLIInput
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi"
    [ CLIInput.tests
    , CLI.tests
    , Routes.tests
    , Api.Routes.tests
    ]
