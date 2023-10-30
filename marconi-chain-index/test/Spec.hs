{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.Marconi.ChainIndex.Api.Routes qualified as Api.Routes
import Spec.Marconi.ChainIndex.Api.Routes qualified as Routes
import Spec.Marconi.ChainIndex.CLI qualified as CLI
import Spec.Marconi.ChainIndex.CLIInputValidation qualified as CLIInput
import Spec.Marconi.ChainIndex.Indexers qualified as Indexers
import Spec.Marconi.ChainIndex.Logger qualified as Logger
import Spec.Marconi.ChainIndex.Orphans qualified as Orphans
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi"
    [ Routes.tests
    , CLIInput.tests
    , CLI.tests
    , Logger.tests
    , Orphans.tests
    , Api.Routes.tests
    , Indexers.tests
    ]
