{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.Marconi.ChainIndex.Api.Routes qualified as Api.Routes
import Spec.Marconi.ChainIndex.CLI qualified as CLI
import Spec.Marconi.ChainIndex.Indexers qualified as Indexers
import Spec.Marconi.ChainIndex.Logger qualified as Logger
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi"
    [ CLI.tests
    , Logger.tests
    , Api.Routes.tests
    , Indexers.tests
    ]
