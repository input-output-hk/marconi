{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.Marconi.ChainIndex.Experimental.Api.Routes qualified as Experimental.Api.Routes
import Spec.Marconi.ChainIndex.Experimental.Indexers qualified as Experimental.Indexers
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi"
    [ -- TODO: PLT-7727 CLI module was shared, will need to add it back in and add the tests for it.
      -- , CLI.tests
      Experimental.Api.Routes.tests
    , Experimental.Indexers.tests
    ]
