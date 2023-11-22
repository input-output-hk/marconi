{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Spec.Marconi.Cardano.Core.Logger qualified as Logger
import Spec.Marconi.Cardano.Core.Orphans qualified as Orphans
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "marconi-cardano-core"
    [Logger.tests, Orphans.tests]
