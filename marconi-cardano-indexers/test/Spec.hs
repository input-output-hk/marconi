module Main (main) where

import Spec.Marconi.Cardano.Indexers qualified as Indexers
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "marconi-cardano-indexers"
    [Indexers.tests]
