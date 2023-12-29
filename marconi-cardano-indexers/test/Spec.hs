module Main (main) where

import Spec.Marconi.Cardano.DbSyncComparison qualified as DbSyncComparison
import Spec.Marconi.Cardano.Indexers qualified as Indexers
import Spec.Marconi.Cardano.Snapshot qualified as Snapshot
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "marconi-cardano-indexers"
    [ Indexers.tests
    , Snapshot.tests
    , DbSyncComparison.tests
    ]
