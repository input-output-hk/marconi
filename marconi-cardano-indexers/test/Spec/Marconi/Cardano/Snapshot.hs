module Spec.Marconi.Cardano.Snapshot (tests) where

import Streaming.Prelude qualified as Stream
import Test.Marconi.Cardano.Snapshot (Snapshot (..), mkSnapshot)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    [t]

t :: TestTree
t =
  testGroup
    "TESTING"
    [ testCase "" $ do
        let configFile = "/home/ana/Workspace/IOG/marconi/config/cardano-node/preprod/config.json"
            testDir = "/home/ana/Workspace/IOG/marconi/testing-snapshot/1"
        snapshot <- mkSnapshot configFile testDir
        print (snapshotPreviousLedgerState snapshot)
        Stream.print (snapshotBlockStream snapshot)
    ]
