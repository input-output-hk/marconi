module Spec.Marconi.Cardano.Snapshot (tests) where

import Streaming.Prelude qualified as Stream
import System.IO (IOMode (WriteMode), hPrint, withFile)
import Test.Marconi.Cardano.Snapshot (
  Snapshot (snapshotBlockStream, snapshotPreviousLedgerState),
  mkSnapshot,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    [testDeserializeSnapshot]

testDeserializeSnapshot :: TestTree
testDeserializeSnapshot =
  goldenVsFileDiff
    "TestDeserializeSnapshot"
    (\expected actual -> ["diff", "--color=always", expected, actual])
    "test/Spec/Golden/Snapshot/preprod-5-10-subchain.golden"
    "test/Spec/Golden/Snapshot/preprod-5-10-subchain.out"
    run
  where
    run = do
      let configFile = "../config/cardano-node/preprod/config.json"
      snapshot <- mkSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10"
      withFile "test/Spec/Golden/Snapshot/preprod-5-10-subchain.out" WriteMode $ \handle -> do
        hPrint handle (snapshotPreviousLedgerState snapshot)
        Stream.toHandle handle (Stream.map show $ snapshotBlockStream snapshot)
