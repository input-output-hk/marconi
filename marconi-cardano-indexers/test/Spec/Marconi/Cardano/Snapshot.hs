{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Concurrent (readMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Lens qualified as Lens
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Core.Indexer.ListIndexer qualified as Core (
  mkListIndexer,
 )
import Marconi.Core.Indexer.ListIndexer qualified as ListIndexer
import Marconi.Core.Type (event)
import Streaming.Prelude qualified as Stream
import System.Directory (removeDirectoryRecursive)
import System.IO (IOMode (WriteMode), hPrint, withFile)
import Test.Marconi.Cardano.Chain.Snapshot (
  deserialiseSnapshot,
  setupSnapshot,
 )
import Test.Marconi.Cardano.DbSyncComparison.Common (NodeType (Preprod), getNodeConfigPath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    [ testDeserialiseSnapshot
    , testRunListIndexerOnSnapshot
    ]

testDeserialiseSnapshot :: TestTree
testDeserialiseSnapshot =
  goldenVsFileDiff
    "TestDeserialiseSnapshot"
    (\expected actual -> ["diff", "--color=always", expected, actual])
    "test/Spec/Golden/Snapshot/preprod-5-10-subchain.golden"
    "test/Spec/Golden/Snapshot/preprod-5-10-subchain.out"
    run
  where
    run = do
      configFile <- getNodeConfigPath Preprod
      (ledgerState, blockEvents) <-
        deserialiseSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10/"
      withFile "test/Spec/Golden/Snapshot/preprod-5-10-subchain.out" WriteMode $ \handle -> do
        hPrint handle ledgerState
        Stream.toHandle handle (Stream.map show blockEvents)

testRunListIndexerOnSnapshot :: TestTree
testRunListIndexerOnSnapshot =
  let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
      config =
        Core.RunIndexerOnSnapshotConfig
          Core.withNoPreprocessorOnSnapshot
          1
   in testCase "TestRunListIndexerOnSnapshot" (runListIndexerTest Preprod dbPath config)

{- | Run a simple list indexer on given snapshot. The list indexer stores
each event as-is. The test checks whether the resulting set of events
is equal to the initial set of events.
-}
runListIndexerTest
  :: NodeType
  -> FilePath
  -- ^ directory to be used as the indexer's DB
  -> Core.RunIndexerOnSnapshotConfig BlockEvent BlockEvent
  -> IO ()
runListIndexerTest nodeType dbPath config = do
  configFile <- getNodeConfigPath nodeType
  blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
  actualResultRaw <-
    withAsync (Core.runIndexerOnSnapshot config Core.mkListIndexer blockStream) $ \runner -> do
      mVar <- wait runner
      indexer <- readMVar mVar
      -- cleanup temporary indexer database
      removeDirectoryRecursive dbPath
      pure (Lens.view ListIndexer.events indexer)
  expectedResult <- Stream.toList_ blockStream
  -- the list indexer stores events in reverse order
  let actualResult = reverse $ Lens.view event <$> actualResultRaw
  -- 'BlockEvent' doesn't have an 'Eq' instance, so a workaround is needed
  assertEqual "" (show <$> expectedResult) (show <$> actualResult)
