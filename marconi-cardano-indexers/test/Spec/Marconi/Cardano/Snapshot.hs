{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Concurrent (readMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Lens qualified as Lens
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.ListIndexer qualified as ListIndexer
import Marconi.Core.Type (event)
import Streaming.Prelude qualified as Stream
import System.Directory (removeDirectoryRecursive)
import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Marconi.Cardano.Snapshot (
  setupSnapshot,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    [testRunListIndexerOnSnapshot]

testRunListIndexerOnSnapshot :: TestTree
testRunListIndexerOnSnapshot =
  let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
      config =
        Core.RunIndexerOnSnapshotConfig
          Core.withNoPreprocessorOnSnapshot
          1
   in testCase "TestRunListIndexerOnSnapshot" (run dbPath config)

run
  :: FilePath
  -> Core.RunIndexerOnSnapshotConfig BlockEvent BlockEvent
  -> IO ()
run dbPath config = do
  configFile <- getNodeConfigPath "preprod"
  blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
  actualResultRaw <-
    withAsync (Core.runIndexerOnSnapshot config Core.mkListIndexer blockStream) $ \runner -> do
      mVar <- wait runner
      indexer <- readMVar mVar
      removeDirectoryRecursive dbPath
      return (Lens.view ListIndexer.events indexer)
  expectedResult <- Stream.toList_ blockStream
  -- the list indexer stores events in reverse order
  let actualResult = reverse $ Lens.view event <$> actualResultRaw
  assertEqual "" (show <$> expectedResult) (show <$> actualResult)

getNodeConfigPath :: String -> IO FilePath
getNodeConfigPath nodeType = do
  configDir <- getEnv "CARDANO_NODE_CONFIG"
  return $ configDir </> "cardano-node" </> nodeType </> "config.json"
