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
   in testCase "TestRunListIndexerOnSnapshot" (run Preprod dbPath config)

{- | Run a simple list indexer on given snapshot. The list indexer stores
each event as-is. The test checks whether the resulting set of events
is equal to the initial set of events.
-}
run
  :: NodeType
  -> FilePath
  -- ^ directory which contains the serialised events
  -> Core.RunIndexerOnSnapshotConfig BlockEvent BlockEvent
  -> IO ()
run nodeType dbPath config = do
  configFile <- getNodeConfigPath nodeType
  blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
  actualResultRaw <-
    withAsync (Core.runIndexerOnSnapshot config Core.mkListIndexer blockStream) $ \runner -> do
      mVar <- wait runner
      indexer <- readMVar mVar
      -- cleanup temporary indexer database
      removeDirectoryRecursive dbPath
      return (Lens.view ListIndexer.events indexer)
  expectedResult <- Stream.toList_ blockStream
  -- the list indexer stores events in reverse order
  let actualResult = reverse $ Lens.view event <$> actualResultRaw
  -- 'BlockEvent' doesn't have an 'Eq' instance, so a workaround is needed
  assertEqual "" (show <$> expectedResult) (show <$> actualResult)

-- | The Cardano network type used to create the snapshot.
data NodeType = Preview | Preprod | Mainnet

{- | The path to a data directory containing configuration files
is set to an environment variable. This function retrieves the right
configuration file path.
-}
getNodeConfigPath :: NodeType -> IO FilePath
getNodeConfigPath nodeType = do
  configDir <- getEnv "CARDANO_NODE_CONFIG"
  return $ configDir </> "cardano-node" </> toPath nodeType </> "config.json"
  where
    toPath Preview = "preview"
    toPath Preprod = "preprod"
    toPath Mainnet = "mainnet"
