{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Concurrent (readMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Lens qualified as Lens
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Indexers.BlockInfo (BlockInfoBySlotNoQuery (BlockInfoBySlotNoQuery))
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.ListIndexer qualified as ListIndexer
import Marconi.Core.Type (event)
import Streaming.Prelude qualified as Stream
import System.Directory (removeDirectoryRecursive)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPrint, withFile)
import Test.Marconi.Cardano.Snapshot (
  deserialiseSnapshot,
  setupSnapshot,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    [ testDeserialiseSnapshot
    , testRunListIndexerOnSnapshot
    , testBlockInfo
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

testBlockInfo :: TestTree
testBlockInfo =
  let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
      config =
        Core.RunIndexerOnSnapshotConfig
          (Core.withPreprocessorOnSnapshot BlockInfo.extractBlockInfo (Just . Lens.view BlockInfo.blockNo))
          1
   in testCase "TestBlockInfo" (runBlockInfo Preprod dbPath config)

runBlockInfo
  :: NodeType
  -> FilePath
  -- ^ directory to be used as the indexer's DB
  -> Core.RunIndexerOnSnapshotConfig BlockEvent BlockInfo.BlockInfo
  -> IO ()
runBlockInfo nodeType dbPath config = do
  configFile <- getNodeConfigPath nodeType
  blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
  blockInfoIndexer <- toRuntimeException $ BlockInfo.mkBlockInfoIndexer (dbPath </> BlockInfo.dbName)
  let runIndexer = Core.runIndexerOnSnapshot config blockInfoIndexer blockStream
  actualResult <-
    withAsync runIndexer $ \runner -> do
      indexer <- wait runner >>= readMVar
      result <-
        toRuntimeException $
          Core.queryLatest
            (BlockInfoBySlotNoQuery 12961 :: BlockInfoBySlotNoQuery BlockInfo.BlockInfo)
            indexer
      -- cleanup temporary indexer database
      removeDirectoryRecursive dbPath
      pure result
  let expectedResult = Just (BlockInfo.BlockInfo (C.BlockNo 7) 12540597 (C.EpochNo 0))
  assertEqual "" expectedResult actualResult

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

-- | The Cardano network type used to create the snapshot.
data NodeType = Preview | Preprod | Mainnet

{- | The path to a data directory containing configuration files
is set to an environment variable. This function retrieves the right
configuration file path.
-}
getNodeConfigPath :: NodeType -> IO FilePath
getNodeConfigPath nodeType = do
  configDir <- getEnv "CARDANO_NODE_CONFIG"
  pure $ configDir </> "cardano-node" </> toPath nodeType </> "config.json"
  where
    toPath Preview = "preview"
    toPath Preprod = "preprod"
    toPath Mainnet = "mainnet"

toRuntimeException :: (Monad m, Show e) => ExceptT e m a -> m a
toRuntimeException action = do
  result <- runExceptT action
  case result of
    Left err -> error (show err)
    Right a -> pure a
