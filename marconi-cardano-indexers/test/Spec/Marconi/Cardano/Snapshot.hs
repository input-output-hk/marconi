{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Concurrent (readMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Lens qualified as Lens
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.Aeson.Types (camelTo2)
import Data.Fixed (Fixed (MkFixed))
import Data.Time (secondsToNominalDiffTime)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Indexers.BlockInfo (BlockInfoBySlotNoQuery (BlockInfoBySlotNoQuery))
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Core.Class qualified as Core (queryLatest)
import Marconi.Core.Indexer.ListIndexer qualified as Core (
  mkListIndexer,
 )
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

data BlockInfoResult = BlockInfoResult
  { blockNo :: Integer
  , time :: String
  , epochNo :: Integer
  }

Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = camelTo2 '_'} ''BlockInfoResult

toResult :: BlockInfo.BlockInfo -> BlockInfoResult
toResult (BlockInfo.BlockInfo (C.BlockNo bn) timestamp (C.EpochNo en)) =
  let blockNo = toInteger bn
      epochNo = toInteger en
      time = show . secondsToNominalDiffTime . MkFixed . toInteger $ timestamp -- formatTime defaultTimeLocale "%c" .
   in BlockInfoResult{blockNo, time, epochNo}

runBlockInfoTest
  :: NodeType
  -> FilePath
  -- ^ directory which contains the serialised sub-chain
  -> FilePath
  -- ^ directory to be used as the indexer's DB
  -> Core.RunIndexerOnSnapshotConfig BlockEvent BlockInfo.BlockInfo
  -> BlockInfoBySlotNoQuery BlockInfo.BlockInfo
  -> IO (Maybe BlockInfo.BlockInfo)
runBlockInfoTest nodeType subChainPath dbPath config query = do
  configFile <- getNodeConfigPath nodeType
  blockStream <- setupSnapshot configFile subChainPath dbPath
  blockInfoIndexer <- toRuntimeException $ BlockInfo.mkBlockInfoIndexer (dbPath </> BlockInfo.dbName)
  let runIndexer = Core.runIndexerOnSnapshot config blockInfoIndexer blockStream
  withAsync runIndexer $ \runner -> do
    putStrLn "\n\nWaiting for indexer..."
    indexer <- wait runner >>= readMVar
    putStrLn "\nQuerying the database..."
    toRuntimeException $ Core.queryLatest query indexer

-- cleanup temporary indexer database
-- removeDirectoryRecursive dbPath
-- pure result

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

blockInfoConfig :: Core.RunIndexerOnSnapshotConfig BlockEvent BlockInfo.BlockInfo
blockInfoConfig =
  Core.RunIndexerOnSnapshotConfig
    (Core.withPreprocessorOnSnapshot BlockInfo.extractBlockInfo (Just . Lens.view BlockInfo.blockNo))
    1 -- is this right?

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    [ testDeserialiseSnapshot
    , testRunListIndexerOnSnapshot
    , testBlockInfo
    , testBlockInfoByronSlot6
    , testBlockInfoByronSlot206938
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
   in testCase "ZZZTestRunListIndexerOnSnapshot" (runListIndexerTest Preprod dbPath config)

testBlockInfo :: TestTree
testBlockInfo =
  let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
      subChainPath = "test/Spec/Golden/Snapshot/preprod-5-10"
      expectedResult = Just (BlockInfo.BlockInfo (C.BlockNo 7) 12540597 (C.EpochNo 0))
      query = BlockInfoBySlotNoQuery 12961 :: BlockInfoBySlotNoQuery BlockInfo.BlockInfo
   in testCase "TestBlockInfo" $ do
        actualResult <- runBlockInfoTest Preprod subChainPath dbPath blockInfoConfig query
        assertEqual "" expectedResult actualResult

-- Corresponding db-sync query:
--   with block_info as (select block_no, time, epoch_no from block where slot_no = 6 limit 1) select json_agg(block_info) from block_info;
testBlockInfoByronSlot6 :: TestTree
testBlockInfoByronSlot6 =
  goldenVsFileDiff
    "Query BlockInfo slot 6, Byron era"
    (\expected actual -> ["diff", "--color=always", expected, actual])
    "test/Spec/Golden/Snapshot/mainnet/byron1/blockinfo-slot6.out.golden"
    "test/Spec/Golden/Snapshot/mainnet/byron1/blockinfo-slot6.out"
    run
  where
    run = do
      let dbPath = "test/Spec/Golden/Snapshot/mainnet-1-db/"
          subChainPath = "../mainnet-snapshots/1"
          query = BlockInfoBySlotNoQuery 6 :: BlockInfoBySlotNoQuery BlockInfo.BlockInfo
      Just queryResult <- runBlockInfoTest Mainnet subChainPath dbPath blockInfoConfig query
      let finalResult = toResult queryResult
      Aeson.encodeFile "test/Spec/Golden/Snapshot/mainnet/byron1/blockinfo-slot6.out" [finalResult]

-- Corresponding db-sync query:
--   with block_info as (select block_no, time, epoch_no from block where slot_no = 206938 limit 1) select json_agg(block_info) from block_info;
testBlockInfoByronSlot206938 :: TestTree
testBlockInfoByronSlot206938 =
  goldenVsFileDiff
    "TESTING Query BlockInfo slot 206938, Byron era"
    (\expected actual -> ["diff", "--color=always", expected, actual])
    "test/Spec/Golden/Snapshot/mainnet/byron1/blockinfo-slot206938.out.golden"
    "test/Spec/Golden/Snapshot/mainnet/byron1/blockinfo-slot206938.out"
    run
  where
    run = do
      let dbPath = "test/Spec/Golden/Snapshot/mainnet-1-db/"
          subChainPath = "../mainnet-snapshots/1"
          query = BlockInfoBySlotNoQuery 206938 :: BlockInfoBySlotNoQuery BlockInfo.BlockInfo
      Just queryResult <- runBlockInfoTest Mainnet subChainPath dbPath blockInfoConfig query
      let finalResult = toResult queryResult
      Aeson.encodeFile "test/Spec/Golden/Snapshot/mainnet/byron1/blockinfo-slot206938.out" [finalResult]
