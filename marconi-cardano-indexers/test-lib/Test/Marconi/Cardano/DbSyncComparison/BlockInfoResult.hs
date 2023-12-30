{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.DbSyncComparison.BlockInfoResult (
  mkBlockInfoQueryBySlotNoTest,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Lens qualified as Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.Aeson.Types (camelTo2)
import Data.Fixed (Fixed (MkFixed))
import Data.List qualified as List
import Data.Time (secondsToNominalDiffTime)
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Indexers.BlockInfo (BlockInfoBySlotNoQuery (BlockInfoBySlotNoQuery))
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Core.Indexer.SQLiteIndexer qualified as Core
import System.FilePath ((</>))
import Test.Marconi.Cardano.DbSyncComparison.Common (
  Era,
  NodeType (Mainnet),
  eraToString,
  nodeTypeToString,
  queryIndexerOnSnapshot,
  toRuntimeException,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

-- | The result type which will be transformed into JSON.
data BlockInfoResult = BlockInfoResult
  { blockNo :: Integer
  , time :: String
  , epochNo :: Integer
  }

Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = camelTo2 '_'} ''BlockInfoResult

-- | Process the result of the query for serialising to JSON.
toResult :: BlockInfo.BlockInfo -> BlockInfoResult
toResult (BlockInfo.BlockInfo (C.BlockNo bn) timestamp (C.EpochNo en)) =
  let blockNo = toInteger bn
      epochNo = toInteger en
      time = show . secondsToNominalDiffTime . MkFixed . toInteger $ timestamp
   in BlockInfoResult{blockNo, time, epochNo}

{- | Runs the 'BlockInfo' indexer on the snapshot and queries the indexer on the 'C.SlotNo' argument.
 The result is slightly processed and written to disk as JSON.
 This result is compared to the expected output, which was generated using the following query on Db-Sync:

   with block_info as
      ( select block_no, time, epoch_no
        from block
        where slot_no = :slot_no
        limit 1
      )
   select json_agg(block_info) from block_info;

 where :slot_no is the argument.
-}
mkBlockInfoQueryBySlotNoTest :: String -> NodeType -> Era -> C.SlotNo -> TestTree
mkBlockInfoQueryBySlotNoTest testName nodeType era slotNo =
  goldenVsFileDiff
    testName
    (\expected actual -> ["diff", "--color=always", expected, actual])
    goldenFile
    outFile
    runTest
  where
    runTest = do
      indexer <- toRuntimeException $ BlockInfo.mkBlockInfoIndexer Core.inMemoryDB
      queryResult <- queryIndexerOnSnapshot Mainnet subChainPath dbPath blockInfoConfig query indexer
      let finalResult = List.singleton . toResult <$> queryResult
      Aeson.encodeFile outFile finalResult

    outFile = pathToOutFile nodeType era slotNo
    goldenFile = pathToGoldenFile nodeType era slotNo
    dbPath = "test/Spec/Golden/Snapshot/block-info-db/"
    subChainPath = "../../mainnet-snapshots" </> eraToString era
    query = BlockInfoBySlotNoQuery slotNo

blockInfoConfig :: Core.RunIndexerOnSnapshotConfig BlockEvent BlockInfo.BlockInfo
blockInfoConfig =
  Core.RunIndexerOnSnapshotConfig
    (Core.withPreprocessorOnSnapshot BlockInfo.extractBlockInfo (Just . Lens.view BlockInfo.blockNo))
    1 -- is this right?

pathToOutFile :: NodeType -> Era -> C.SlotNo -> FilePath
pathToOutFile (nodeTypeToString -> nodeType) (eraToString -> era) (C.unSlotNo -> slotNo) =
  "test/Spec/Golden/Snapshot" </> nodeType </> era </> "blockinfo-slot" <> show slotNo <> ".out"

pathToGoldenFile :: NodeType -> Era -> C.SlotNo -> FilePath
pathToGoldenFile nt e s = pathToOutFile nt e s <> ".golden"
