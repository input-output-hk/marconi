{-# LANGUAGE TemplateHaskell #-}

module Test.Marconi.Cardano.DbSyncComparison.UtxoResult (
  mkUtxoEventTest,
) where

import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Monad ((<=<))
import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.Aeson.Types (camelTo2)
import Data.List.NonEmpty qualified as NE
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Core.Types (AnyTxBody (AnyTxBody))
import Marconi.Cardano.Indexers.Utxo (Utxo, UtxoEvent)
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Core.Indexer.SQLiteIndexer qualified as Core
import Marconi.Core.Query (EventAtQuery (EventAtQuery))
import System.FilePath ((</>))
import Test.Marconi.Cardano.DbSyncComparison.Common (
  DbSyncComparisonConfig (DbSyncComparisonConfig),
  eraToString,
  pathToGoldenFile,
  pathToOutFile,
  queryIndexerOnSnapshot,
  toRuntimeException,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

newtype UtxoResult = UtxoResult UtxoEvent

Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = camelTo2 '_'} ''UtxoResult

toResult :: UtxoEvent -> UtxoResult
toResult = UtxoResult

mkUtxoEventTest :: String -> DbSyncComparisonConfig -> TestTree
mkUtxoEventTest testName cfg@(DbSyncComparisonConfig nodeType era _ dbPath _ _) =
  goldenVsFileDiff
    testName
    (\expected actual -> ["diff", "--color=always", expected, actual])
    goldenFile
    outFile
    runTest
  where
    runTest = do
      indexer <- toRuntimeException $ Utxo.mkUtxoIndexer Core.inMemoryDB
      queryResult <-
        queryIndexerOnSnapshot nodeType subChainPath dbPath utxoConfig Nothing query indexer
      let finalResult = toResult <$> queryResult
      Aeson.encodeFile outFile finalResult

    outFile = pathToOutFile cfg
    goldenFile = pathToGoldenFile cfg
    subChainPath = "../../mainnet-snapshots" </> eraToString era
    query = EventAtQuery

utxoConfig :: Core.RunIndexerOnSnapshotConfig BlockEvent UtxoEvent
utxoConfig =
  Core.RunIndexerOnSnapshotConfig
    -- TODO: I think this needs to be (WithDistance UtxoEvent) because:
    --  1. we need data about the blockNo
    --  2. inside the Queryable instance, we're inspecting some ChainPoint
    --  which I _think_ comes from WithDistance, need to investigate more
    (Core.withPreprocessorOnSnapshot extractUtxoEvent (const Nothing))
    1 -- is this right?

extractUtxoEvent :: BlockEvent -> UtxoEvent
extractUtxoEvent =
  NE.fromList . (toUtxos <=< Utxo.toTxBodys)
  where
    toUtxos :: AnyTxBody -> [Utxo]
    toUtxos (AnyTxBody _ txIndexInBlock txBody) =
      Utxo.getUtxosFromTxBody txIndexInBlock txBody
