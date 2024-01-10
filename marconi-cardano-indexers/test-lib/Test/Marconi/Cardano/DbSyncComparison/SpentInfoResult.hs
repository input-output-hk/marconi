-- | Utilities for comparing the @Spent.'SpentInfoEvent'@ indexer to cardano-db-sync results.
module Test.Marconi.Cardano.DbSyncComparison.SpentInfoResult where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent))
import Cardano.Api.Shelley qualified as C
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Short qualified as BSS
import Data.List.NonEmpty (nonEmpty)
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Indexers.Spent qualified as Spent
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer qualified as SQLiteIndexer
import System.FilePath ((</>))
import Test.Marconi.Cardano.DbSyncComparison.Common (
  DbSyncComparisonConfig (DbSyncComparisonConfig),
  eraToString,
  pathToGoldenFile,
  pathToOutFile,
  queryIndexerOnSnapshot,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

-- | TODO: docstring with sql query?
mkSpentInfoEventAtQueryTest :: String -> DbSyncComparisonConfig -> TestTree
mkSpentInfoEventAtQueryTest testName cfg@(DbSyncComparisonConfig nodeType era slotNo dbPath _ _) =
  goldenVsFileDiff
    testName
    (\expected actual -> ["diff", "--color=always", expected, actual])
    goldenFile
    outFile
    runTest
  where
    runTest = do
      indexer <-
        either throwIO pure
          =<< runExceptT (Spent.mkSpentIndexer SQLiteIndexer.inMemoryDB)
      queryResult :: Maybe Spent.SpentInfoEvent <-
        queryIndexerOnSnapshot
          -- TODO: PLT-6165 confirm this shouldn't be hardcoded to mainnet
          nodeType
          subChainPath
          dbPath
          spentInfoIndexerConfig
          (Just $ chainPointAtSlotNo slotNo)
          Core.EventAtQuery
          indexer
      Aeson.encodeFile outFile queryResult

    outFile = pathToOutFile cfg
    goldenFile = pathToGoldenFile cfg
    -- TODO: PLT-6165
    subChainPath = "../../mainnet-snapshots" </> eraToString era

-- | Config used for testing SpentInfo indexer on snapshot with no rollback.
spentInfoIndexerConfig :: Runner.RunIndexerOnSnapshotConfig BlockEvent Spent.SpentInfoEvent
spentInfoIndexerConfig = Runner.RunIndexerOnSnapshotConfig preprocessor 1
  where
    preprocessor = Runner.RunIndexerEventPreprocessing extractSpent (const Nothing) (const Nothing)
    mkIndexedTimedSpent
      :: C.ChainPoint -> C.TxBody era -> Core.ProcessedInput C.ChainPoint Spent.SpentInfoEvent
    mkIndexedTimedSpent point = Core.Index . Core.Timed point . nonEmpty . Spent.getInputs
    extractSpent :: BlockEvent -> [Core.ProcessedInput C.ChainPoint Spent.SpentInfoEvent]
    extractSpent (BlockEvent (C.BlockInMode (C.Block (C.BlockHeader slotNo hash _) txs) _) _ _) =
      map (mkIndexedTimedSpent (C.ChainPoint slotNo hash) . C.getTxBody) txs

-- TODO: PLT-6165 consider undefined in place of empty hash here

{- | Create a 'ChainPoint' from a 'SlotNo' with arbitrary hash.
Used solely for testing here, where the hash information of chain point is
known not to be used.

NOTE: This is needed since the only way to query SpentInfo at a given slot number, as of
writing, is to use the 'ChainPoint' provided to 'query' with 'EventAtQuery'.
-}
chainPointAtSlotNo :: C.SlotNo -> C.ChainPoint
chainPointAtSlotNo = flip C.ChainPoint (C.HeaderHash BSS.empty)
