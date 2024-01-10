{- | TODO: Move this to the right place once ana's work is merged
then add to cabal file.
-}
module Test.Marconi.Cardano.DbSyncComparison.SpentInfoResult where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Indexers.Spent qualified as Spent
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer qualified as SQLiteIndexer
import System.FilePath ((</>))
import Test.Marconi.Cardano.DbSyncComparison.Common (
  Era,
  NodeType (Mainnet),
  eraToString,
  nodeTypeToString,
  queryIndexerOnSnapshot,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

-- TODO: fill with types / utilities from import Test.Marconi.Cardano.DbSyncComparison.Common
pathToOutFile = undefined
pathToGoldenFile = undefined

-- | TODO: docstring with sql query?
mkSpentInfoEventAtQueryTest :: String -> NodeType -> Era -> C.SlotNo -> TestTree
mkSpentInfoEventAtQueryTest testName nodeType era slotNo =
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
      queryResult <-
        queryIndexerOnSnapshot Mainnet subChainPath dbPath spentInfoConfig undefined indexer
      Aeson.encodeFile outFile queryResult

    outFile = pathToOutFile nodeType era slotNo
    goldenFile = pathToGoldenFile nodeType era slotNo
    dbPath = "test/Spec/Golden/Snapshot/block-info-db/"
    subChainPath = "../../mainnet-snapshots" </> eraToString era

-- TODO: check security param: likely ok.
spentInfoConfig :: Runner.RunIndexerOnSnapshotConfig BlockEvent Spent.SpentInfo
spentInfoConfig = Runner.RunIndexerOnSnapshotConfig preprocessor 1
  where
    preprocessor = Runner.RunIndexerEventPreprocessing extractSpent extractBlockNo extractTip
    extractSpent :: BlockEvent -> [Core.ProcessedInput C.ChainPoint Spent.SpentInfo]
    extractSpent = undefined -- Spent.getInputs
    extractBlockNo = undefined
    extractTip = undefined
