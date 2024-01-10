module Spec.Marconi.Cardano.DbSyncComparison where

import Cardano.Api qualified as C
import Test.Marconi.Cardano.DbSyncComparison.BlockInfoResult (mkBlockInfoQueryBySlotNoTest)
import Test.Marconi.Cardano.DbSyncComparison.Common (
  DbSyncComparisonConfig (DbSyncComparisonConfig),
  Era (Allegra, Alonzo1, Alonzo2, Babbage1, Babbage2, Byron1, Byron2, Mary, Shelley),
  NodeType (Mainnet),
 )
import Test.Marconi.Cardano.DbSyncComparison.SpentInfoResult (mkSpentInfoEventAtQueryTest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.DbSyncComparison"
    [ ignoreTest blockInfoTests
    , ignoreTest spentInfoTests
    ]

blockInfoTests :: TestTree
blockInfoTests =
  testGroup
    "BlockInfo tests"
    [ mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Byron1 0
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Byron1 6
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Byron1 206938
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Byron2 3801600
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Byron2 3901600
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Byron2 4039199
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Shelley 4492800
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Shelley 4520900
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Shelley 9244771
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Allegra 16588800
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Allegra 18698800
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Allegra 21340776
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Mary 23068800
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Mary 25168800
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Mary 27820794
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Alonzo1 39916975
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Alonzo1 43372584
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Alonzo1 43372792
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Alonzo2 43372972
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Alonzo2 45748924
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Alonzo2 48124747
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Babbage1 72316896
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Babbage1 77064584
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Babbage1 77068768
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Babbage2 84844885
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Babbage2 87198551
    , mkBlockInfoQueryBySlotNoTest $ blockInfoConfig Mainnet Babbage2 89596758
    ]

spentInfoTests :: TestTree
spentInfoTests =
  testGroup
    "SpentInfo tests"
    [ mkSpentInfoEventAtQueryTest $ spentInfoConfig Mainnet Allegra 18728839
    , mkSpentInfoEventAtQueryTest $ spentInfoConfig Mainnet Babbage1 72316896
    ]

{- Config creation -}

goldenDir :: FilePath
goldenDir = "test/Spec/Golden/Snapshot"

blockInfoConfig :: NodeType -> Era -> C.SlotNo -> DbSyncComparisonConfig
blockInfoConfig nodeType era slotNo =
  DbSyncComparisonConfig
    nodeType
    era
    slotNo
    (goldenDir <> "/block-info-db/")
    goldenDir
    "blockinfo"

spentInfoConfig :: NodeType -> Era -> C.SlotNo -> DbSyncComparisonConfig
spentInfoConfig nodeType era slotNo =
  DbSyncComparisonConfig
    nodeType
    era
    slotNo
    (goldenDir <> "/spent-info-db/")
    goldenDir
    "spentinfo"
