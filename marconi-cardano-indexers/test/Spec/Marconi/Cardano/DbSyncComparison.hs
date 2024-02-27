module Spec.Marconi.Cardano.DbSyncComparison where

import Cardano.Api qualified as C
import Test.Marconi.Cardano.DbSyncComparison.BlockInfoResult (mkBlockInfoQueryBySlotNoTest)
import Test.Marconi.Cardano.DbSyncComparison.Common (
  DbSyncComparisonConfig (DbSyncComparisonConfig),
  Era (Allegra, Alonzo1, Alonzo2, Babbage1, Babbage2, Byron1, Byron2, Mary, Shelley),
  NodeType (Mainnet),
 )
import Test.Marconi.Cardano.DbSyncComparison.SpentInfoResult (mkSpentInfoEventAtQueryTest)
import Test.Marconi.Cardano.DbSyncComparison.UtxoResult (mkUtxoEventTest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.DbSyncComparison"
    [ ignoreTest blockInfoTests
    , ignoreTest spentInfoTests
    , utxoTests
    ]

blockInfoTests :: TestTree
blockInfoTests =
  testGroup
    "BlockInfo tests"
    [ mkBlockInfoQueryBySlotNoTest "At slot number 0" $ blockInfoConfig Mainnet Byron1 0
    , mkBlockInfoQueryBySlotNoTest "At slot number 6" $ blockInfoConfig Mainnet Byron1 6
    , mkBlockInfoQueryBySlotNoTest "At slot number 206938" $ blockInfoConfig Mainnet Byron1 206938
    , mkBlockInfoQueryBySlotNoTest "At slot number 3801600" $ blockInfoConfig Mainnet Byron2 3801600
    , mkBlockInfoQueryBySlotNoTest "At slot number 3901600" $ blockInfoConfig Mainnet Byron2 3901600
    , mkBlockInfoQueryBySlotNoTest "At slot number 4039199" $ blockInfoConfig Mainnet Byron2 4039199
    , mkBlockInfoQueryBySlotNoTest "At slot number 4492800" $ blockInfoConfig Mainnet Shelley 4492800
    , mkBlockInfoQueryBySlotNoTest "At slot number 4520900" $ blockInfoConfig Mainnet Shelley 4520900
    , mkBlockInfoQueryBySlotNoTest "At slot number 9244771" $ blockInfoConfig Mainnet Shelley 9244771
    , mkBlockInfoQueryBySlotNoTest "At slot number 16588800" $ blockInfoConfig Mainnet Allegra 16588800
    , mkBlockInfoQueryBySlotNoTest "At slot number 18698800" $ blockInfoConfig Mainnet Allegra 18698800
    , mkBlockInfoQueryBySlotNoTest "At slot number 21340776" $ blockInfoConfig Mainnet Allegra 21340776
    , mkBlockInfoQueryBySlotNoTest "At slot number 23068800" $ blockInfoConfig Mainnet Mary 23068800
    , mkBlockInfoQueryBySlotNoTest "At slot number 25168800" $ blockInfoConfig Mainnet Mary 25168800
    , mkBlockInfoQueryBySlotNoTest "At slot number 27820794" $ blockInfoConfig Mainnet Mary 27820794
    , mkBlockInfoQueryBySlotNoTest "At slot number 39916975" $ blockInfoConfig Mainnet Alonzo1 39916975
    , mkBlockInfoQueryBySlotNoTest "At slot number 43372584" $ blockInfoConfig Mainnet Alonzo1 43372584
    , mkBlockInfoQueryBySlotNoTest "At slot number 43372792" $ blockInfoConfig Mainnet Alonzo1 43372792
    , mkBlockInfoQueryBySlotNoTest "At slot number 43372972" $ blockInfoConfig Mainnet Alonzo2 43372972
    , mkBlockInfoQueryBySlotNoTest "At slot number 45748924" $ blockInfoConfig Mainnet Alonzo2 45748924
    , mkBlockInfoQueryBySlotNoTest "At slot number 48124747" $ blockInfoConfig Mainnet Alonzo2 48124747
    , mkBlockInfoQueryBySlotNoTest "At slot number 72316896" $ blockInfoConfig Mainnet Babbage1 72316896
    , mkBlockInfoQueryBySlotNoTest "At slot number 77064584" $ blockInfoConfig Mainnet Babbage1 77064584
    , mkBlockInfoQueryBySlotNoTest "At slot number 77068768" $ blockInfoConfig Mainnet Babbage1 77068768
    , mkBlockInfoQueryBySlotNoTest "At slot number 84844885" $ blockInfoConfig Mainnet Babbage2 84844885
    , mkBlockInfoQueryBySlotNoTest "At slot number 87198551" $ blockInfoConfig Mainnet Babbage2 87198551
    , mkBlockInfoQueryBySlotNoTest "At slot number 89596758" $ blockInfoConfig Mainnet Babbage2 89596758
    ]

spentInfoTests :: TestTree
spentInfoTests =
  testGroup
    "SpentInfo tests"
    [ mkSpentInfoEventAtQueryTest "At slot number 3801600" $ spentInfoConfig Mainnet Byron2 3801600
    , mkSpentInfoEventAtQueryTest "At slot number 3901600" $ spentInfoConfig Mainnet Byron2 3901600
    , -- NOTE: This one should return nothing.
      mkSpentInfoEventAtQueryTest "At slot number 4492800" $ spentInfoConfig Mainnet Shelley 4492800
    , mkSpentInfoEventAtQueryTest "At slot number 4520900" $ spentInfoConfig Mainnet Shelley 4520900
    , mkSpentInfoEventAtQueryTest "At slot number 16588800" $ blockInfoConfig Mainnet Allegra 16588800
    , mkSpentInfoEventAtQueryTest "At slot number 18728839" $ spentInfoConfig Mainnet Allegra 18728839
    , mkSpentInfoEventAtQueryTest "At slot number 23068800" $ spentInfoConfig Mainnet Mary 23068800
    , mkSpentInfoEventAtQueryTest "At slot number 39916975" $ blockInfoConfig Mainnet Alonzo1 39916975
    , mkSpentInfoEventAtQueryTest "At slot number 43372972" $ blockInfoConfig Mainnet Alonzo2 43372972
    , -- NOTE: This slot is known to have a block with at least one transaction whose script is
      -- invalid. It's here to have at least one test of the logic indexing collateral as spent
      -- rather than the usual transaction TxIns.
      mkSpentInfoEventAtQueryTest "At slot number 59438205" $ spentInfoConfig Mainnet Alonzo2 59438205
    , mkSpentInfoEventAtQueryTest "At slot number 72316896" $ spentInfoConfig Mainnet Babbage1 72316896
    ]

utxoTests :: TestTree
utxoTests =
  testGroup
    "Utxo tests"
    [ mkUtxoEventTest "TESTING" $ utxoEventConfig Mainnet Shelley 4520900
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

utxoEventConfig :: NodeType -> Era -> C.SlotNo -> DbSyncComparisonConfig
utxoEventConfig nodeType era slotNo =
  DbSyncComparisonConfig
    nodeType
    era
    slotNo
    (goldenDir <> "/utxo-db/")
    goldenDir
    "utxo"
