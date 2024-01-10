module Spec.Marconi.Cardano.DbSyncComparison where

import Test.Marconi.Cardano.DbSyncComparison.BlockInfoResult (mkBlockInfoQueryBySlotNoTest)
import Test.Marconi.Cardano.DbSyncComparison.Common (
  Era (Allegra, Alonzo1, Alonzo2, Babbage1, Babbage2, Byron1, Byron2, Mary, Shelley),
  NodeType (Mainnet),
 )
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
    [ mkBlockInfoQueryBySlotNoTest "At slot number 0" Mainnet Byron1 0
    , mkBlockInfoQueryBySlotNoTest "At slot number 6" Mainnet Byron1 6
    , mkBlockInfoQueryBySlotNoTest "At slot number 206938" Mainnet Byron1 206938
    , mkBlockInfoQueryBySlotNoTest "At slot number 3801600" Mainnet Byron2 3801600
    , mkBlockInfoQueryBySlotNoTest "At slot number 3901600" Mainnet Byron2 3901600
    , mkBlockInfoQueryBySlotNoTest "At slot number 4039199" Mainnet Byron2 4039199
    , mkBlockInfoQueryBySlotNoTest "At slot number 4492800" Mainnet Shelley 4492800
    , mkBlockInfoQueryBySlotNoTest "At slot number 4520900" Mainnet Shelley 4520900
    , mkBlockInfoQueryBySlotNoTest "At slot number 9244771" Mainnet Shelley 9244771
    , mkBlockInfoQueryBySlotNoTest "At slot number 16588800" Mainnet Allegra 16588800
    , mkBlockInfoQueryBySlotNoTest "At slot number 18698800" Mainnet Allegra 18698800
    , mkBlockInfoQueryBySlotNoTest "At slot number 21340776" Mainnet Allegra 21340776
    , mkBlockInfoQueryBySlotNoTest "At slot number 23068800" Mainnet Mary 23068800
    , mkBlockInfoQueryBySlotNoTest "At slot number 25168800" Mainnet Mary 25168800
    , mkBlockInfoQueryBySlotNoTest "At slot number 27820794" Mainnet Mary 27820794
    , mkBlockInfoQueryBySlotNoTest "At slot number 39916975" Mainnet Alonzo1 39916975
    , mkBlockInfoQueryBySlotNoTest "At slot number 43372584" Mainnet Alonzo1 43372584
    , mkBlockInfoQueryBySlotNoTest "At slot number 43372792" Mainnet Alonzo1 43372792
    , mkBlockInfoQueryBySlotNoTest "At slot number 43372972" Mainnet Alonzo2 43372972
    , mkBlockInfoQueryBySlotNoTest "At slot number 45748924" Mainnet Alonzo2 45748924
    , mkBlockInfoQueryBySlotNoTest "At slot number 48124747" Mainnet Alonzo2 48124747
    , mkBlockInfoQueryBySlotNoTest "At slot number 72316896" Mainnet Babbage1 72316896
    , mkBlockInfoQueryBySlotNoTest "At slot number 77064584" Mainnet Babbage1 77064584
    , mkBlockInfoQueryBySlotNoTest "At slot number 77068768" Mainnet Babbage1 77068768
    , mkBlockInfoQueryBySlotNoTest "At slot number 84844885" Mainnet Babbage2 84844885
    , mkBlockInfoQueryBySlotNoTest "At slot number 87198551" Mainnet Babbage2 87198551
    , mkBlockInfoQueryBySlotNoTest "At slot number 89596758" Mainnet Babbage2 89596758
    ]

spentInfoTests :: TestTree
spentInfoTests =
  testGroup
    "SpentInfo tests"
    []
