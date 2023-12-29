module Spec.Marconi.Cardano.DbSyncComparison where

import Test.Marconi.Cardano.DbSyncComparison.BlockInfoResult (mkBlockInfoQueryBySlotNoTest)
import Test.Marconi.Cardano.DbSyncComparison.Common (Era (Byron1), NodeType (Mainnet))
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.DbSyncComparison"
    [ blockInfoTests
    ]

blockInfoTests :: TestTree
blockInfoTests =
  testGroup
    "BlockInfo tests"
    [ mkBlockInfoQueryBySlotNoTest "TTTT" Mainnet Byron1 6
    , mkBlockInfoQueryBySlotNoTest "TTTT" Mainnet Byron1 206938
    ]
