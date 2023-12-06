module Spec.Marconi.Cardano.Indexers (tests) where

import Test.Tasty (TestTree, localOption, testGroup)

import Spec.Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Spec.Marconi.Cardano.Indexers.ChainTip qualified as ChainTip
import Spec.Marconi.Cardano.Indexers.Datum qualified as Datum
import Spec.Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Spec.Marconi.Cardano.Indexers.Spent qualified as Spent
import Spec.Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Spec.Marconi.Cardano.Indexers.UtxoQuery qualified as UtxoQuery
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Indexers"
    [ unitTests
    , propTests
    ]

-- | Genuine property tests, with an arbitrary number of test runs.
propTests :: TestTree
propTests =
  localOption (HedgehogTestLimit $ Just 200) $
    testGroup
      "Spec.Marconi.Cardano.Indexers.propTests"
      [ Utxo.tests
      , Spent.tests
      , Datum.tests
      , BlockInfo.propTests
      , ChainTip.tests
      , UtxoQuery.tests
      , MintTokenEvent.propTests
      ]

-- | Tests whose number of runs is set in the test definition, e.g. to 1.
unitTests :: TestTree
unitTests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.unitTests"
    [ BlockInfo.unitTests
    , MintTokenEvent.unitTests
    ]
