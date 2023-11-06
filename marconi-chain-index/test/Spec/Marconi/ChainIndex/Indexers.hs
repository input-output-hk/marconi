module Spec.Marconi.ChainIndex.Indexers (tests) where

import Test.Tasty (TestTree, localOption, testGroup)

import Spec.Marconi.ChainIndex.Indexers.BlockInfo qualified as BlockInfo
import Spec.Marconi.ChainIndex.Indexers.ChainTip qualified as ChainTip
import Spec.Marconi.ChainIndex.Indexers.Datum qualified as Datum
import Spec.Marconi.ChainIndex.Indexers.MintTokenEvent qualified as MintTokenEvent
import Spec.Marconi.ChainIndex.Indexers.Spent qualified as Spent
import Spec.Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Spec.Marconi.ChainIndex.Indexers.UtxoQuery qualified as UtxoQuery
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Indexer"
    [ unitTests
    , propTests
    ]

-- | Genuine property tests, with an arbitrary number of test runs.
propTests :: TestTree
propTests =
  localOption (HedgehogTestLimit $ Just 200) $
    testGroup
      "Spec.Marconi.ChainIndex.Indexer.propTests"
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
    "Spec.Marconi.ChainIndex.Indexer.unitTests"
    [ BlockInfo.unitTests
    , MintTokenEvent.unitTests
    ]
