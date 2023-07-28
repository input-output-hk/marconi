module Spec.Marconi.ChainIndex.Experimental.Indexers (tests) where

import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Spec.Marconi.ChainIndex.Experimental.Indexers.BlockInfo qualified as BlockInfo
import Spec.Marconi.ChainIndex.Experimental.Indexers.Datum qualified as Datum
import Spec.Marconi.ChainIndex.Experimental.Indexers.Spent qualified as Spent
import Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Spec.Marconi.ChainIndex.Experimental.Indexers.UtxoQuery qualified as UtxoQuery

tests :: TestTree
tests =
  localOption (HedgehogTestLimit $ Just 200) $
    testGroup
      "Spec.Marconi.ChainIndex.Experimental.Indexer"
      [ Utxo.tests
      , Spent.tests
      , Datum.tests
      , BlockInfo.tests
      , UtxoQuery.tests
      ]
