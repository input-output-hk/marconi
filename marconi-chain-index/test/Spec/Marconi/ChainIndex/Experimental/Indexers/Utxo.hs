module Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo (tests) where

import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo.UtxoIndex qualified as UtxoIndex

tests :: TestTree
tests = localOption (HedgehogTestLimit $ Just 200) $
    testGroup "Spec.Marconi.ChainIndex.Experimental.Indexer.Utxo"
    [ UtxoIndex.tests
    ]
