module Spec.Marconi.ChainIndex.Legacy.Indexers.AddressDatum (tests) where

import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Spec.Marconi.ChainIndex.Legacy.Indexers.AddressDatum.AddressDatumIndex qualified as AddressDatumIndex
import Spec.Marconi.ChainIndex.Legacy.Indexers.AddressDatum.AddressDatumIndexEvent qualified as AddressDatumIndexEvent

tests :: TestTree
tests =
  localOption (HedgehogTestLimit $ Just 200) $
    testGroup
      "Spec.Marconi.ChainIndex.Legacy.Indexers.AddressDatum"
      [ AddressDatumIndexEvent.tests
      , AddressDatumIndex.tests
      ]
