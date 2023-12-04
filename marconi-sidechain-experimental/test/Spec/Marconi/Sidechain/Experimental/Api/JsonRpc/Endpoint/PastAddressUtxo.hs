{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Control.Concurrent (readMVar)
import Control.Lens ((^.))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Indexers qualified as Indexers
import Marconi.ChainIndex.Api.Types qualified as ChainIndex
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.Types qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Marconi.Sidechain.Experimental.Env qualified as Sidechain
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Indexers qualified as Test.Indexers
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo"
    []

-- TODO: PLT-8634 change test name to be more descriptive
propQueryTargetAddresses :: Hedgehog.Property
propQueryTargetAddresses = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.Utxo.genTimedUtxosEvents

  -- Subset of the unique addresses generated to query
  targetAddresses <-
    Hedgehog.forAll $
      Utils.addressAnysToTargetAddresses . Utils.addressesFromTimedUtxoEvents
        <$> Hedgehog.Gen.subsequence events

  let args = Utils.initTestingCliArgs{CLI.targetAddresses = targetAddresses}

  -- Make the http and build indexers configs (with indexers exposed)
  -- just as you would with the sidechain app.
  (httpConfig, indexersConfig) <- Utils.mkTestSidechainConfigsFromCliArgs args

  -- Index the utxo events directly
  indexer <-
    Hedgehog.evalIO $
      readMVar $
        indexersConfig
          ^. Test.Indexers.testBuildIndexersResultUtxo

  -- TODO: PLT-8634 sample addresses, then populate cli args, then create indexer
  -- and index events, then query

  undefined
