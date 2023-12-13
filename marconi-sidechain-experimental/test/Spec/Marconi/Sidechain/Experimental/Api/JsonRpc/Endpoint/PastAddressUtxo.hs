{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Cardano.Api qualified as C
import Control.Lens ((^.), (^..))
import Control.Lens qualified as Lens
import Control.Monad (when)
import Data.List (sortOn)
import Data.Text qualified as Text
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types qualified as ChainIndex
import Marconi.Cardano.Core.Extract.WithDistance (getEvent)
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
import Test.Gen.Marconi.Cardano.Core.Types qualified as Test.Types
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo
import Test.Helpers qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (
  testPropertyNamed,
 )

tests :: TestTree
tests =
  testGroup
    "Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo"
    [ testPropertyNamed
        "Handler returns Utxos from queried address"
        "propQueryTargetAddresses"
        propQueryTargetAddresses
    ]

{- | Property test for 'getUtxosFromAddress' query. Wraps propQueryTargetAddressesWithMockchain so
that each test runs in a separate directory that can be cleaned between runs.

NOTE: There is an issue with too many open files if the standard number of tests is run. That is
possibly related to some other tests using 'workspace' and can be addressed later.
-}
propQueryTargetAddresses :: Hedgehog.Property
propQueryTargetAddresses =
  Hedgehog.withTests 10 $
    Hedgehog.withShrinks 1 $
      Hedgehog.property $
        Hedgehog.forAll Test.Mockchain.genShelleyMockchainWithInfoAndDistance
          >>= propQueryTargetAddressesWithMockchain

{- | Create a property test for getUtxosFromAddress endpoint using the provided
mockchain as input.
-}
propQueryTargetAddressesWithMockchain
  :: Test.Mockchain.MockchainWithInfoAndDistance C.BabbageEra -> Hedgehog.PropertyT IO ()
propQueryTargetAddressesWithMockchain events = Test.Helpers.workspace "." $ \tmp -> do
  let
    -- Events are sorted increasing in time to ensure the last utxo in the list
    -- is unspent, since it is used for the test query.
    utxoEvents =
      sortOn (^. Core.point) $
        Test.Utxo.getTimedUtxosEventsWithDistance $
          Test.Mockchain.mockchainWithInfoAsMockchainWithDistance events
    --  Extract the UtxoEvents to be used in 'uniformGetUtxosFromAddressResult',
    --  which will filter expected to the address slected for the test query.
    expected = utxoEvents ^.. Lens.folded . Core.event . Lens.to getEvent . Lens.folded
    hasUtxoEvents = not $ null expected

  Hedgehog.cover 80 "At least one UtxoEvent" hasUtxoEvents

  when hasUtxoEvents $ do
    -- Select a single address from the sampled utxos.
    -- An address from the last event is selected so that we know it is unspent.
    let
      e = last utxoEvents

    addr <- Hedgehog.forAll $ Hedgehog.Gen.element (Test.Utxo.addressesFromTimedUtxoEvent e)

    -- Make config for this test

    configPath <- Hedgehog.evalIO Utils.getNodeConfigPath

    let
      args =
        Utils.initTestingCliArgs
          { CLI.targetAddresses = Test.Types.addressAnysToTargetAddresses [addr]
          , -- Use tmp directory instead of "" since LastEventIndexer writes latestStable.cbor
            CLI.dbDir = tmp
          , CLI.nodeConfigPath = configPath
          }
      -- Serialise to Bech32-format string as required by handler
      addrString = Text.unpack $ C.serialiseAddress addr

    let
      params = ChainIndex.GetUtxosFromAddressParams addrString Nothing Nothing

    -- Index then query via JSON-RPC handler
    actual <-
      Hedgehog.evalIO $
        Utils.queryHandlerWithIndexers
          events
          (Utils.mkTestSidechainConfigsFromCliArgs args)
          (Sidechain.getPastAddressUtxoHandler params)

    -- Extract the utxos associated with the selected address from 'expected'
    -- and compare actual vs. expected by sorted (TxIn, Value) pairs.
    uncurry (===) $
      Utils.uniformGetUtxosFromAddressResult addr actual expected
