{-# LANGUAGE NumericUnderscores #-}

{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types qualified as ChainIndex
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
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

-- TODO: PLT-8634 come back to the open files issue.

{- | Property test for 'getUtxosFromAddress' query. Wraps propQueryTargetAddressesWithMockchain so
that each test runs in a separate directory that can be cleaned between runs.
-}
propQueryTargetAddresses :: Hedgehog.Property
propQueryTargetAddresses =
  Hedgehog.withTests 10 $
    Hedgehog.withShrinks 1 $
      Hedgehog.property $
        Hedgehog.forAll Test.Mockchain.genMockchainWithInfoAndDistance
          >>= propQueryTargetAddressesWithMockchain

{- | Create a property test for getUtxosFromAddress endpoint using the provided
mockchain as input.
-}
propQueryTargetAddressesWithMockchain
  :: Test.Mockchain.MockchainWithInfoAndDistance C.BabbageEra -> Hedgehog.PropertyT IO ()
propQueryTargetAddressesWithMockchain events = Test.Helpers.workspace "." $ \tmp -> do
  -- TODO: PLT-8634 generated events should be an input. wrapper to check that we have
  -- at least one event?

  -- TODO: PLT-8634 is this sufficient? need to get *all* utxos from this event associated with that address.
  -- in other words, can there be an address in a utxo in the final event that also appears in a
  -- utxo in earlier events that are not also spent?

  -- Select a single address from the sampled utxos.
  -- An address from the last event is selected so that we know it is unspent.
  let
    (e : _) =
      reverse $
        Test.Utxo.getTimedUtxosEventsWithDistance $
          Test.Mockchain.mockchainWithInfoAsMockchainWithDistance events

  addr <- Hedgehog.forAll $ Hedgehog.Gen.element (Utils.addressesFromTimedUtxoEvent e)

  let
    args =
      Utils.initTestingCliArgs
        { CLI.targetAddresses = Utils.addressAnysToTargetAddresses [addr]
        , -- Use tmp directory instead of "" since LastEventIndexer writes latestStable.cbor
          CLI.dbDir = tmp
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

  let
    expected = mapMaybe ((\(WithDistance _ x) -> x) . (^. Core.event)) [e]

  -- Extract the utxos associated with the selected address from 'expected'
  -- and compare actual vs. expected by sorted (TxIn, Value) pairs.
  uncurry (===) $
    Utils.uniformGetUtxosFromAddressResult addr actual expected
