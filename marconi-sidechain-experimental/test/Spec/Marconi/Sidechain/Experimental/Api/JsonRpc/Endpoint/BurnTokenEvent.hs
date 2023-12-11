{-# LANGUAGE NumericUnderscores #-}

{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent where

import Cardano.Api qualified as C
import Control.Lens ((^.), (^..))
import Control.Lens qualified as Lens
import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Core.Extract.WithDistance qualified as WithDistance
import Marconi.Cardano.Indexers.MintTokenEvent (mintTokenEventAsset, mintTokenEventLocation)
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken qualified as ChainIndex
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
import Test.Gen.Marconi.Cardano.Indexers.MintTokenEvent qualified as Test.MintTokenEvent
import Test.Helpers qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (
  testPropertyNamed,
 )

tests :: TestTree
tests =
  testGroup
    "Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent"
    [ testPropertyNamed
        "Handler returns burn events correctly"
        "propQueryBurnTokenEvents"
        propQueryBurnTokenEvents
    ]

{- | Property test for 'getBurnTokenEventsHandler' query result. Wraps propQueryTargetAddressesWithMockchain so
that each test runs in a separate directory that can be cleaned between runs.
-}
propQueryBurnTokenEvents :: Hedgehog.Property
propQueryBurnTokenEvents =
  Hedgehog.withTests 50 $
    Hedgehog.withShrinks 1 $
      Hedgehog.property $
        Hedgehog.forAll Test.MintTokenEvent.genMockchainWithMintsWithInfoAndDistance
          >>= propQueryMintingPolicyWithMockchain

{- | Create a property test for getUtxosFromAddress endpoint using the provided
mockchain as input.
-}
propQueryMintingPolicyWithMockchain
  :: Test.Mockchain.MockchainWithInfoAndDistance C.BabbageEra -> Hedgehog.PropertyT IO ()
propQueryMintingPolicyWithMockchain events = Test.Helpers.workspace "." $ \tmp -> do
  let
    isBurn :: MintTokenEvent.MintTokenEvent -> Bool
    isBurn = (< 0) . (^. MintTokenEvent.mintTokenEventAsset . MintTokenEvent.mintAssetQuantity)
    getPolicyId = (^. mintTokenEventAsset . MintTokenEvent.mintAssetPolicyId)

    mintBurnEvents =
      Test.MintTokenEvent.getTimedMintTokentEventsWithDistance $
        Test.Mockchain.mockchainWithInfoAsMockchainWithDistance events
    burnEvents =
      mintBurnEvents
        ^.. Lens.folded . Core.event
        ^.. Lens.folded
          . Lens.to WithDistance.getEvent
          . Lens.folded
          . MintTokenEvent.mintTokenEvents
          . Lens.folded
          . Lens.filtered isBurn
    hasBurnEvents = not (null burnEvents)

  Hedgehog.cover 10 "At least one burn event" hasBurnEvents

  when hasBurnEvents $ do
    policy <- Hedgehog.forAll $ getPolicyId <$> Hedgehog.Gen.element burnEvents

    let
      burnEventsWithPolicy = filter ((== policy) . getPolicyId) burnEvents

    let
      args =
        Utils.initTestingCliArgs
          { CLI.dbDir = tmp
          }

    let
      params = Sidechain.GetBurnTokenEventsParams policy Nothing Nothing Nothing

    -- Index then query via JSON-RPC handler
    actual <-
      Hedgehog.evalIO $
        Utils.queryHandlerWithIndexers
          events
          (Utils.mkTestSidechainConfigsFromCliArgs args)
          (Sidechain.getBurnTokenEventsHandler params)

    -- Compare actual vs. expected after conversion to common format.
    -- Equality checked on most fields of result.
    uncurry (===) $
      Utils.uniformGetBurnTokenEventsResult actual burnEventsWithPolicy
