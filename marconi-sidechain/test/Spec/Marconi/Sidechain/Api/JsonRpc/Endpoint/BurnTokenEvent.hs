{-# LANGUAGE NumericUnderscores #-}

{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Routes.
-}
module Spec.Marconi.Sidechain.Api.JsonRpc.Endpoint.BurnTokenEvent where

import Cardano.Api qualified as C
import Control.Lens ((^.), (^..))
import Control.Lens qualified as Lens
import Control.Monad (unless)
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Core.Extract.WithDistance qualified as WithDistance
import Marconi.Cardano.Indexers.MintTokenEvent (mintTokenEventAsset)
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Core qualified as Core
import Marconi.Sidechain.Api.JsonRpc.Endpoint.BurnTokenEvent qualified as Sidechain
import Marconi.Sidechain.CLI qualified as CLI
import Spec.Marconi.Sidechain.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
import Test.Gen.Marconi.Cardano.Indexers.MintTokenEvent qualified as Test.MintTokenEvent
import Test.Helpers qualified
import Test.Marconi.Cardano.ChainIndex.Api.HttpServer qualified as Test.HttpServer
import Test.Marconi.Cardano.ChainIndex.Api.JsonRpc qualified as Test.JsonRpc
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (
  testPropertyNamed,
 )

tests :: TestTree
tests =
  testGroup
    "Marconi.Sidechain.Api.JsonRpc.Endpoint.BurnTokenEvent"
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
    hasNoBurnEvent = null burnEvents

  Hedgehog.cover 10 "At least one burn event" (not hasNoBurnEvent)

  unless hasNoBurnEvent $ do
    policy <- Hedgehog.forAll $ getPolicyId <$> Hedgehog.Gen.element burnEvents
    configPath <- Hedgehog.evalIO Utils.getNodeConfigPath

    let burnEventsWithPolicy = filter ((== policy) . getPolicyId) burnEvents
        args =
          Utils.initTestingCliArgs
            { CLI.dbDir = tmp
            , CLI.nodeConfigPath = configPath
            }
        params = Sidechain.GetBurnTokenEventsParams policy Nothing Nothing Nothing

    -- Index then query via JSON-RPC handler
    actual <-
      Hedgehog.evalIO $
        Test.HttpServer.queryHandlerWithIndexers
          events
          (Utils.mkTestSidechainConfigsFromCliArgs args)
          (Sidechain.getBurnTokenEventsHandler params)

    -- Compare actual vs. expected after conversion to common format.
    -- Equality checked on most fields of result.
    uncurry (===) $
      Test.JsonRpc.uniformGetBurnTokenEventsResult actual burnEventsWithPolicy
