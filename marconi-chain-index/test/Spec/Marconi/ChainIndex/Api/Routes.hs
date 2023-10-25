{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Api.Routes (tests) where

import Data.Aeson qualified as Aeson
import Hedgehog (
  Property,
  forAll,
  property,
  tripping,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Spec.Marconi.ChainIndex.Api.Gen (
  genBurnTokenEventResult,
  genGetBurnTokenEventsParams,
 )
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Api.Routes"
    [ testGroup
        "ToJSON/FromJSON rountrip"
        [ testPropertyNamed
            "GetBurnTokenEventsParams"
            "propJSONRountripGetBurnTokenEventsParams"
            propJSONRountripGetBurnTokenEventsParams
        , testPropertyNamed
            "GetBurnTokenEventsResult"
            "propJSONRountripGetBurnTokenEventsResult"
            propJSONRountripGetBurnTokenEventsResult
        ]
    ]

propJSONRountripGetBurnTokenEventsParams :: Property
propJSONRountripGetBurnTokenEventsParams = property $ do
  r <- forAll genGetBurnTokenEventsParams
  tripping r Aeson.encode Aeson.eitherDecode

propJSONRountripGetBurnTokenEventsResult :: Property
propJSONRountripGetBurnTokenEventsResult = property $ do
  r <- fmap GetBurnTokenEventsResult $ forAll $ Gen.list (Range.linear 0 10) $ do
    hsd <- Gen.maybe CGen.genHashableScriptData
    genBurnTokenEventResult hsd
  tripping r Aeson.encode Aeson.eitherDecode
