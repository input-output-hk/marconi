{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Experimental.Api.Routes (tests) where

import Cardano.Api qualified as C
import Data.Aeson qualified as Aeson
import Data.String (fromString)
import Gen.Marconi.ChainIndex.Types qualified as Gen
import Hedgehog (
  Property,
  forAll,
  property,
  tripping,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.MintBurnToken (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsParams (GetBurnTokenEventsParams),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Experimental.Api.Routes"
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
  r <-
    forAll $
      GetBurnTokenEventsParams
        <$> (C.PolicyId <$> CGen.genScriptHash)
        <*> (fmap fromString <$> Gen.maybe (Gen.string (Range.linear 1 10) Gen.alphaNum))
        <*> (Gen.maybe $ C.SlotNo . fromInteger <$> Gen.integral (Range.linear 1 10))
        <*> Gen.maybe CGen.genTxId
  tripping r Aeson.encode Aeson.eitherDecode

propJSONRountripGetBurnTokenEventsResult :: Property
propJSONRountripGetBurnTokenEventsResult = property $ do
  r <- fmap GetBurnTokenEventsResult $ forAll $ Gen.list (Range.linear 0 10) $ do
    hsd <- Gen.maybe CGen.genHashableScriptData
    BurnTokenEventResult
      <$> (Just <$> Gen.genSlotNo)
      <*> (Just <$> Gen.genHashBlockHeader)
      <*> Gen.genBlockNo
      <*> CGen.genTxId
      <*> pure (fmap C.hashScriptDataBytes hsd)
      <*> pure (fmap C.getScriptData hsd)
      <*> CGen.genAssetName
      <*> Gen.genQuantity (Range.linear 0 10)
      <*> pure True
  tripping r Aeson.encode Aeson.eitherDecode
