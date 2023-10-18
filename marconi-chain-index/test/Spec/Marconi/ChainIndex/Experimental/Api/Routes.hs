{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Experimental.Api.Routes (tests) where

import Cardano.Api (
  PolicyId (PolicyId),
  getScriptData,
  hashScriptDataBytes,
 )
import Data.Aeson (eitherDecode, encode)
import Data.String (fromString)
import Gen.Marconi.ChainIndex.Types (
  genBlockNo,
  genHashBlockHeader,
  genQuantity,
  genSlotNo,
 )
import Hedgehog (
  Property,
  forAll,
  property,
  tripping,
 )
import Hedgehog.Gen (alphaNum, integral, list, maybe, string)
import Hedgehog.Range (linear)
import Marconi.Sidechain.Api.Routes (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsParams (GetBurnTokenEventsParams),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Test.Gen.Cardano.Api.Typed (
  genAssetName,
  genHashableScriptData,
  genScriptHash,
  genTxId,
 )
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
        <$> (PolicyId <$> genScriptHash)
        <*> (fmap fromString <$> Hedgehog.Gen.maybe (string (linear 1 10) alphaNum))
        <*> (Hedgehog.Gen.maybe $ integral (linear 1 10))
        <*> Hedgehog.Gen.maybe genTxId
  tripping r encode eitherDecode

propJSONRountripGetBurnTokenEventsResult :: Property
propJSONRountripGetBurnTokenEventsResult = property $ do
  r <- fmap GetBurnTokenEventsResult $ forAll $ list (linear 0 10) $ do
    hsd <- Hedgehog.Gen.maybe genHashableScriptData
    BurnTokenEventResult
      <$> genSlotNo
      <*> genHashBlockHeader
      <*> genBlockNo
      <*> genTxId
      <*> pure (fmap hashScriptDataBytes hsd)
      <*> pure (fmap getScriptData hsd)
      <*> genAssetName
      <*> genQuantity (linear 0 10)
      <*> pure True
  tripping r encode eitherDecode
