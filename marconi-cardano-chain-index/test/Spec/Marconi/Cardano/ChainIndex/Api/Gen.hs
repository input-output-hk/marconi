module Spec.Marconi.Cardano.ChainIndex.Api.Gen where

import Cardano.Api qualified as C
import Data.String (fromString)
import Hedgehog (
  Gen,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsParams (GetBurnTokenEventsParams),
 )
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Types qualified as Gen

genBurnTokenEventResult :: Maybe C.HashableScriptData -> Gen BurnTokenEventResult
genBurnTokenEventResult hsd =
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

genGetBurnTokenEventsParams :: Gen GetBurnTokenEventsParams
genGetBurnTokenEventsParams =
  GetBurnTokenEventsParams
    <$> (C.PolicyId <$> CGen.genScriptHash)
    <*> (fmap fromString <$> Gen.maybe (Gen.string (Range.linear 1 10) Gen.alphaNum))
    <*> (Gen.maybe $ C.SlotNo . fromInteger <$> Gen.integral (Range.linear 1 10))
    <*> Gen.maybe CGen.genTxId
