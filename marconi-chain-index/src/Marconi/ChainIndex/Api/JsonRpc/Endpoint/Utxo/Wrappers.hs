{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Wrappers (
  UtxoTxInput (UtxoTxInput),
  ValueWrapper (ValueWrapper),
) where

import Cardano.Api qualified as C
import Control.Monad (join)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, unpack)
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)

data UtxoTxInput = UtxoTxInput {txId :: C.TxId, txIx :: C.TxIx}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | Wrapper around Cardano.Api,Value to provide a custom JSON serialisation/deserialisation
newtype ValueWrapper = ValueWrapper {unValueWrapper :: C.Value}
  deriving stock (Eq, Show, Generic)

instance ToJSON ValueWrapper where
  toJSON =
    let nameValueToEntry (assetNameHex, qty) = assetNameHex .= qty
        policyValueToEntry xs@((policyIdHex, _) :| _) =
          policyIdHex .= Aeson.object (NonEmpty.toList $ nameValueToEntry . snd <$> xs)
        assetToKeys C.AdaAssetId = ("", "")
        assetToKeys (C.AssetId pid assetName') =
          let pidHex = Aeson.fromString $ unpack $ C.serialiseToRawBytesHexText pid
              assetNameHex = Aeson.fromString $ unpack $ C.serialiseToRawBytesHexText assetName'
           in (pidHex, assetNameHex)
     in Aeson.object
          . fmap policyValueToEntry
          . NonEmpty.groupBy ((==) `on` fst)
          . fmap (\((pId, an), qty) -> (pId, (an, qty)))
          . List.sortOn fst
          . fmap (first assetToKeys)
          . C.valueToList
          . unValueWrapper

instance FromJSON ValueWrapper where
  parseJSON =
    let parsePolicyId pIdHex = do
          either (const $ fail "Invalid policyId") pure $
            C.deserialiseFromRawBytesHex C.AsPolicyId (Text.encodeUtf8 pIdHex)
        parseAssetName assetNameHex = do
          either (const $ fail "Invalid assetName") pure $
            C.deserialiseFromRawBytesHex C.AsAssetName (Text.encodeUtf8 assetNameHex)

        parseAdaAsset :: Aeson.KeyMap Aeson.Value -> Aeson.Parser C.Quantity
        parseAdaAsset v
          | [("", q)] <- Aeson.toList v = parseJSON q
          | otherwise = fail "ambiguous Ada asset value"

        parseAssetItem :: Aeson.Key -> Aeson.Value -> Aeson.Parser (C.AssetName, C.Quantity)
        parseAssetItem k v = (,) <$> parseAssetName (Aeson.toText k) <*> parseJSON v

        parseAssetItems :: Aeson.KeyMap Aeson.Value -> Aeson.Parser [(C.AssetName, C.Quantity)]
        parseAssetItems = traverse (uncurry parseAssetItem) . Aeson.toList

        parseValueItem :: (Text, Aeson.Value) -> Aeson.Parser [(C.AssetId, C.Quantity)]
        parseValueItem ("", o) = pure . (C.AdaAssetId,) <$> Aeson.withObject "AdaAsset" parseAdaAsset o
        parseValueItem (k, o) = do
          policyId' <- parsePolicyId k
          namesAndQuantities <- Aeson.withObject "AdaAsset" parseAssetItems o
          pure $ first (C.AssetId policyId') <$> namesAndQuantities

        fromJSONSideChainValue :: Aeson.KeyMap Aeson.Value -> Aeson.Parser ValueWrapper
        fromJSONSideChainValue v =
          ValueWrapper . C.valueFromList . join
            <$> traverse parseValueItem (first Aeson.toText <$> Aeson.toList v)
     in Aeson.withObject "Value" fromJSONSideChainValue
