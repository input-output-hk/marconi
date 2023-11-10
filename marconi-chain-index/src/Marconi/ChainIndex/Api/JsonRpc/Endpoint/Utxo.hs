{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo (
  AddressUtxoResult (AddressUtxoResult),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
  RpcGetUtxosFromAddressMethod,
  SpentInfoResult (SpentInfoResult),
  UtxoTxInput (UtxoTxInput),
  getUtxosFromAddressQueryHandler,
) where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as C
import Control.Lens (view, (^.))
import Control.Monad (join, unless)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (bimap, first)
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configQueryables, configTrackedAddresses)
import Marconi.ChainIndex.Indexers (queryableUtxo)
import Marconi.ChainIndex.Indexers.BlockInfo qualified as BI
import Marconi.ChainIndex.Indexers.Utxo (datumHash, txIn, txIndex, value)
import Marconi.ChainIndex.Indexers.UtxoQuery (
  UtxoQueryEvent,
  UtxoQueryInput (UtxoQueryInput),
  UtxoResult (UtxoResult),
 )
import Marconi.ChainIndex.Types (TxIndexInBlock)
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler, hoistHttpHandler, queryErrToRpcErr)
import Marconi.Core.Type (Result)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, mkJsonRpcInternalErr, mkJsonRpcInvalidParamsErr)
import Servant.Server (Handler)

------------------
-- Method types --
------------------

type RpcGetUtxosFromAddressMethod =
  JsonRpc
    "getUtxosFromAddress"
    GetUtxosFromAddressParams
    String
    GetUtxosFromAddressResult

--------------
-- Handlers --
--------------

-- | Handler for retrieving UTXOs by Address
getUtxoQueryInputHandler
  :: UtxoQueryInput
  -- ^ Bech32 addressCredential and possibly a pair of slotNumbers
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) (Result UtxoQueryInput))
getUtxoQueryInputHandler query = do
  indexer <- view (configQueryables . queryableUtxo)
  let sqliteAggregateLastSyncPoint :: ExceptT (Core.QueryError UtxoQueryInput) Handler C.ChainPoint
      sqliteAggregateLastSyncPoint = do
        res <- liftIO $ runExceptT $ lift $ Core.lastSyncPoint indexer
        case res of
          Left err -> throwError err
          Right res' -> pure res'
  lastPointM <- hoistHttpHandler $ runExceptT sqliteAggregateLastSyncPoint
  case lastPointM of
    Left err ->
      pure $
        Left $
          mkJsonRpcInternalErr $
            Just $
              "Can't resolve last point in getUtxosFromAddress: " <> show err
    Right lastPoint ->
      hoistHttpHandler $
        liftIO $
          first queryErrToRpcErr <$> Core.queryEither lastPoint query indexer

-- | Return 'GetBurnTokenEventsResult' based on 'GetBurnTokenEventsParams'
getUtxosFromAddressQueryHandler
  :: GetUtxosFromAddressParams
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) GetUtxosFromAddressResult)
getUtxosFromAddressQueryHandler query = do
  addresses <- view configTrackedAddresses
  res <- join <$> traverse getUtxoQueryInputHandler (mapGetBurnTokenEventsQuery addresses query)
  pure $ mapGetBurnTokenEventsResult =<< res
  where
    mapGetBurnTokenEventsQuery
      :: [C.AddressAny] -> GetUtxosFromAddressParams -> Either (JsonRpcErr String) UtxoQueryInput
    mapGetBurnTokenEventsQuery
      targetAddresses
      (GetUtxosFromAddressParams addr createdAtAfter unspentBefore) = do
        mappedAddr <- addressMapping (pack addr)
        unless
          (mappedAddr `elem` targetAddresses)
          ( Left $
              mkJsonRpcInvalidParamsErr $
                Just "The 'address' param value must belong to the provided target addresses."
          )
        pure $ UtxoQueryInput mappedAddr createdAtAfter unspentBefore
        where
          addressMapping addressText = do
            bimap
              ( const $
                  mkJsonRpcInvalidParamsErr $
                    Just "The 'address' param value must be in the Bech32 format."
              )
              C.toAddressAny
              $ C.deserialiseFromBech32 C.AsShelleyAddress addressText
    -- Map internal events to our 'getBurnTokenEventsHandler' response object
    mapGetBurnTokenEventsResult :: [UtxoResult] -> Either (JsonRpcErr String) GetUtxosFromAddressResult
    mapGetBurnTokenEventsResult = fmap GetUtxosFromAddressResult . traverse mapResult
    mapResult :: UtxoResult -> Either (JsonRpcErr String) AddressUtxoResult
    mapResult (UtxoResult _ _ (Core.Timed C.ChainPointAtGenesis _) _ _) =
      Left $ mkJsonRpcInvalidParamsErr $ Just "Block at genesis not supported"
    mapResult
      ( UtxoResult
          utxo
          datum
          blockInfo@(Core.Timed (C.ChainPoint blockSlotNo header) _)
          spentInfo
          inputs
        ) =
        let mapSpentInfo :: Core.Timed C.ChainPoint (BI.BlockInfo, C.TxId) -> Maybe SpentInfoResult
            mapSpentInfo (Core.Timed C.ChainPointAtGenesis _) = Nothing
            mapSpentInfo (Core.Timed (C.ChainPoint spentInfoSlotNo _) (_, txId)) =
              Just $ SpentInfoResult spentInfoSlotNo txId
         in Right $
              AddressUtxoResult
                blockSlotNo
                header
                (blockInfo ^. Core.event . BI.epochNo)
                (blockInfo ^. Core.event . BI.blockNo)
                (utxo ^. txIndex)
                (utxo ^. txIn)
                (utxo ^. datumHash)
                datum
                (utxo ^. value)
                (mapSpentInfo =<< spentInfo)
                (UtxoTxInput <$> inputs)

data GetUtxosFromAddressParams = GetUtxosFromAddressParams
  { address :: !String
  -- ^ Address to query for
  , createdAtOrAfterSlotNo :: !(Maybe C.SlotNo)
  -- ^ Lower slot in the window
  , unspentBeforeSlotNo :: !(Maybe C.SlotNo)
  -- ^ Upper slot in the window
  }
  deriving (Show, Eq, Generic, FromJSON)

newtype GetUtxosFromAddressResult = GetUtxosFromAddressResult
  {unAddressUtxosResult :: [AddressUtxoResult]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data AddressUtxoResult = AddressUtxoResult
  { utxoResultSlotNo :: !C.SlotNo
  , utxoResultBlockHeader :: !(C.Hash C.BlockHeader)
  , utxoResultEpochNo :: !C.EpochNo
  , utxoResultBlockNo :: !C.BlockNo
  , utxoResultTxIndex :: !TxIndexInBlock
  , utxoResultTxIn :: !C.TxIn
  , utxoResultDatumHash :: !(Maybe (C.Hash C.ScriptData))
  , utxoResultDatum :: !(Maybe C.ScriptData)
  , utxoResultValue :: !C.Value
  , utxoResultSpentInfo :: !(Maybe SpentInfoResult)
  , utxoResultInputs :: ![UtxoTxInput] -- List of inputs that were used in the transaction that created this UTxO.
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AddressUtxoResult where
  toJSON (AddressUtxoResult slotNo bhh en bn txIndexInBlock txIn' dath dat val spentBy txInputs) =
    let C.TxIn txId txIx = txIn'
     in Aeson.object
          [ "slotNo" .= slotNo
          , "blockHeaderHash" .= bhh
          , "epochNo" .= en
          , "blockNo" .= bn
          , "txIndexInBlock" .= txIndexInBlock
          , "txId" .= txId
          , "txIx" .= txIx
          , "datumHash" .= dath
          , "datum" .= dat
          , "value" .= ValueWrapper val
          , "spentBy" .= spentBy
          , "txInputs" .= txInputs
          ]

instance FromJSON AddressUtxoResult where
  parseJSON =
    let parseAddressUtxoResult v = do
          AddressUtxoResult
            <$> v
              .: "slotNo"
            <*> v
              .: "blockHeaderHash"
            <*> v
              .: "epochNo"
            <*> v
              .: "blockNo"
            <*> v
              .: "txIndexInBlock"
            <*> (C.TxIn <$> v .: "txId" <*> v .: "txIx")
            <*> v
              .: "datumHash"
            <*> v
              .: "datum"
            <*> (unValueWrapper <$> v .: "value")
            <*> v
              .: "spentBy"
            <*> v
              .: "txInputs"
     in Aeson.withObject "AddressUtxoResult" parseAddressUtxoResult

newtype UtxoTxInput = UtxoTxInput C.TxIn
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON UtxoTxInput where
  toJSON (UtxoTxInput (C.TxIn txId txIx)) =
    Aeson.object
      [ "txId" .= txId
      , "txIx" .= txIx
      ]

instance FromJSON UtxoTxInput where
  parseJSON =
    let parseUtxoTxInput v = do
          txIn' <- C.TxIn <$> v .: "txId" <*> v .: "txIx"
          pure $ UtxoTxInput txIn'
     in Aeson.withObject "UtxoTxInput" parseUtxoTxInput

data SpentInfoResult
  = SpentInfoResult
      !C.SlotNo
      !C.TxId
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON SpentInfoResult where
  toJSON (SpentInfoResult sn txid) = Aeson.object ["slotNo" .= sn, "txId" .= txid]

instance FromJSON SpentInfoResult where
  parseJSON =
    let parseSpent v = SpentInfoResult <$> v .: "slotNo" <*> v .: "txId"
     in Aeson.withObject "SpentInfoResult" parseSpent

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
