{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.Sidechain.Api.Routes (
  module Marconi.Sidechain.Api.Routes,
  Utxo.interval,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Slotting.Slot (WithOrigin (At, Origin), withOriginFromMaybe)
import Control.Applicative ((<|>))
import Control.Lens ((.~), (?~))
import Control.Monad (join)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Function (on, (&))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi (
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiObject),
  ToSchema (declareNamedSchema),
  declareSchemaRef,
 )
import Data.OpenApi.Lens (properties, required, type_)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Legacy.Indexers.Utxo (BlockInfo (BlockInfo))
import Marconi.ChainIndex.Legacy.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Legacy.Orphans ()
import Marconi.ChainIndex.Legacy.Types (TxIndexInBlock)
import Marconi.Sidechain.Api.Orphans ()
import Marconi.Sidechain.CLI (CliArgs)
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)
import Servant.API (Get, JSON, PlainText, (:<|>), (:>))

-- | marconi-sidechain APIs
type API = JsonRpcAPI :<|> RestAPI

----------------------------------------------
--  RPC types
--  methodName -> parameter(s) -> return-type
----------------------------------------------

-- | JSON-RPC API, endpoint
type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | RPC routes
type RpcAPI =
  RpcEchoMethod
    :<|> RpcTargetAddressesMethod
    :<|> RpcCurrentSyncedBlockMethod
    :<|> RpcPastAddressUtxoMethod
    :<|> RpcGetBurnTokenEventsMethod
    :<|> RpcEpochActiveStakePoolDelegationMethod
    :<|> RpcEpochNonceMethod

type RpcEchoMethod = JsonRpc "echo" String String String

type RpcTargetAddressesMethod =
  JsonRpc
    "getTargetAddresses"
    UnusedRequestParams
    String
    [Text]

type RpcCurrentSyncedBlockMethod =
  JsonRpc
    "getCurrentSyncedBlock"
    UnusedRequestParams
    String
    GetCurrentSyncedBlockResult

type RpcPastAddressUtxoMethod =
  JsonRpc
    "getUtxosFromAddress"
    GetUtxosFromAddressParams
    String
    GetUtxosFromAddressResult

type RpcGetBurnTokenEventsMethod =
  JsonRpc
    "getBurnTokenEvents"
    GetBurnTokenEventsParams
    String
    GetBurnTokenEventsResult

type RpcEpochActiveStakePoolDelegationMethod =
  JsonRpc
    "getActiveStakePoolDelegationByEpoch"
    Word64
    String
    GetEpochActiveStakePoolDelegationResult

type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    Word64
    String
    GetEpochNonceResult

--------------------------
-- Query and Result types
--------------------------

newtype SidechainTip = SidechainTip {getTip :: C.ChainTip}
  deriving stock (Eq, Ord, Generic, Show)

instance ToJSON SidechainTip where
  toJSON (SidechainTip C.ChainTipAtGenesis) = Aeson.Null
  toJSON (SidechainTip (C.ChainTip sn bhh bn)) =
    Aeson.object
      [ "blockNo" .= bn
      , "blockHeaderHash" .= bhh
      , "slotNo" .= sn
      ]

instance FromJSON SidechainTip where
  parseJSON Aeson.Null = pure $ SidechainTip C.ChainTipAtGenesis
  parseJSON obj =
    let parseTip v = do
          slotNo <- v .: "slotNo"
          bhh <- v .: "blockHeaderHash"
          bn <- v .: "blockNo"
          pure $ SidechainTip $ C.ChainTip slotNo bhh bn
     in Aeson.withObject "ChainTip" parseTip obj

instance ToSchema SidechainTip where
  declareNamedSchema _ = do
    blockNoSchema <- declareSchemaRef $ Proxy @C.BlockNo
    blockHeaderSchema <- declareSchemaRef $ Proxy @(C.Hash C.BlockHeader)
    slotNoSchema <- declareSchemaRef $ Proxy @C.SlotNo
    return $
      NamedSchema (Just "SidechainTip") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("blockNo", blockNoSchema)
               , ("blockHeaderHash", blockHeaderSchema)
               , ("slotNo", slotNoSchema)
               ]
          & required
            .~ [ "blockNo"
               , "blockHeaderHash"
               , "slotNo"
               ]

data GetCurrentSyncedBlockResult
  = GetCurrentSyncedBlockResult (WithOrigin BlockInfo) SidechainTip
  deriving stock (Eq, Ord, Generic, Show)

instance ToJSON GetCurrentSyncedBlockResult where
  toJSON (GetCurrentSyncedBlockResult blockInfoM tip) =
    let nodeTip = case tip of
          SidechainTip C.ChainTipAtGenesis -> []
          tip' -> ["nodeTip" .= toJSON tip']
        chainPointObj =
          case blockInfoM of
            (At (BlockInfo sn bhh bn bt en)) ->
              [ "blockNo" .= bn
              , "blockTimestamp" .= bt
              , "blockHeaderHash" .= bhh
              , "slotNo" .= sn
              , "epochNo" .= en
              ]
                <> nodeTip
            Origin -> nodeTip
     in Aeson.object chainPointObj

instance FromJSON GetCurrentSyncedBlockResult where
  parseJSON =
    let parseBlock v = do
          slotNoM <- v .:? "slotNo"
          bhhM <- v .:? "blockHeaderHash"
          bnM <- v .:? "blockNo"
          blockTimestampM <- v .:? "blockTimestamp"
          epochNoM <- v .:? "epochNo"
          tip <- v .:? "nodeTip"
          let blockInfoM = withOriginFromMaybe $ BlockInfo <$> slotNoM <*> bhhM <*> bnM <*> blockTimestampM <*> epochNoM
          pure $ GetCurrentSyncedBlockResult blockInfoM (fromMaybe (SidechainTip C.ChainTipAtGenesis) tip)
     in Aeson.withObject "BlockResult" parseBlock

instance ToSchema GetCurrentSyncedBlockResult where
  declareNamedSchema _ = do
    blockNoSchema <- declareSchemaRef $ Proxy @C.BlockNo
    blockTimestampSchema <- declareSchemaRef $ Proxy @Word64
    blockHeaderSchema <- declareSchemaRef $ Proxy @(C.Hash C.BlockHeader)
    slotNoSchema <- declareSchemaRef $ Proxy @C.SlotNo
    epochNoSchema <- declareSchemaRef $ Proxy @C.EpochNo
    nodeTipSchema <- declareSchemaRef $ Proxy @SidechainTip
    return $
      NamedSchema (Just "GetCurrentSyncedBlockResult") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("blockNo", blockNoSchema)
               , ("blockTimestamp", blockTimestampSchema)
               , ("blockHeaderHash", blockHeaderSchema)
               , ("slotNo", slotNoSchema)
               , ("epochNo", epochNoSchema)
               , ("nodeTip", nodeTipSchema)
               ]
          & required
            .~ []

data GetUtxosFromAddressParams = GetUtxosFromAddressParams
  { queryAddress :: !String
  -- ^ address to query for
  , querySearchInterval :: !(Utxo.Interval Ledger.SlotNo)
  -- ^ Query interval
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetUtxosFromAddressParams where
  parseJSON =
    let buildInterval v = do
          lo <-
            v .:? "createdAtOrAfterSlotNo"
              <|> fail "The 'createAfterSlotNo' param value must be a natural number"
          hi <-
            v .:? "unspentBeforeSlotNo"
              <|> fail "The 'unspentBeforeSlotNo' param value must be a natural number"
          case Utxo.interval lo hi of
            Left _ -> fail "The 'unspentBeforeSlotNo' param value must be larger than 'createAfterSlotNo'."
            Right i -> pure i
        parseParams v = do
          address <- v .: "address" <|> fail "The 'address' param value must be in the Bech32 format"
          interval <- buildInterval v
          pure $ GetUtxosFromAddressParams address interval
     in Aeson.withObject "GetUtxosFromAddressParams" parseParams

instance ToJSON GetUtxosFromAddressParams where
  toJSON q =
    Aeson.object $
      catMaybes
        [ Just ("address" .= queryAddress q)
        , ("createdAtOrAfterSlotNo" .=) <$> Utxo.intervalLowerBound (querySearchInterval q)
        , ("unspentBeforeSlotNo" .=) <$> Utxo.intervalUpperBound (querySearchInterval q)
        ]

instance ToSchema GetUtxosFromAddressParams where
  declareNamedSchema _ = do
    addressSchema <- declareSchemaRef $ Proxy @String
    slotNoSchema <- declareSchemaRef $ Proxy @(Maybe C.SlotNo)
    return $
      NamedSchema (Just "GetUtxosFromAddressParams") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("address", addressSchema)
               , ("createdAtOrAfterSlotNo", slotNoSchema)
               , ("unspentBeforeSlotNo", slotNoSchema)
               ]
          & required
            .~ [ "address"
               ]

newtype GetUtxosFromAddressResult = GetUtxosFromAddressResult
  {unAddressUtxosResult :: [AddressUtxoResult]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

deriving newtype instance ToSchema GetUtxosFromAddressResult

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

instance ToSchema SpentInfoResult

-- | Wrapper around Cardano.Api,Value to provide a custom JSON serialisation/deserialisation
newtype SidechainValue = SidechainValue {getSidechainValue :: C.Value}
  deriving stock (Eq, Show, Generic)

instance ToJSON SidechainValue where
  toJSON =
    let nameValueToEntry (assetNameHex, qty) = assetNameHex .= qty
        policyValueToEntry xs@((policyIdHex, _) :| _) =
          policyIdHex .= Aeson.object (NonEmpty.toList $ nameValueToEntry . snd <$> xs)
        assetToKeys C.AdaAssetId = ("", "")
        assetToKeys (C.AssetId pid assetName') =
          let pidHex = Aeson.fromString $ Text.unpack $ C.serialiseToRawBytesHexText pid
              assetNameHex = Aeson.fromString $ Text.unpack $ C.serialiseToRawBytesHexText assetName'
           in (pidHex, assetNameHex)
     in Aeson.object
          . fmap policyValueToEntry
          . NonEmpty.groupBy ((==) `on` fst)
          . fmap (\((pId, an), qty) -> (pId, (an, qty)))
          . List.sortOn fst
          . fmap (first assetToKeys)
          . C.valueToList
          . getSidechainValue

instance FromJSON SidechainValue where
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

        fromJSONSideChainValue :: Aeson.KeyMap Aeson.Value -> Aeson.Parser SidechainValue
        fromJSONSideChainValue v =
          SidechainValue . C.valueFromList . join
            <$> traverse parseValueItem (first Aeson.toText <$> Aeson.toList v)
     in Aeson.withObject "Value" fromJSONSideChainValue

instance ToSchema SidechainValue

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
  toJSON (AddressUtxoResult slotNo bhh en bn txIndexInBlock txIn dath dat value spentBy txInputs) =
    let C.TxIn txId txIx = txIn
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
          , "value" .= SidechainValue value
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
            <*> (getSidechainValue <$> v .: "value")
            <*> v
              .: "spentBy"
            <*> v
              .: "txInputs"
     in Aeson.withObject "AddressUtxoResult" parseAddressUtxoResult

instance ToSchema AddressUtxoResult where
  declareNamedSchema _ = do
    slotNoSchema <- declareSchemaRef $ Proxy @C.SlotNo
    blockHeaderSchema <- declareSchemaRef $ Proxy @(C.Hash C.BlockHeader)
    blockNoSchema <- declareSchemaRef $ Proxy @C.BlockNo
    txIndexInBlockSchema <- declareSchemaRef $ Proxy @TxIndexInBlock
    txIdSchema <- declareSchemaRef $ Proxy @C.TxId
    txIxSchema <- declareSchemaRef $ Proxy @C.TxIx
    scriptHashSchema <- declareSchemaRef $ Proxy @(Maybe (C.Hash C.ScriptData))
    scriptDataSchema <- declareSchemaRef $ Proxy @(Maybe C.ScriptData)
    valueSchema <- declareSchemaRef $ Proxy @C.Value
    spentBySchema <- declareSchemaRef $ Proxy @(Maybe SpentInfoResult)
    txInputsSchema <- declareSchemaRef $ Proxy @[UtxoTxInput]
    return $
      NamedSchema (Just "AddressUtxoResult") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("slotNo", slotNoSchema)
               , ("blockHeaderHash", blockHeaderSchema)
               , ("blockNo", blockNoSchema)
               , ("txIndexInBlock", txIndexInBlockSchema)
               , ("txId", txIdSchema)
               , ("txIx", txIxSchema)
               , ("datumHash", scriptHashSchema)
               , ("datum", scriptDataSchema)
               , ("value", valueSchema)
               , ("spentBy", spentBySchema)
               , ("txInputs", txInputsSchema)
               ]
          & required
            .~ [ "slotNo"
               , "blockHeaderHash"
               , "blockNo"
               , "txIndexInBlock"
               , "txId"
               , "txIx"
               , "datumHash"
               , "datum"
               , "value"
               , "spentBy"
               , "txInputs"
               ]

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
          txIn <- C.TxIn <$> v .: "txId" <*> v .: "txIx"
          pure $ UtxoTxInput txIn
     in Aeson.withObject "UtxoTxInput" parseUtxoTxInput

instance ToSchema UtxoTxInput where
  declareNamedSchema _ = do
    txIdSchema <- declareSchemaRef $ Proxy @C.TxId
    txIxSchema <- declareSchemaRef $ Proxy @C.TxIx
    return $
      NamedSchema (Just "UtxoTxInput") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("txId", txIdSchema)
               , ("txIx", txIxSchema)
               ]
          & required
            .~ [ "txId"
               , "txIx"
               ]

data GetBurnTokenEventsParams = GetBurnTokenEventsParams
  { policyId :: !C.PolicyId
  , assetName :: !(Maybe C.AssetName)
  , beforeSlotNo :: !(Maybe Word64)
  , afterTx :: !(Maybe C.TxId)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GetBurnTokenEventsParams where
  parseJSON =
    let parseParams v =
          GetBurnTokenEventsParams
            <$> (v .: "policyId" <|> fail "The 'policyId' param value must be a valid minting policy hash.")
            <*> (v .:? "assetName")
            <*> (v .:? "createdBeforeSlotNo" <|> fail "The 'slotNo' param value must be a natural number.")
            <*> (v .:? "createdAfterTx" <|> fail "The 'afterTx' param value must be a valid transaction ID.")
     in Aeson.withObject "GetBurnTokenEventsParams" parseParams

instance ToJSON GetBurnTokenEventsParams where
  toJSON q =
    Aeson.object $
      catMaybes
        [ Just ("policyId" .= policyId q)
        , ("assetName" .=) <$> assetName q
        , ("createdBeforeSlotNo" .=) <$> beforeSlotNo q
        , ("createdAfterTx" .=) <$> afterTx q
        ]

instance ToSchema GetBurnTokenEventsParams where
  declareNamedSchema _ = do
    policyIdSchema <- declareSchemaRef $ Proxy @C.PolicyId
    assetNameSchema <- declareSchemaRef $ Proxy @(Maybe C.AssetName)
    beforeSlotNoSchema <- declareSchemaRef $ Proxy @(Maybe Word64)
    afterTxSchema <- declareSchemaRef $ Proxy @(Maybe C.TxId)
    return $
      NamedSchema (Just "GetBurnTokenEventsParams") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("policyId", policyIdSchema)
               , ("assetName", assetNameSchema)
               , ("createdBeforeSlotNo", beforeSlotNoSchema)
               , ("createdAfterTx", afterTxSchema)
               ]
          & required
            .~ [ "policyId"
               ]

newtype GetBurnTokenEventsResult
  = GetBurnTokenEventsResult [BurnTokenEventResult]
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

deriving newtype instance ToSchema GetBurnTokenEventsResult

-- | The quantity represents a burn amount only, so this is always a positive number.
data BurnTokenEventResult
  = BurnTokenEventResult
      !C.SlotNo
      !(C.Hash C.BlockHeader)
      !C.BlockNo
      !C.TxId
      !(Maybe (C.Hash C.ScriptData))
      !(Maybe C.ScriptData)
      !C.AssetName
      !C.Quantity
      !Bool
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON BurnTokenEventResult where
  toJSON (BurnTokenEventResult slotNo bhh bn txId redh red an qty isStable) =
    Aeson.object
      [ "slotNo" .= slotNo
      , "blockHeaderHash" .= bhh
      , "blockNo" .= bn
      , "txId" .= txId
      , "redeemerHash" .= redh
      , "redeemer" .= red
      , "assetName" .= an
      , "burnAmount" .= qty
      , "isStable" .= isStable
      ]

instance FromJSON BurnTokenEventResult where
  parseJSON =
    let parseAssetIdTxResult v =
          BurnTokenEventResult
            <$> v
              .: "slotNo"
            <*> v
              .: "blockHeaderHash"
            <*> v
              .: "blockNo"
            <*> v
              .: "txId"
            <*> v
              .: "redeemerHash"
            <*> v
              .: "redeemer"
            <*> v
              .: "assetName"
            <*> v
              .: "burnAmount"
            <*> v
              .: "isStable"
     in Aeson.withObject "AssetIdTxResult" parseAssetIdTxResult

instance ToSchema BurnTokenEventResult where
  declareNamedSchema _ = do
    slotNoSchema <- declareSchemaRef $ Proxy @C.SlotNo
    blockHeaderSchema <- declareSchemaRef $ Proxy @(C.Hash C.BlockHeader)
    blockNoSchema <- declareSchemaRef $ Proxy @C.BlockNo
    txIdSchema <- declareSchemaRef $ Proxy @C.TxId
    scriptHashSchema <- declareSchemaRef $ Proxy @(Maybe (C.Hash C.ScriptData))
    scriptDataSchema <- declareSchemaRef $ Proxy @(Maybe C.ScriptData)
    assetNameSchema <- declareSchemaRef $ Proxy @(Maybe C.AssetName)
    quantitySchema <- declareSchemaRef $ Proxy @C.Quantity
    boolSchema <- declareSchemaRef $ Proxy @Bool
    return $
      NamedSchema (Just "BurnTokenEventResult") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("slotNo", slotNoSchema)
               , ("blockHeaderHash", blockHeaderSchema)
               , ("blockNo", blockNoSchema)
               , ("txId", txIdSchema)
               , ("redeemerHash", scriptHashSchema)
               , ("redeemer", scriptDataSchema)
               , ("assetName", assetNameSchema)
               , ("burnAmount", quantitySchema)
               , ("isStable", boolSchema)
               ]
          & required
            .~ [ "slotNo"
               , "blockHeaderHash"
               , "blockNo"
               , "txId"
               , "redeemerHash"
               , "redeemer"
               , "assetName"
               , "burnAmount"
               , "isStable"
               ]

newtype GetEpochActiveStakePoolDelegationResult
  = GetEpochActiveStakePoolDelegationResult [ActiveSDDResult]
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

data ActiveSDDResult
  = ActiveSDDResult
      !C.PoolId
      !C.Lovelace
      !(Maybe C.SlotNo)
      !(Maybe (C.Hash C.BlockHeader))
      !C.BlockNo
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON ActiveSDDResult where
  parseJSON =
    let parseResult v = do
          ActiveSDDResult
            <$> v
              .: "poolId"
            <*> v
              .: "lovelace"
            <*> (fmap C.SlotNo <$> v .:? "slotNo")
            <*> v
              .:? "blockHeaderHash"
            <*> (C.BlockNo <$> v .: "blockNo")
     in Aeson.withObject "ActiveSDDResult" parseResult

instance ToJSON ActiveSDDResult where
  toJSON
    ( ActiveSDDResult
        poolId
        lovelace
        slotNo
        blockHeaderHash
        (C.BlockNo blockNo)
      ) =
      Aeson.object
        [ "poolId" .= poolId
        , "lovelace" .= lovelace
        , "slotNo" .= fmap C.unSlotNo slotNo
        , "blockHeaderHash" .= blockHeaderHash
        , "blockNo" .= blockNo
        ]

instance ToSchema ActiveSDDResult where
  declareNamedSchema _ = do
    poolIdSchema <- declareSchemaRef $ Proxy @C.PoolId
    lovelaceSchema <- declareSchemaRef $ Proxy @C.Lovelace
    slotNoSchema <- declareSchemaRef $ Proxy @C.SlotNo
    blockHeaderSchema <- declareSchemaRef $ Proxy @(C.Hash C.BlockHeader)
    blockNoSchema <- declareSchemaRef $ Proxy @C.BlockNo
    return $
      NamedSchema (Just "ActiveSDDResult") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("poolId", poolIdSchema)
               , ("lovelace", lovelaceSchema)
               , ("slotNo", slotNoSchema)
               , ("blockHeaderHash", blockHeaderSchema)
               , ("blockNo", blockNoSchema)
               ]
          & required
            .~ [ "poolId"
               , "lovelace"
               , "slotNo"
               , "blockHeaderHash"
               , "blockNo"
               ]

newtype GetEpochNonceResult
  = GetEpochNonceResult (Maybe NonceResult)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

data NonceResult
  = NonceResult
      !Ledger.Nonce
      !(Maybe C.SlotNo)
      !(Maybe (C.Hash C.BlockHeader))
      !C.BlockNo
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON NonceResult where
  parseJSON =
    let parseResult v = do
          NonceResult
            <$> (Ledger.Nonce <$> v .: "nonce")
            <*> (fmap C.SlotNo <$> v .:? "slotNo")
            <*> v
              .:? "blockHeaderHash"
            <*> (C.BlockNo <$> v .: "blockNo")
     in Aeson.withObject "NonceResult" parseResult

instance ToJSON NonceResult where
  toJSON
    ( NonceResult
        nonce
        slotNo
        blockHeaderHash
        (C.BlockNo blockNo)
      ) =
      let nonceValue = case nonce of
            Ledger.NeutralNonce -> Nothing
            Ledger.Nonce n -> Just n
       in Aeson.object
            [ "nonce" .= nonceValue
            , "slotNo" .= fmap C.unSlotNo slotNo
            , "blockHeaderHash" .= blockHeaderHash
            , "blockNo" .= blockNo
            ]

instance ToSchema NonceResult where
  declareNamedSchema _ = do
    nonceSchema <- declareSchemaRef $ Proxy @Ledger.Nonce
    slotNoSchema <- declareSchemaRef $ Proxy @C.SlotNo
    blockHeaderSchema <- declareSchemaRef $ Proxy @(C.Hash C.BlockHeader)
    blockNoSchema <- declareSchemaRef $ Proxy @C.BlockNo
    return $
      NamedSchema (Just "NonceResult") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("nonce", nonceSchema)
               , ("slotNo", slotNoSchema)
               , ("blockHeaderHash", blockHeaderSchema)
               , ("blockNo", blockNoSchema)
               ]
          & required
            .~ [ "nonce"
               , "slotNo"
               , "blockHeaderHash"
               , "blockNo"
               ]

------------------------
-- REST API endpoints --
------------------------

type RestAPI =
  GetTime
    :<|> GetParams
    :<|> GetTargetAddresses
    :<|> GetMetrics

type GetTime = "time" :> Get '[PlainText] String

type GetParams = "params" :> Get '[JSON] CliArgs

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

type GetMetrics = "metrics" :> Get '[PlainText] Text
