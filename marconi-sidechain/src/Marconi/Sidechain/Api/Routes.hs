{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.Sidechain.Api.Routes where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Slotting.Slot (WithOrigin (At, Origin), withOriginFromMaybe)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers.Utxo (BlockInfo (BlockInfo))
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (TxIndexInBlock)
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc)
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

type RpcTargetAddressesMethod = JsonRpc "getTargetAddresses" String String [Text]

type RpcCurrentSyncedBlockMethod =
  JsonRpc
    "getCurrentSyncedBlock"
    String
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

--------------------
-- REST related ---
--------------------

-- | REST API, endpoints
type RestAPI = "rest" :> (GetTime :<|> GetTargetAddresses)

type GetTime = "time" :> Get '[PlainText] String

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

--------------------------
-- Query and Result types
--------------------------

newtype GetCurrentSyncedBlockResult
  = GetCurrentSyncedBlockResult (WithOrigin BlockInfo)
  deriving (Eq, Ord, Generic, Show)

instance ToJSON GetCurrentSyncedBlockResult where
  toJSON (GetCurrentSyncedBlockResult blockInfoM) =
    let chainPointObj = case blockInfoM of
          (At (BlockInfo sn bhh bn bt en)) ->
            [ "blockNo" .= bn
            , "blockTimestamp" .= bt
            , "blockHeaderHash" .= bhh
            , "slotNo" .= sn
            , "epochNo" .= en
            ]
          Origin -> []
     in Aeson.object chainPointObj

instance FromJSON GetCurrentSyncedBlockResult where
  parseJSON =
    let parseBlock v = do
          slotNoM <- v .:? "slotNo"
          bhhM <- v .:? "blockHeaderHash"
          bnM <- v .:? "blockNo"
          blockTimestampM <- v .:? "blockTimestamp"
          epochNoM <- v .:? "epochNo"
          let blockInfoM = withOriginFromMaybe $ BlockInfo <$> slotNoM <*> bhhM <*> bnM <*> blockTimestampM <*> epochNoM
          pure $ GetCurrentSyncedBlockResult blockInfoM
     in Aeson.withObject "BlockResult" parseBlock

data GetUtxosFromAddressParams = GetUtxosFromAddressParams
  { queryAddress :: !String
  -- ^ address to query for
  , queryCreatedAfterSlotNo :: !(Maybe Word64)
  -- ^ query upper bound slotNo interval, unspent before or at this slot
  , queryUnspentBeforeSlotNo :: !Word64
  -- ^ query lower bound slotNo interval, filter out UTxO that were created during or before that slo
  }
  deriving (Show, Eq)

instance FromJSON GetUtxosFromAddressParams where
  parseJSON =
    let parseParams v =
          GetUtxosFromAddressParams
            <$> (v .: "address")
            <*> (v .:? "createdAfterSlotNo")
            <*> (v .: "unspentBeforeSlotNo")
     in Aeson.withObject "GetUtxosFromAddressParams" parseParams

instance ToJSON GetUtxosFromAddressParams where
  toJSON q =
    Aeson.object $
      catMaybes
        [ Just ("address" .= queryAddress q)
        , ("createdAfterSlotNo" .=) <$> queryCreatedAfterSlotNo q
        , Just $ "unspentBeforeSlotNo" .= queryUnspentBeforeSlotNo q
        ]

newtype GetUtxosFromAddressResult = GetUtxosFromAddressResult
  {unAddressUtxosResult :: [AddressUtxoResult]}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SpentInfoResult
  = SpentInfoResult
      !C.SlotNo
      !C.TxId
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SpentInfoResult where
  toJSON (SpentInfoResult sn txid) = Aeson.object ["slotNo" .= sn, "txId" .= txid]

instance FromJSON SpentInfoResult where
  parseJSON =
    let parseSpent v = SpentInfoResult <$> v .: "slotNo" <*> v .: "txId"
     in Aeson.withObject "SpentInfoResult" parseSpent

-- | Wrapper around Cardano.Api,Value to provide a custom JSON serialisation/deserialisation
newtype SidechainValue = SidechainValue {getSidechainValue :: C.Value}
  deriving (Eq, Show, Generic)

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

data AddressUtxoResult = AddressUtxoResult
  { utxoResultSlotNo :: !C.SlotNo
  , utxoResultBlockHeader :: !(C.Hash C.BlockHeader)
  , utxoResultBlockNo :: !C.BlockNo
  , utxoResultTxIndex :: !TxIndexInBlock
  , utxoResultTxIn :: !C.TxIn
  , utxoResultDatumHash :: !(Maybe (C.Hash C.ScriptData))
  , utxoResultDatum :: !(Maybe C.ScriptData)
  , utxoResultValue :: !C.Value
  , utxoResultSpentInfo :: !(Maybe SpentInfoResult)
  , utxoResultInputs :: ![UtxoTxInput] -- List of inputs that were used in the transaction that created this UTxO.
  }
  deriving (Eq, Show, Generic)

instance ToJSON AddressUtxoResult where
  toJSON (AddressUtxoResult slotNo bhh bn txIndexInBlock txIn dath dat value spentBy txInputs) =
    let C.TxIn txId txIx = txIn
     in Aeson.object
          [ "slotNo" .= slotNo
          , "blockHeaderHash" .= bhh
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
            <$> v .: "slotNo"
            <*> v .: "blockHeaderHash"
            <*> v .: "blockNo"
            <*> v .: "txIndexInBlock"
            <*> (C.TxIn <$> v .: "txId" <*> v .: "txIx")
            <*> v .: "datumHash"
            <*> v .: "datum"
            <*> (getSidechainValue <$> v .: "value")
            <*> v .: "spentBy"
            <*> v .: "txInputs"
     in Aeson.withObject "AddressUtxoResult" parseAddressUtxoResult

newtype UtxoTxInput = UtxoTxInput C.TxIn
  deriving (Eq, Ord, Show, Generic)

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

data GetBurnTokenEventsParams = GetBurnTokenEventsParams
  { policyId :: !C.PolicyId
  , assetName :: !(Maybe C.AssetName)
  , beforeSlotNo :: !(Maybe Word64)
  , afterTx :: !(Maybe C.TxId)
  }
  deriving (Eq, Show)

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

newtype GetBurnTokenEventsResult
  = GetBurnTokenEventsResult [AssetIdTxResult]
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data AssetIdTxResult
  = -- | Burn amount only, so this is always a positive number
    AssetIdTxResult
      !C.SlotNo
      !(C.Hash C.BlockHeader)
      !C.BlockNo
      !C.TxId
      !(Maybe (C.Hash C.ScriptData))
      !(Maybe C.ScriptData)
      !C.AssetName
      !C.Quantity
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AssetIdTxResult where
  toJSON (AssetIdTxResult slotNo bhh bn txId redh red an qty) =
    Aeson.object
      [ "slotNo" .= slotNo
      , "blockHeaderHash" .= bhh
      , "blockNo" .= bn
      , "txId" .= txId
      , "redeemerHash" .= redh
      , "redeemer" .= red
      , "assetName" .= an
      , "burnAmount" .= qty
      ]

instance FromJSON AssetIdTxResult where
  parseJSON =
    let parseAssetIdTxResult v =
          AssetIdTxResult
            <$> v .: "slotNo"
            <*> v .: "blockHeaderHash"
            <*> v .: "blockNo"
            <*> v .: "txId"
            <*> v .: "redeemerHash"
            <*> v .: "redeemer"
            <*> v .: "assetName"
            <*> v .: "burnAmount"
     in Aeson.withObject "AssetIdTxResult" parseAssetIdTxResult

newtype GetEpochActiveStakePoolDelegationResult
  = GetEpochActiveStakePoolDelegationResult [ActiveSDDResult]
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ActiveSDDResult
  = ActiveSDDResult
      !C.PoolId
      !C.Lovelace
      !C.SlotNo
      !(C.Hash C.BlockHeader)
      !C.BlockNo
  deriving (Eq, Ord, Show)

instance FromJSON ActiveSDDResult where
  parseJSON =
    let parseResult v = do
          ActiveSDDResult
            <$> v .: "poolId"
            <*> v .: "lovelace"
            <*> (C.SlotNo <$> v .: "slotNo")
            <*> v .: "blockHeaderHash"
            <*> (C.BlockNo <$> v .: "blockNo")
     in Aeson.withObject "ActiveSDDResult" parseResult

instance ToJSON ActiveSDDResult where
  toJSON
    ( ActiveSDDResult
        poolId
        lovelace
        (C.SlotNo slotNo)
        blockHeaderHash
        (C.BlockNo blockNo)
      ) =
      Aeson.object
        [ "poolId" .= poolId
        , "lovelace" .= lovelace
        , "slotNo" .= slotNo
        , "blockHeaderHash" .= blockHeaderHash
        , "blockNo" .= blockNo
        ]

newtype GetEpochNonceResult
  = GetEpochNonceResult (Maybe NonceResult)
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data NonceResult
  = NonceResult
      !Ledger.Nonce
      !C.SlotNo
      !(C.Hash C.BlockHeader)
      !C.BlockNo
  deriving (Eq, Ord, Show)

instance FromJSON NonceResult where
  parseJSON =
    let parseResult v = do
          NonceResult
            <$> (Ledger.Nonce <$> v .: "nonce")
            <*> (C.SlotNo <$> v .: "slotNo")
            <*> v .: "blockHeaderHash"
            <*> (C.BlockNo <$> v .: "blockNo")
     in Aeson.withObject "NonceResult" parseResult

instance ToJSON NonceResult where
  toJSON
    ( NonceResult
        nonce
        (C.SlotNo slotNo)
        blockHeaderHash
        (C.BlockNo blockNo)
      ) =
      let nonceValue = case nonce of
            Ledger.NeutralNonce -> Nothing
            Ledger.Nonce n -> Just n
       in Aeson.object
            [ "nonce" .= nonceValue
            , "slotNo" .= slotNo
            , "blockHeaderHash" .= blockHeaderHash
            , "blockNo" .= blockNo
            ]
