{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsParams (GetBurnTokenEventsParams),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
  RpcGetBurnTokenEventsMethod,
  getBurnTokenEventsHandler,
  getMintingPolicyHashTxHandler,
) where

import Cardano.Api (FromJSON)
import Cardano.Api qualified as C
import Control.Applicative ((<|>))
import Control.Comonad (Comonad (extract))
import Control.Lens (view, (^.), (^?), _1, _2)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types (ToJSON (toJSON), object, parseJSON, withObject, (.:), (.:?), (.=))
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig, configQueryables)
import Marconi.Cardano.ChainIndex.Indexers (queryableMintToken)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Indexers.MintTokenEvent (
  mintAssetAssetName,
  mintAssetQuantity,
  mintAssetRedeemer,
  mintAssetRedeemerData,
  mintAssetRedeemerHash,
  mintTokenEventAsset,
  mintTokenEventBlockNo,
  mintTokenEventLocation,
  mintTokenEventTxId,
  mintTokenEvents,
 )
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler, dimapHandler, hoistHttpHandler, queryErrToRpcErr)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

------------------
-- Method types --
------------------

type RpcGetBurnTokenEventsMethod =
  JsonRpc
    "getBurnTokenEvents"
    GetBurnTokenEventsParams
    String
    GetBurnTokenEventsResult

----------------------------
-- Query and result types --
----------------------------

data GetBurnTokenEventsParams = GetBurnTokenEventsParams
  { getBurnTokenEventsParamsPolicyId :: !C.PolicyId
  , getBurnTokenEventsParamsAssetName :: !(Maybe C.AssetName)
  , getBurnTokenEventsParamsBeforeSlotNo :: !(Maybe C.SlotNo)
  , getBurnTokenEventsParamsAfterTx :: !(Maybe C.TxId)
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
     in withObject "GetBurnTokenEventsParams" parseParams

instance ToJSON GetBurnTokenEventsParams where
  toJSON q =
    object $
      catMaybes
        [ Just ("policyId" .= getBurnTokenEventsParamsPolicyId q)
        , ("assetName" .=) <$> getBurnTokenEventsParamsAssetName q
        , ("createdBeforeSlotNo" .=) <$> getBurnTokenEventsParamsBeforeSlotNo q
        , ("createdAfterTx" .=) <$> getBurnTokenEventsParamsAfterTx q
        ]

newtype GetBurnTokenEventsResult
  = GetBurnTokenEventsResult [BurnTokenEventResult]
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The quantity represents a burn amount only, so this is always a positive number.
data BurnTokenEventResult = BurnTokenEventResult
  { slotNo :: !(Maybe C.SlotNo)
  , blockHeaderHash :: !(Maybe (C.Hash C.BlockHeader))
  , blockNo :: !C.BlockNo
  , txId :: !C.TxId
  , redeemerHash :: !(Maybe (C.Hash C.ScriptData))
  , redeemer :: !(Maybe C.ScriptData)
  , assetName :: !C.AssetName
  , burnAmount :: !C.Quantity
  , isStable :: !Bool
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

--------------
-- Handlers --
--------------

getMintingPolicyHashTxHandler
  :: MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents
  -> ReaderHandler
      HttpServerConfig
      ( Either
          (JsonRpcErr String)
          ( Core.Result
              ( Core.WithStability
                  (MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents)
              )
          )
      )
getMintingPolicyHashTxHandler q = do
  indexer <- view (configQueryables . queryableMintToken)
  hoistHttpHandler $
    liftIO $
      first queryErrToRpcErr <$> Core.queryLatestEither (Core.WithStability q) indexer

-- | Return 'GetBurnTokenEventsResult' based on 'GetBurnTokenEventsParams'
getBurnTokenEventsHandler
  :: GetBurnTokenEventsParams
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) GetBurnTokenEventsResult)
getBurnTokenEventsHandler =
  dimapHandler mapGetBurnTokenEventsQuery mapGetBurnTokenEventsResult getMintingPolicyHashTxHandler
  where
    -- Map the more specific 'GetBurnTokenEventsParams' query to the more general 'QueryByAssetId'
    mapGetBurnTokenEventsQuery
      :: GetBurnTokenEventsParams -> MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents
    mapGetBurnTokenEventsQuery (GetBurnTokenEventsParams policyId assetName' beforeSlotNo afterTx) =
      MintTokenEvent.QueryByAssetId
        policyId
        assetName'
        (Just MintTokenEvent.BurnEventType)
        beforeSlotNo
        afterTx
    -- Map internal events to our 'getBurnTokenEventsHandler' response object
    mapGetBurnTokenEventsResult
      :: [ Core.Stability
            ( Core.Timed
                (Core.Point MintTokenEvent.MintTokenBlockEvents)
                MintTokenEvent.MintTokenBlockEvents
            )
         ]
      -> GetBurnTokenEventsResult
    mapGetBurnTokenEventsResult events = GetBurnTokenEventsResult $ mapResult =<< events
    {- Map a block's worth of internal events to the components of our 'getBurnTokenEventsHandler'
    response object -}
    mapResult
      :: Core.Stability (Core.Timed C.ChainPoint MintTokenEvent.MintTokenBlockEvents)
      -> [BurnTokenEventResult]
    mapResult res =
      let timed = extract res
       in mapEvent (timed ^. Core.point) (Core.isStable res)
            <$> NonEmpty.toList (timed ^. Core.event . mintTokenEvents)

    {- Map an internal event to a single component of our 'getBurnTokenEventsHandler' response
    object -}
    mapEvent :: C.ChainPoint -> Bool -> MintTokenEvent.MintTokenEvent -> BurnTokenEventResult
    mapEvent chainPoint stable event =
      let slotAndHash = case chainPoint of
            (C.ChainPoint slot hash) -> Just (slot, hash)
            C.ChainPointAtGenesis -> Nothing
       in BurnTokenEventResult
            (slotAndHash ^? traverse . _1)
            (slotAndHash ^? traverse . _2)
            (event ^. mintTokenEventLocation . mintTokenEventBlockNo)
            (event ^. mintTokenEventLocation . mintTokenEventTxId)
            (event ^? mintTokenEventAsset . mintAssetRedeemer . traverse . mintAssetRedeemerHash)
            (event ^? mintTokenEventAsset . mintAssetRedeemer . traverse . mintAssetRedeemerData)
            (event ^. mintTokenEventAsset . mintAssetAssetName)
            (abs $ event ^. mintTokenEventAsset . mintAssetQuantity)
            stable
