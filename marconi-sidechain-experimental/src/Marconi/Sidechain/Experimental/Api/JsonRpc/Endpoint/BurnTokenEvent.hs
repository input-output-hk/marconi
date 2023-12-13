{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent (
  RpcGetBurnTokenEventsMethod,
  GetBurnTokenEventsParams (..),
  ChainIndex.GetBurnTokenEventsResult (..),
  ChainIndex.BurnTokenEventResult (..),
  getBurnTokenEventsHandler,
) where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Orphans ()
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken qualified as ChainIndex
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}

type RpcGetBurnTokenEventsMethod =
  JsonRpc
    "getBurnTokenEvents"
    GetBurnTokenEventsParams
    String
    ChainIndex.GetBurnTokenEventsResult

{- TYPES -}

-- | Parameter type defining the JSON shape.
data GetBurnTokenEventsParams = GetBurnTokenEventsParams
  { policyId :: C.PolicyId
  , assetName :: Maybe C.AssetName
  , createdBeforeSlotNo :: Maybe Word64
  , createdAfterTx :: Maybe C.TxId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Convert the sidechain param shape to the one from 'marconi-cardano-chain-index',
for reuse in the handler borrowed from that package.
-}
sidechainParamsToChainIndexParams
  :: GetBurnTokenEventsParams -> ChainIndex.GetBurnTokenEventsParams
sidechainParamsToChainIndexParams GetBurnTokenEventsParams{..} =
  ChainIndex.GetBurnTokenEventsParams
    policyId
    assetName
    (C.SlotNo <$> createdBeforeSlotNo)
    createdAfterTx

{- HANDLER -}

getBurnTokenEventsHandler
  :: GetBurnTokenEventsParams
  -> ReaderHandler
      SidechainHttpServerConfig
      (Either (JsonRpcErr String) ChainIndex.GetBurnTokenEventsResult)
getBurnTokenEventsHandler =
  withChainIndexHandler . ChainIndex.getBurnTokenEventsHandler . sidechainParamsToChainIndexParams
