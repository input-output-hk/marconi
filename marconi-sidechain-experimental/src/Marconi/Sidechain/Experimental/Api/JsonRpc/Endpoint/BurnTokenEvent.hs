{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken qualified as ChainIndex
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}

type RpcGetBurnTokenEventsMethod =
  JsonRpc
    "getBurnTokenEvents"
    GetBurnTokenEventsParams
    String
    -- NOTE: record field names and thus JSON shape identical to existing
    -- marconi-sidechain version of this result type.
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

{- | Convert the sidechain param shape to the one from 'marconi-chain-index',
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

{- | TODO: PLT-8630 check again that the results are the same. need to compare directly from code
in absence of tests.
-}
getBurnTokenEventsHandler
  :: GetBurnTokenEventsParams
  -> ReaderHandler
      SidechainHttpServerConfig
      (Either (JsonRpcErr String) ChainIndex.GetBurnTokenEventsResult)
getBurnTokenEventsHandler =
  withChainIndexHandler . ChainIndex.getBurnTokenEventsHandler . sidechainParamsToChainIndexParams
