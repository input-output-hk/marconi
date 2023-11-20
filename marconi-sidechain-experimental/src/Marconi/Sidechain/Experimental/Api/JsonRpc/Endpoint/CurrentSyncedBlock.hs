{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock where

import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock qualified as ChainIndex
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock.Tip qualified as ChainIndex
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, UnusedRequestParams)

{- METHOD -}

type RpcCurrentSyncedBlockMethod =
  JsonRpc
    "getCurrentSyncedBlock"
    UnusedRequestParams
    String
    -- TODO: PLT-8630 do we need to model WithOrigin from marconi-sidechain version?
    ChainIndex.GetCurrentSyncedBlockResult

{- HANDLER -}

-- | TODO: PLT-8630 be sure it is the same as for marconi-sidechain
getCurrentSyncedBlockHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler
      SidechainHttpServerConfig
      (Either (JsonRpcErr String) ChainIndex.GetCurrentSyncedBlockResult)
getCurrentSyncedBlockHandler = withChainIndexHandler . ChainIndex.getCurrentSyncedBlockHandler
