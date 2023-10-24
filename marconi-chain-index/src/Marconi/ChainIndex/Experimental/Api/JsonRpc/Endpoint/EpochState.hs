{-# LANGUAGE DataKinds #-}

module Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.EpochState (
  RpcEpochActiveStakePoolDelegationMethod,
  RpcEpochNonceMethod,
  getEpochNonceHandler,
  getEpochStakePoolDelegationHandler,
) where

import Marconi.ChainIndex.Experimental.Api.Types (HttpServerConfig)
import Marconi.ChainIndex.Experimental.Indexers.EpochState qualified as EpochState
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

------------------
-- Method types --
------------------

type RpcEpochActiveStakePoolDelegationMethod =
  JsonRpc
    "getActiveStakePoolDelegationByEpoch"
    EpochState.ActiveSDDByEpochNoQuery
    String
    (Core.Result EpochState.ActiveSDDByEpochNoQuery)

type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    EpochState.NonceByEpochNoQuery
    String
    (Core.Result EpochState.NonceByEpochNoQuery)

--------------
-- Handlers --
--------------

-- | Return the stake pool delegation per epoch
getEpochStakePoolDelegationHandler
  :: EpochState.ActiveSDDByEpochNoQuery
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) (Core.Result EpochState.ActiveSDDByEpochNoQuery))
getEpochStakePoolDelegationHandler = queryHttpReaderHandler (configQueryables . queryableEpochState)

-- | Return an epoch nonce
getEpochNonceHandler
  :: EpochState.NonceByEpochNoQuery
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) (Core.Result EpochState.NonceByEpochNoQuery))
getEpochNonceHandler = queryHttpReaderHandler (configQueryables . queryableEpochState)
