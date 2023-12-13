{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo (
  RpcPastAddressUtxoMethod,
  getPastAddressUtxoHandler,
) where

import Marconi.Cardano.Core.Orphans ()
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo qualified as ChainIndex
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types qualified as ChainIndex
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}

type RpcPastAddressUtxoMethod =
  JsonRpc
    "getUtxosFromAddress"
    ChainIndex.GetUtxosFromAddressParams
    String
    ChainIndex.GetUtxosFromAddressResult

{- HANDLER -}

getPastAddressUtxoHandler
  :: ChainIndex.GetUtxosFromAddressParams
  -> ReaderHandler
      SidechainHttpServerConfig
      (Either (JsonRpcErr String) ChainIndex.GetUtxosFromAddressResult)
getPastAddressUtxoHandler = withChainIndexHandler . ChainIndex.getUtxosFromAddressQueryHandler
