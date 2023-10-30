{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState (
  RpcEpochActiveStakePoolDelegationMethod,
  RpcEpochNonceMethod,
  getEpochNonceHandler,
  getEpochStakePoolDelegationHandler,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens.Getter qualified as Lens
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.:), (.=))
import Data.Aeson qualified as Aeson
import GHC.Word (Word64)
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configQueryables)
import Marconi.ChainIndex.Indexers (
  queryableEpochState,
 )
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler, dimapHandler, queryHttpReaderHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

------------------
-- Method types --
------------------

type RpcEpochActiveStakePoolDelegationMethod =
  JsonRpc
    "getActiveStakePoolDelegationByEpoch"
    Word64
    String
    [ActiveSDDResult]

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
            <$> v
              .: "poolId"
            <*> v
              .: "lovelace"
            <*> (C.SlotNo <$> v .: "slotNo")
            <*> v
              .: "blockHeaderHash"
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
  :: Word64
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) [ActiveSDDResult])
getEpochStakePoolDelegationHandler =
  dimapHandler f g $ queryHttpReaderHandler (configQueryables . queryableEpochState)
  where
    f :: Word64 -> EpochState.ActiveSDDByEpochNoQuery
    f = EpochState.ActiveSDDByEpochNoQuery . C.EpochNo
    g :: Core.Result EpochState.ActiveSDDByEpochNoQuery -> [ActiveSDDResult]
    g =
      let
        x :: Core.Timed C.ChainPoint EpochState.EpochSDD -> ActiveSDDResult
        x (Core.Timed chainPoint epochSDD) =
          case chainPoint of
            C.ChainPoint slotNo hash ->
              ActiveSDDResult
                (Lens.view EpochState.sddPoolId epochSDD)
                (Lens.view EpochState.sddLovelace epochSDD)
                slotNo
                hash
                (Lens.view EpochState.sddBlockNo epochSDD)
            C.ChainPointAtGenesis ->
              -- TODO: I believe this is actually impossible, the types are too loose,
              -- a solution would be to change the implementation of the query to construct
              -- an 'ActiveSDDResult' directly (not sure yet if feasible)
              undefined
       in
        fmap x

-- | Return an epoch nonce
getEpochNonceHandler
  :: EpochState.NonceByEpochNoQuery
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) (Core.Result EpochState.NonceByEpochNoQuery))
getEpochNonceHandler = queryHttpReaderHandler (configQueryables . queryableEpochState)
