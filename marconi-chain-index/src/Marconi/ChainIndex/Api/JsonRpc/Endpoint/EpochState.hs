{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState (
  RpcEpochActiveStakePoolDelegationMethod,
  RpcEpochNonceMethod,
  ActiveSDDResult (..),
  getEpochNonceHandler,
  getEpochStakePoolDelegationHandler,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens.Getter qualified as Lens
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (parseJSON))
import GHC.Word (Word64)
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configQueryables)
import Marconi.ChainIndex.Indexers (
  queryableEpochState,
 )
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Utils qualified as Util
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

data ActiveSDDResult = ActiveSDDResult
  { activeSDDResultPoolId :: !C.PoolId
  , activeSDDResultLovelace :: !C.Lovelace
  , activeSDDResultSlotNo :: !(Maybe C.SlotNo)
  , activeSDDResultHash :: !(Maybe (C.Hash C.BlockHeader))
  , activeSDDResultBlockNo :: !C.BlockNo
  }
  deriving stock (Eq, Ord, Show)

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
              .: "blockHeaderHash"
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
  dimapHandler toActiveSDDByEpochNoQuery toActiveSDDResults $
    queryHttpReaderHandler (configQueryables . queryableEpochState)
  where
    toActiveSDDByEpochNoQuery :: Word64 -> EpochState.ActiveSDDByEpochNoQuery
    toActiveSDDByEpochNoQuery = EpochState.ActiveSDDByEpochNoQuery . C.EpochNo

    toActiveSDDResults :: Core.Result EpochState.ActiveSDDByEpochNoQuery -> [ActiveSDDResult]
    toActiveSDDResults = fmap toActiveSDDResult

    toActiveSDDResult :: Core.Timed C.ChainPoint EpochState.EpochSDD -> ActiveSDDResult
    toActiveSDDResult (Core.Timed chainPoint epochSDD) =
      ActiveSDDResult
        (Lens.view EpochState.sddPoolId epochSDD)
        (Lens.view EpochState.sddLovelace epochSDD)
        (Util.chainPointToSlotNo chainPoint)
        (Util.chainPointToHash chainPoint)
        (Lens.view EpochState.sddBlockNo epochSDD)

-- | Return an epoch nonce
getEpochNonceHandler
  :: EpochState.NonceByEpochNoQuery
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) (Core.Result EpochState.NonceByEpochNoQuery))
getEpochNonceHandler = queryHttpReaderHandler (configQueryables . queryableEpochState)
