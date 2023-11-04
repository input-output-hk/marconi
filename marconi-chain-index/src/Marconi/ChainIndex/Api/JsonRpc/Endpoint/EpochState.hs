{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import Data.Char (toLower)
import GHC.Word (Word64)
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configQueryables)
import Marconi.ChainIndex.Indexers (
  queryableEpochState,
 )
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Utils qualified as Util
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler, dimapHandler, queryHttpReaderHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, mkJsonRpcInvalidParamsErr)

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
  , activeSDDResultBlockHeaderHash :: !(Maybe (C.Hash C.BlockHeader))
  , activeSDDResultBlockNo :: !C.BlockNo
  , activeSDDResultEpochNo :: !C.EpochNo
  }
  deriving stock (Eq, Ord, Show)

$( deriveJSON
    defaultOptions
      { fieldLabelModifier = \str ->
          case drop 15 str of
            c : rest -> toLower c : rest
            _ -> error "Malformed label in JSON type ActiveSDDResult."
      }
    ''ActiveSDDResult
 )

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
getEpochStakePoolDelegationHandler epochNo
  | epochNo < 0 =
      return . Left . mkJsonRpcInvalidParamsErr . Just $
        "The 'epochNo' param value must be a natural number."
  | otherwise =
      dimapHandler
        toActiveSDDByEpochNoQuery
        toActiveSDDResults
        (queryHttpReaderHandler (configQueryables . queryableEpochState))
        epochNo
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
        (Lens.view EpochState.sddEpochNo epochSDD)

-- | Return an epoch nonce
getEpochNonceHandler
  :: EpochState.NonceByEpochNoQuery
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) (Core.Result EpochState.NonceByEpochNoQuery))
getEpochNonceHandler = queryHttpReaderHandler (configQueryables . queryableEpochState)
