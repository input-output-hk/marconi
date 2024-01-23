{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.EpochState (
  RpcEpochActiveStakePoolDelegationMethod,
  RpcEpochNonceMethod,
  ActiveSDDResult (..),
  EpochNonceResult (..),
  getEpochNonceHandler,
  getEpochStakePoolDelegationHandler,
  nonceToMaybe,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Shelley.API qualified as Ledger
import Control.Lens.Getter qualified as Lens
import Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import Data.Char (toLower)
import GHC.Word (Word64)
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig, configQueryables)
import Marconi.Cardano.ChainIndex.Indexers (
  queryableEpochNonce,
  queryableEpochSDD,
 )
import Marconi.Cardano.Indexers.EpochNonce qualified as EpochState
import Marconi.Cardano.Indexers.EpochSDD qualified as EpochState
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
    Word64
    String
    EpochNonceResult

data EpochNonceResult = EpochNonceResult
  { epochNonceBlockHeaderHash :: !(Maybe (C.Hash C.BlockHeader))
  , epochNonceBlockNo :: !(Maybe C.BlockNo)
  , epochNonceEpochNo :: !(Maybe C.EpochNo)
  , epochNonceSlotNo :: !(Maybe C.SlotNo)
  , epochNonceNonce :: !(Maybe (Crypto.Hash Crypto.Blake2b_256 Ledger.Nonce))
  }
  deriving stock (Eq, Ord, Show)

$( deriveJSON
    defaultOptions
      { fieldLabelModifier = \str ->
          case drop 10 str of
            c : rest -> toLower c : rest
            _ -> error "Malformed label in JSON type EpochNonceResult."
      }
    ''EpochNonceResult
 )

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
        (queryHttpReaderHandler (configQueryables . queryableEpochSDD))
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
        (C.chainPointToSlotNo chainPoint)
        (C.chainPointToHeaderHash chainPoint)
        (Lens.view EpochState.sddBlockNo epochSDD)
        (Lens.view EpochState.sddEpochNo epochSDD)

-- | Return an epoch nonce
getEpochNonceHandler
  :: Word64
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) EpochNonceResult)
getEpochNonceHandler epochNo
  | epochNo < 0 =
      return . Left . mkJsonRpcInvalidParamsErr . Just $
        "The 'epochNo' param value must be a natural number."
  | otherwise =
      dimapHandler
        toNonceByEpochNoQuery
        toEpochNonceResult
        (queryHttpReaderHandler (configQueryables . queryableEpochNonce))
        epochNo
  where
    toNonceByEpochNoQuery :: Word64 -> EpochState.NonceByEpochNoQuery
    toNonceByEpochNoQuery = EpochState.NonceByEpochNoQuery . C.EpochNo

    toEpochNonceResult :: Core.Result EpochState.NonceByEpochNoQuery -> EpochNonceResult
    toEpochNonceResult Nothing = EpochNonceResult Nothing Nothing Nothing Nothing Nothing
    toEpochNonceResult (Just (Core.Timed chainPoint epochNonce)) =
      EpochNonceResult
        (C.chainPointToHeaderHash chainPoint)
        (Just $ Lens.view EpochState.nonceBlockNo epochNonce)
        (Just $ Lens.view EpochState.nonceEpochNo epochNonce)
        (C.chainPointToSlotNo chainPoint)
        (nonceToMaybe $ Lens.view EpochState.nonceNonce epochNonce)

nonceToMaybe :: Ledger.Nonce -> Maybe (Crypto.Hash Crypto.Blake2b_256 Ledger.Nonce)
nonceToMaybe Ledger.NeutralNonce = Nothing
nonceToMaybe (Ledger.Nonce hash) = Just hash
