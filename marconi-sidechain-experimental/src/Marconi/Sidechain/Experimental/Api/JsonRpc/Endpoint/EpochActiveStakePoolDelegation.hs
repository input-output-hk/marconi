{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochActiveStakePoolDelegation (
  RpcEpochActiveStakePoolDelegationMethod,
  GetEpochActiveStakePoolDelegationResult (..),
  ActiveSDDResult (..),
  getEpochActiveStakePoolDelegationHandler,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Orphans ()
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState qualified as ChainIndex
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}
type RpcEpochActiveStakePoolDelegationMethod =
  JsonRpc
    "getActiveStakePoolDelegationByEpoch"
    Word64
    String
    GetEpochActiveStakePoolDelegationResult

{- TYPES -}

newtype GetEpochActiveStakePoolDelegationResult
  = GetEpochActiveStakePoolDelegationResult [ActiveSDDResult]
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Type defining JSON shape of the result.
data ActiveSDDResult = ActiveSDDResult
  { poolId :: C.PoolId
  , lovelace :: C.Lovelace
  , slotNo :: Maybe C.SlotNo
  , blockHeaderHash :: Maybe (C.Hash C.BlockHeader)
  , blockNo :: C.BlockNo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

sidechainActiveSDDResultFromChainIndex :: ChainIndex.ActiveSDDResult -> ActiveSDDResult
sidechainActiveSDDResultFromChainIndex ChainIndex.ActiveSDDResult{..} =
  ActiveSDDResult
    activeSDDResultPoolId
    activeSDDResultLovelace
    activeSDDResultSlotNo
    activeSDDResultBlockHeaderHash
    activeSDDResultBlockNo

{- HANDLER -}

getEpochActiveStakePoolDelegationHandler
  :: Word64
  -> ReaderHandler
      SidechainHttpServerConfig
      (Either (JsonRpcErr String) GetEpochActiveStakePoolDelegationResult)
getEpochActiveStakePoolDelegationHandler =
  withChainIndexHandler
    . fmap (fmap sidechainResultFromChainIndex)
    . ChainIndex.getEpochStakePoolDelegationHandler
  where
    sidechainResultFromChainIndex =
      GetEpochActiveStakePoolDelegationResult . map sidechainActiveSDDResultFromChainIndex
