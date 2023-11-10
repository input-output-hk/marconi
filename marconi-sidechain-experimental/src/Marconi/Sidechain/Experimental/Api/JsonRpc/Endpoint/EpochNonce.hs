{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce where

import Cardano.Api qualified as C
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState qualified as ChainIndex.EpochState
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, mapChainIndexExceptT)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}

type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    Word64
    String
    SidechainEpochNonceResult

{- TYPES -}

type SidechainEpochNonceResult = Maybe NonceResult

-- | Result type that determines the JSON shape.
data NonceResult = NonceResult
  { nonce :: !Ledger.Nonce
  , slotNo :: !(Maybe C.SlotNo)
  , blockHeaderHash :: !(Maybe (C.Hash C.BlockHeader))
  , blockNo :: !C.BlockNo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- HANDLER -}

{- | Call @ChainIndex.EpochState.'getEpochNonceHandler'@, mapping the configuration
and result types to match the ones of this package.
-}
getEpochNonceHandler
  :: Word64
  -- ^ EpochNo
  -> ReaderHandler SidechainHttpServerConfig (Either (JsonRpcErr String) SidechainEpochNonceResult)
getEpochNonceHandler = mapChainIndexExceptT . fmap (fmap mapResult) . ChainIndex.EpochState.getEpochNonceHandler
  where
    mapResult :: ChainIndex.EpochState.EpochNonceResult -> SidechainEpochNonceResult
    -- TODO: PLT-8076 what did the Nothing case in sidechain correspond to in the chain-index
    -- version? A guess for now based on EpochState module.
    mapResult (ChainIndex.EpochState.EpochNonceResult hash bn _ sn nc) =
      case (bn, nc) of
        (Nothing, _) -> Nothing
        (Just bn', Nothing) -> Just $ NonceResult Ledger.NeutralNonce sn hash bn'
        (Just bn', Just h) -> Just $ NonceResult (Ledger.Nonce h) sn hash bn'
