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
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}

type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    Word64
    String
    GetEpochNonceResult

{- TYPES -}

type GetEpochNonceResult = Maybe NonceResult

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
  -> ReaderHandler SidechainHttpServerConfig (Either (JsonRpcErr String) GetEpochNonceResult)
getEpochNonceHandler = withChainIndexHandler . fmap (fmap mapResult) . ChainIndex.EpochState.getEpochNonceHandler
  where
    mapResult :: ChainIndex.EpochState.EpochNonceResult -> GetEpochNonceResult
    mapResult (ChainIndex.EpochState.EpochNonceResult hash bn _ sn nc) =
      let nonce' = maybe Ledger.NeutralNonce Ledger.Nonce nc
       in NonceResult nonce' sn hash <$> bn
