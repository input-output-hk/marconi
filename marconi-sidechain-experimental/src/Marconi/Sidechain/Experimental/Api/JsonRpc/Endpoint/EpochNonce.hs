{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce where

import Cardano.Api qualified as C
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.Word (Word64)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState qualified as ChainIndex.EpochState
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, mapChainIndexExceptT)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}

-- | TODO: PLT-8076
type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    Word64
    String
    SidechainEpochNonceResult

{- TYPES -}

{- | TODO: PLT-8076 change this to one that supports the same json shape as the
sidechain package
-}
type SidechainEpochNonceResult = Maybe NonceResult

data NonceResult
  = NonceResult
      !Ledger.Nonce
      !(Maybe C.SlotNo)
      !(Maybe (C.Hash C.BlockHeader))
      !C.BlockNo
  deriving stock (Eq, Ord, Show)

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
    mapResult (ChainIndex.EpochState.EpochNonceResult blockHash blockNo _ slotNo nonce) =
      case (blockNo, nonce) of
        (Nothing, _) -> Nothing
        (Just bn, Nothing) -> Just $ NonceResult Ledger.NeutralNonce slotNo blockHash bn
        (Just bn, Just h) -> Just $ NonceResult (Ledger.Nonce h) slotNo blockHash bn
