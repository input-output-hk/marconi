{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Crypto
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
  { nonce :: NonceWrapper
  , slotNo :: Maybe C.SlotNo
  , blockHeaderHash :: Maybe (C.Hash C.BlockHeader)
  , blockNo :: C.BlockNo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Type equivalent to @Ledger.'Nonce'@ that has the desired JSON shape, with 'Nothing'
corresponding to @Ledger.'NeutralNonce'@.
-}
type NonceWrapper = Maybe (Crypto.Hash Crypto.Blake2b_256 Ledger.Nonce)

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
    mapResult (ChainIndex.EpochState.EpochNonceResult hash bn _ sn nc) = NonceResult nc sn hash <$> bn
