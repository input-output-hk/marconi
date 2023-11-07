{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo where

import Cardano.Api (FromJSON, SerialiseAddress (serialiseAddress), ToJSON)
import Cardano.Api qualified as C
import Control.Lens (view)
import Data.Text (Text)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configTrackedAddresses)
import Marconi.Core.JsonRpc (ReaderHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, UnusedRequestParams)

------------------
-- Method types --
------------------

type RpcTargetAddressesMethod =
  JsonRpc
    "getUtxosFromAddress"
    GetUtxosFromAddressParams
    String
    [Text]

--------------
-- Handlers --
--------------

-- | Return the list of TargetAddresses in Bech32 representation.
getTargetAddressesQueryHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler _ = do
  addresses <- view configTrackedAddresses
  pure $ Right $ serialiseAddress <$> addresses

data GetUtxosFromAddressParams = GetUtxosFromAddressParams
  { address :: !String
  -- ^ address to query for
  , createdAtOrAfterSlotNo :: !(Maybe C.SlotNo)
  -- ^ Query interval
  , unspentBeforeSlotNo :: !(Maybe C.SlotNo)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
