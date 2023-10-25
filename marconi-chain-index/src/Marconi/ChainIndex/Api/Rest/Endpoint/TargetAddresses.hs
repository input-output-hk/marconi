{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.ChainIndex.Api.Rest.Endpoint.TargetAddresses where

import Cardano.Api (serialiseAddress)
import Control.Lens (view)
import Data.Text (Text)
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configTrackedAddresses)
import Marconi.Core.JsonRpc (ReaderHandler)
import Servant (Get, JSON, (:>))

------------------
-- Method types --
------------------

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

----------------------------
-- Query and result types --
----------------------------

-- | Prints TargetAddresses Bech32 representation to the console
getTargetAddressesHandler :: ReaderHandler HttpServerConfig [Text]
getTargetAddressesHandler = do
  addresses <- view configTrackedAddresses
  pure $ serialiseAddress <$> addresses
