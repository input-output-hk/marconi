{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.TargetAddresses where

import Cardano.Api (serialiseAddress)
import Control.Lens (view)
import Data.Text (Text)
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig, configTrackedAddresses)
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
