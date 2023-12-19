{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Params where

import Control.Lens (view)
import Data.Aeson.Types qualified as Aeson
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig, configParams)
import Marconi.Core.JsonRpc (ReaderHandler)
import Servant (Get, JSON, (:>))

------------------
-- Method types --
------------------

type GetParams = "params" :> Get '[JSON] Aeson.Value

--------------
-- Handlers --
--------------

-- | Returns params given through CLI as a JSON REST response.
getParamsHandler :: ReaderHandler HttpServerConfig Aeson.Value
getParamsHandler = view configParams
