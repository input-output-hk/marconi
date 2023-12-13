{-# LANGUAGE TemplateHaskell #-}

module Marconi.ChainIndex.Api.Types (
  HttpServerConfig (HttpServerConfig),
  configTrace,
  configPort,
  configSecurityParam,
  configTrackedAddresses,
  configParams,
  configQueryables,
) where

import Cardano.Api (AddressAny)
import Cardano.BM.Trace (Trace)
import Control.Lens (makeLenses)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Marconi.Cardano.Core.Types (SecurityParam)
import Marconi.Cardano.Indexers (MarconiCardanoQueryables)

data HttpServerConfig = HttpServerConfig
  { _configTrace :: !(Trace IO Text)
  , _configPort :: !Int
  , _configSecurityParam :: !SecurityParam
  , _configTrackedAddresses :: ![AddressAny]
  , _configParams :: !Aeson.Value
  , _configQueryables :: !MarconiCardanoQueryables
  }

makeLenses 'HttpServerConfig
