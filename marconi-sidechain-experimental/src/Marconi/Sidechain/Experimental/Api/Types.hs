{-# LANGUAGE TemplateHaskell #-}

module Marconi.Sidechain.Experimental.Api.Types (
  Types.configPort,
  Types.configTrace,
  module Marconi.Sidechain.Experimental.Api.Types,
) where

import Cardano.Api qualified as C
import Control.Lens (makeLenses, (^.))
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT, withReaderT)
import Data.List.NonEmpty (NonEmpty)
import Marconi.ChainIndex.Api.Types qualified as Types

{- SERVER CONFIG -}

{- | 'SidechainHttpServerConfig' augments @Types.'HttpServerConfig'@ with
 - additional fields containing anything needed for sidechain
 - queries not included in @Types.'HttpServerConfig'@.
-}
data SidechainHttpServerConfig = SidechainHttpServerConfig
  { _chainIndexHttpServerConfig :: !Types.HttpServerConfig
  , _sidechainTargetAssets :: Maybe TargetAssets
  }

{- SIDECHAIN EXTRA CONFIG -}

type TargetAssets = NonEmpty (C.PolicyId, Maybe C.AssetName)

{- LENSES -}
makeLenses ''SidechainHttpServerConfig

{- UTILITIES -}

{- | Specialization of a composition of 'mapError' and 'withReaderT' to facilitate
mappings between handlers from `marconi-chain-index` using @Types.'HttpServerConfig'@
and handlers of this package using 'SidechainHttpServerConfig'.
-}
withChainIndexHandler
  :: ExceptT e (ReaderT Types.HttpServerConfig m) a
  -> ExceptT e (ReaderT SidechainHttpServerConfig m) a
withChainIndexHandler = mapExceptT (withReaderT (^. chainIndexHttpServerConfig))
