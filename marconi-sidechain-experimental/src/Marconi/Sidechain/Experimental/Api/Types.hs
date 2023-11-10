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
 - 'SidechainExtraHttpServerConfig', containing anything needed for sidechain
 - queries not included in HttpServerConfig.
-}
data SidechainHttpServerConfig = SidechainHttpServerConfig
  { _chainIndexHttpServerConfig :: !Types.HttpServerConfig
  , _sidechainExtraHttpServerConfig :: !SidechainExtraHttpServerConfig
  }

{- SIDECHAIN EXTRA CONFIG -}

type TargetAssets = NonEmpty (C.PolicyId, Maybe C.AssetName)

newtype SidechainExtraHttpServerConfig = SidechainExtraHttpServerConfig
  { _sidechainTargetAssets :: Maybe TargetAssets
  }

{- LENSES -}
makeLenses ''SidechainExtraHttpServerConfig
makeLenses ''SidechainHttpServerConfig

{- UTILITIES -}

-- | Specialization of 'withReaderT'.
withChainIndexHttpServerConfig
  :: ReaderT Types.HttpServerConfig m a -> ReaderT SidechainHttpServerConfig m a
withChainIndexHttpServerConfig = withReaderT (^. chainIndexHttpServerConfig)

-- | Specialization of 'mapError' and 'withReaderT' in the case used here.
mapChainIndexExceptT
  :: ExceptT e (ReaderT Types.HttpServerConfig m) a
  -> ExceptT e (ReaderT SidechainHttpServerConfig m) a
mapChainIndexExceptT = mapExceptT withChainIndexHttpServerConfig
