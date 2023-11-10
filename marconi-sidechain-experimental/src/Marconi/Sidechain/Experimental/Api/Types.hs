{-# LANGUAGE TemplateHaskell #-}

module Marconi.Sidechain.Experimental.Api.Types (
  -- TODO: PLT-8076 export all lenses
  Types.configPort,
  Types.configTrace,
  module Marconi.Sidechain.Experimental.Api.Types,
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT, withReaderT)
import qualified Marconi.ChainIndex.Api.Types as Types

{- | TODO: PLT-8076 this likely needs to be changed. can augment or modify the one from
chain-index. should include the cli args as well based on existing sidechain package
for example, even the TargetAddresses type of marconi-sidechain is not exactly the [AddressAny]
  type of marconi-chain-index. Perhaps that does not need to be handled here, so long as the APIs
  do not change.
-}
data SidechainHttpServerConfig = SidechainHttpServerConfig
  { _chainIndexHttpServerConfig :: Types.HttpServerConfig
  , -- TODO: PLT-8076
    _sidechainLocalHttpServerConfig :: ()
  }

makeLenses ''SidechainHttpServerConfig

-- | Specialization of 'withReaderT'.
withChainIndexHttpServerConfig
  :: ReaderT Types.HttpServerConfig m a -> ReaderT SidechainHttpServerConfig m a
withChainIndexHttpServerConfig = withReaderT (^. chainIndexHttpServerConfig)

-- | Specialization of 'mapError' and 'withReaderT' in the case used here.
mapChainIndexExceptT
  :: ExceptT e (ReaderT Types.HttpServerConfig m) a
  -> ExceptT e (ReaderT SidechainHttpServerConfig m) a
mapChainIndexExceptT = mapExceptT withChainIndexHttpServerConfig
