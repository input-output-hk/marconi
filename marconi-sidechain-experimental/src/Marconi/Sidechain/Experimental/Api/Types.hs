module Marconi.Sidechain.Experimental.Api.Types (
  Types.configPort,
  Types.configTrace,
  SidechainHttpServerConfig,
) where

import qualified Marconi.ChainIndex.Api.Types as Types

{- | TODO: PLT-8076 this likely needs to be changed. can augment or modify the one from
chain-index. should include the cli args as well based on existing sidechain package
for example, even the TargetAddresses type of marconi-sidechain is not exactly the [AddressAny]
  type of marconi-chain-index. Perhaps that does not need to be handled here, so long as the APIs
  do not change.
-}
type SidechainHttpServerConfig = Types.HttpServerConfig
