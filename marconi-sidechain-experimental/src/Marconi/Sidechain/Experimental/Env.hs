{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Sidechain.Experimental.Env where

import Control.Lens (makeLenses)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig)
import Marconi.Sidechain.Experimental.CLI (
  CliArgs (CliArgs, httpPort, targetAddresses, targetAssets),
 )
import Marconi.Sidechain.Experimental.Indexers (SidechainIndexersConfig)

{- TYPE -}

{- | Sidechain application config, including the configurations for running the indexers and for
 - running the http server. Construct with 'mkSidechainEnvFromCliArgs' to ensure shared components
 - have the same values, such as the security parameter.
-}
data SidechainEnv = SidechainEnv
  { _sidechainHttpServerConfig :: !SidechainHttpServerConfig
  , _sidechainIndexersConfig :: !SidechainIndexersConfig
  }

{- CONSTRUCTORS -}

-- mkSidechainEnv
--  :: SecurityParam
--  -> Port
--  -> Maybe TargetAddresses
--  -> Maybe TargetAssets
--  -> CliArgs
--  -> MarconiTrace IO
--  -> IO SidechainEnv
-- mkSidechainEnv securityParam httpPort targetAddresses targetAssets cliArgs trace = do
--  -- TODO: PLT-8076 decide whether you want to use the chain-index http server stuff
--  -- or the sidechain server stuff. likely the former.
--  let httpSettings = undefined
--  let sidechainQueryEnv = SidechainQueryEnv undefined undefined
--  -- TODO: PLT-8076
--  -- sidechain <-
--  --  SidechainIndexersEnv
--  --    <$> mkAddressUtxoIndexerEnv targetAddresses
--  --    <*> mkMintEventIndexerEnv targetAssets
--  --    <*> mkEpochStateEnv
--  -- pure $ SidechainEnv sidechainQueryEnv sidechainIndexers cliArgs trace
--  undefined
--
-- mkSidechainEnvFromCliArgs
--  :: SecurityParam
--  -> CliArgs
--  -> MarconiTrace IO
--  -> IO SidechainEnv
-- mkSidechainEnvFromCliArgs securityParam cliArgs@CliArgs{httpPort, targetAddresses, targetAssets} trace = do
--  mkSidechainEnv securityParam httpPort targetAddresses targetAssets cliArgs trace
--
-- makeLenses ''SidechainQueryEnv

{- LENSES -}
makeLenses ''SidechainEnv
