{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Sidechain.Experimental.Env where

import Cardano.Api qualified as C
import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Marconi.ChainIndex.Api.Types (HttpServerConfig)
import Marconi.ChainIndex.Types (MarconiTrace, SecurityParam, TargetAddresses)
import Marconi.Sidechain.Experimental.CLI (
  CliArgs (CliArgs, httpPort, targetAddresses, targetAssets),
 )
import Network.Wai.Handler.Warp (Port, Settings, defaultSettings, setPort)

{- | Sidechain application config, including the Cli arguments,
JSON-RPC server settings and queryable indexers.
-}
data SidechainEnv = SidechainEnv
  { _sidechainCliArgs :: !CliArgs
  , _sidechainQueryEnv :: !SidechainQueryEnv
  -- ^ Server config and query-related values, such as
  -- 'MarconiChainIndexQueryables'
  , _sidechainTrace :: !(MarconiTrace IO)
  }

type TargetAssets = (NonEmpty (C.PolicyId, Maybe C.AssetName))

{- | @Marconi.ChainIndex.Api.Types.'HttpServerConfig'@ augmented with
'TargetAssets'.
-}
data SidechainQueryEnv = SidechainQueryEnv
  { _sidechainQueryEnvHttpSettings :: !HttpServerConfig
  , _sidechainQueryEnvTargetAssets :: !(Maybe TargetAssets)
  }

mkSidechainEnv
  :: SecurityParam
  -> Port
  -> Maybe TargetAddresses
  -> Maybe TargetAssets
  -> CliArgs
  -> MarconiTrace IO
  -> IO SidechainEnv
mkSidechainEnv securityParam httpPort targetAddresses targetAssets cliArgs trace = do
  -- TODO: PLT-8076 decide whether you want to use the chain-index http server stuff
  -- or the sidechain server stuff. likely the former.
  let httpSettings = undefined
  let sidechainQueryEnv = SidechainQueryEnv undefined undefined
  -- TODO: PLT-8076
  -- sidechain <-
  --  SidechainIndexersEnv
  --    <$> mkAddressUtxoIndexerEnv targetAddresses
  --    <*> mkMintEventIndexerEnv targetAssets
  --    <*> mkEpochStateEnv
  -- pure $ SidechainEnv sidechainQueryEnv sidechainIndexers cliArgs trace
  undefined

mkSidechainEnvFromCliArgs
  :: SecurityParam
  -> CliArgs
  -> MarconiTrace IO
  -> IO SidechainEnv
mkSidechainEnvFromCliArgs securityParam cliArgs@CliArgs{httpPort, targetAddresses, targetAssets} trace = do
  mkSidechainEnv securityParam httpPort targetAddresses targetAssets cliArgs trace

makeLenses ''SidechainEnv
makeLenses ''SidechainQueryEnv
