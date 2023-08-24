{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides support for writing handlers for JSON-RPC endpoints.
module Marconi.Sidechain.Env (
  SidechainEnv (..),
  sidechainQueryEnv,
  sidechainIndexersEnv,
  SidechainQueryEnv (..),
  sidechainQueryEnvSecurityParam,
  sidechainQueryEnvHttpSettings,
  SidechainIndexersEnv (..),
  sidechainAddressUtxoIndexer,
  sidechainMintBurnIndexer,
  sidechainEpochStateIndexer,
  AddressUtxoIndexerEnv (..),
  addressUtxoIndexerEnvTargetAddresses,
  addressUtxoIndexerEnvIndexer,
  MintBurnIndexerEnv (..),
  mintBurnIndexerEnvTargetAssets,
  mintBurnIndexerEnvIndexer,
  EpochStateIndexerEnv (..),
  epochStateIndexerEnvIndexer,
  mkSidechainEnvFromCliArgs,
  mkSidechainEnv,
  mkAddressUtxoIndexerEnv,
  mkMintEventIndexerEnv,
  mkEpochStateEnv,
) where

import Cardano.Api qualified as C
import Control.Concurrent.STM (TMVar, newEmptyTMVarIO)
import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle)
import Marconi.ChainIndex.Indexers.MintBurn (MintBurnHandle)
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.ChainIndex.Types (
  SecurityParam,
  TargetAddresses,
 )
import Marconi.Core.Storable (State)
import Marconi.Sidechain.CLI (
  CliArgs (CliArgs, httpPort, targetAddresses, targetAssets),
 )
import Network.Wai.Handler.Warp (Port, Settings, defaultSettings, setPort)

-- | JSON-RPC as well as the Query Indexer Env
data SidechainEnv = SidechainEnv
  { _sidechainQueryEnv :: !SidechainQueryEnv
  , _sidechainIndexersEnv :: !SidechainIndexersEnv
  -- ^ Access the Sidechain indexers for querying purposes.
  }

data SidechainQueryEnv = SidechainQueryEnv
  { _sidechainQueryEnvSecurityParam :: !SecurityParam
  , _sidechainQueryEnvHttpSettings :: !Settings
  -- ^ HTTP server setting
  }

-- | Should contain all the indexers required by Sidechain.
data SidechainIndexersEnv = SidechainIndexersEnv
  { _sidechainAddressUtxoIndexer :: !AddressUtxoIndexerEnv
  -- ^ For query thread to access in-memory utxos
  , _sidechainMintBurnIndexer :: !MintBurnIndexerEnv
  -- ^ For query thread to access in-memory mintBurn
  , _sidechainEpochStateIndexer :: !EpochStateIndexerEnv
  -- ^ For query thread to access in-memory epoch state data
  }

data AddressUtxoIndexerEnv = AddressUtxoIndexerEnv
  { _addressUtxoIndexerEnvTargetAddresses :: !(Maybe TargetAddresses)
  , _addressUtxoIndexerEnvIndexer :: !(TMVar (State UtxoHandle))
  }

data MintBurnIndexerEnv = MintBurnIndexerEnv
  { _mintBurnIndexerEnvTargetAssets :: !(Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)))
  , _mintBurnIndexerEnvIndexer :: !(TMVar (State MintBurnHandle))
  }

newtype EpochStateIndexerEnv = EpochStateIndexerEnv
  { _epochStateIndexerEnvIndexer :: TMVar (State EpochStateHandle)
  }

mkSidechainEnvFromCliArgs
  :: SecurityParam
  -> CliArgs
  -> IO SidechainEnv
mkSidechainEnvFromCliArgs securityParam CliArgs{httpPort, targetAddresses, targetAssets} = do
  mkSidechainEnv securityParam httpPort targetAddresses targetAssets

mkSidechainEnv
  :: SecurityParam
  -> Maybe Port
  -> Maybe TargetAddresses
  -> Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -> IO SidechainEnv
mkSidechainEnv securityParam httpPort targetAddresses targetAssets = do
  let httpSettings = maybe defaultSettings (flip setPort defaultSettings) httpPort
  let sidechainQueryEnv = SidechainQueryEnv securityParam httpSettings
  sidechainIndexers <-
    SidechainIndexersEnv
      <$> mkAddressUtxoIndexerEnv targetAddresses
      <*> mkMintEventIndexerEnv targetAssets
      <*> mkEpochStateEnv
  pure $ SidechainEnv sidechainQueryEnv sidechainIndexers

{- | Bootstraps the utxo query environment.
 The module is responsible for accessing SQLite for queries.
 The main issue we try to avoid here is mixing inserts and queries in SQLite to avoid locking the database.
-}
mkAddressUtxoIndexerEnv
  :: Maybe TargetAddresses
  -- ^ User provided target addresses
  -> IO AddressUtxoIndexerEnv
  -- ^ AddressUtxo Indexer environment
mkAddressUtxoIndexerEnv targetAddresses = do
  ix <- newEmptyTMVarIO
  pure $ AddressUtxoIndexerEnv targetAddresses ix

mkMintEventIndexerEnv
  :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -> IO MintBurnIndexerEnv
  -- ^ MintToken indexer environment
mkMintEventIndexerEnv targets = do
  ix <- newEmptyTMVarIO
  pure $ MintBurnIndexerEnv targets ix

mkEpochStateEnv
  :: IO EpochStateIndexerEnv
  -- ^ EpochState indexer environment
mkEpochStateEnv = EpochStateIndexerEnv <$> newEmptyTMVarIO

makeLenses ''SidechainEnv
makeLenses ''SidechainQueryEnv
makeLenses ''SidechainIndexersEnv
makeLenses ''AddressUtxoIndexerEnv
makeLenses ''MintBurnIndexerEnv
makeLenses ''EpochStateIndexerEnv
