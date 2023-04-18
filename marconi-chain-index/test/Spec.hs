{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (lookupEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.Marconi.ChainIndex.CLI qualified as CLI
import Spec.Marconi.ChainIndex.Indexers.AddressDatum qualified as Indexers.AddressDatum
import Spec.Marconi.ChainIndex.Indexers.ScriptTx qualified as Indexers.ScriptTx
-- TODO see tests below
-- import Spec.Marconi.ChainIndex.Indexers.EpochStakepoolSize qualified as Indexers.EpochStakepoolSize
import CompareToDbSync qualified
import Spec.Marconi.ChainIndex.Indexers.MintBurn qualified as Indexers.MintBurn
import Spec.Marconi.ChainIndex.Indexers.Utxo qualified as Indexers.Utxo
import Spec.Marconi.ChainIndex.Orphans qualified as Orphans

main :: IO ()
main = do
  -- Run comparison tests when DBSYNC_COMPARE env variable is set.
  mb <- lookupEnv "DBSYNC_COMPARE"
  defaultMain $ case mb of
    Just _  -> CompareToDbSync.tests
    Nothing -> tests

tests :: TestTree
tests = testGroup "Marconi"
  [ Orphans.tests
  , Indexers.Utxo.tests
  , Indexers.MintBurn.tests
  , Indexers.AddressDatum.tests
  , Indexers.ScriptTx.tests
  , CLI.tests
  -- TODO Enable when test environemnt is reconfigured
  -- , EpochStakepoolSize.tests
  ]
