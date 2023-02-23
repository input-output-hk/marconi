{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.Marconi.ChainIndex.CLI qualified as CLI
import Spec.Marconi.ChainIndex.Indexers.AddressDatum qualified as Indexers.AddressDatum
import Spec.Marconi.ChainIndex.Indexers.ScriptTx qualified as Indexers.ScriptTx
-- TODO see tests below
-- import Spec.Marconi.ChainIndex.Indexers.EpochStakepoolSize qualified as Indexers.EpochStakepoolSize
import Integration qualified
import Spec.Marconi.ChainIndex.Indexers.MintBurn qualified as Indexers.MintBurn
import Spec.Marconi.ChainIndex.Indexers.Utxo qualified as Indexers.Utxo

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Marconi"
  [ Indexers.Utxo.tests
  , Indexers.ScriptTx.tests
  , Indexers.AddressDatum.tests
  , Indexers.MintBurn.tests
  , CLI.tests
  , Integration.tests
  -- TODO Enable when test environemnt is reconfigured
  -- , EpochStakepoolSize.tests
  ]