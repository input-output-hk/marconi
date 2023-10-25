{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.Marconi.ChainIndex.Legacy.CLI qualified as CLI
import Spec.Marconi.ChainIndex.Legacy.Coordinator qualified as Coordinator
import Spec.Marconi.ChainIndex.Legacy.Indexers.AddressDatum qualified as Indexers.AddressDatum
import Spec.Marconi.ChainIndex.Legacy.Indexers.ScriptTx qualified as Indexers.ScriptTx
import Spec.Marconi.ChainIndex.Legacy.Logging qualified as Logging

-- TODO see tests below
-- import Spec.Marconi.ChainIndex.Legacy.Indexers.EpochStakepoolSize qualified as Indexers.EpochStakepoolSize
import Spec.Marconi.ChainIndex.Legacy.Indexers.MintBurn qualified as Indexers.MintBurn
import Spec.Marconi.ChainIndex.Legacy.Indexers.Utxo qualified as Indexers.Utxo
import Spec.Marconi.ChainIndex.Legacy.Orphans qualified as Orphans

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi"
    [ Orphans.tests
    , CLI.tests
    , Logging.tests
    , Coordinator.tests
    , Indexers.Utxo.tests
    , Indexers.MintBurn.tests
    , Indexers.AddressDatum.tests
    , Indexers.ScriptTx.tests
    -- TODO Enable when test environemnt is reconfigured
    -- , EpochStakepoolSize.tests
    ]
