{- | Run the Marconi and cardano-db-sync comparison by:

1. Sync up cardano-db-sync

2. Run the EpochState indexer up to sync, possibly using the
   cardano-node from the cardano-db-sync docker

3. Run this test by setting the env variables:

     - CARDANO_NODE_SOCKET_PATH
     - CARDANO_NODE_CONFIG_PATH
     - DBSYNC_PG_URL
     - MARCONI_DB_DIRECTORY_PATH
     - NETWORK_MAGIC

   And then run the command:

@
   cabal test marconi-chain-index-test-compare-cardano-db-sync --flag '-ci'
@

   The --flag '-ci' is there to unset the "ci" cabal flag which is on
   by default as we don't want to run it on CI.
-}
module Main where

import EpochState qualified
import Utxo qualified

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi to cardano-db-sync comparisons"
    [ EpochState.tests
    , Utxo.tests
    ]
