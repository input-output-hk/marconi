{-# LANGUAGE NumericUnderscores #-}

{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Cardano.Api qualified as C
import Control.Concurrent (readMVar, threadDelay, withMVar)
import Control.Exception (finally, throwIO)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.List.NonEmpty qualified as NEList
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Indexers qualified as Indexers
import Marconi.Cardano.Indexers.SyncHelper qualified as SyncHelper
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Cardano.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types qualified as ChainIndex
import Marconi.ChainIndex.Api.Types qualified as ChainIndex
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo qualified as Sidechain
import Marconi.Sidechain.Experimental.Api.Types qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Marconi.Sidechain.Experimental.Env qualified as Sidechain
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
import Test.Gen.Marconi.Cardano.Indexers qualified as Test.Indexers
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo
import Test.Gen.Marconi.Cardano.Indexers.UtxoQuery qualified as Test.UtxoQuery
import Test.Helpers qualified
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (
  HedgehogShrinkLimit (HedgehogShrinkLimit),
  HedgehogTestLimit (HedgehogTestLimit),
  testPropertyNamed,
 )

tests :: TestTree
tests =
  -- TODO: PLT-8634 issue with too many open files if run for the default number.
  -- need the tmp directory to clean up, but it seems workspace does not actually do that
  -- between runs. possible solution is to pull the generation outside.
  localOption (HedgehogTestLimit $ Just 10) $
    localOption (HedgehogShrinkLimit $ Just 1) $
      testGroup
        "Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo"
        [ testPropertyNamed
            "Handler returns Utxos from queried address"
            "propQueryTargetAddresses"
            propQueryTargetAddresses
        ]

-- TODO: PLT-8634 change test name to be more descriptive
propQueryTargetAddresses :: Hedgehog.Property
propQueryTargetAddresses = Hedgehog.property $ Test.Helpers.workspace "." $ \tmp -> do
  events <- Hedgehog.forAll Test.Mockchain.genMockchainWithInfoAndDistance

  -- TODO: PLT-8634 ensure the result is nonempty or otherwise deal with this
  -- Select a single address from the sampled events
  let
    utxoEvents@(e : _) =
      Test.Utxo.getTimedUtxosEventsWithDistance $
        Test.Mockchain.mockchainWithInfoAsMockchainWithDistance events

  addr <- Hedgehog.forAll $ Hedgehog.Gen.element (Utils.addressesFromTimedUtxoEvent e)

  let
    args =
      Utils.initTestingCliArgs
        { CLI.targetAddresses = Utils.addressAnysToTargetAddresses [addr]
        , CLI.dbDir = tmp
        }
    -- Serialise to Bech32-format string as required by handler
    addrString = Text.unpack $ C.serialiseAddress addr

  -- Make the http and build indexers configs (with indexers exposed)
  -- just as you would with the sidechain app.
  (httpConfig, indexersConfig) <- Utils.mkTestSidechainConfigsFromCliArgs args

  -- Index the events directly
  Hedgehog.evalIO $ Test.Indexers.indexAllWithMockchain indexersConfig events

  Hedgehog.evalIO $ threadDelay 1_000_000

  let
    params = ChainIndex.GetUtxosFromAddressParams addrString Nothing Nothing
    -- TODO: PLT8634 Params for direct query
    query = UtxoQuery.UtxoQueryInput addr Nothing Nothing

  -- Query via JSON-RPC handler
  res <-
    Hedgehog.evalIO $
      flip runReaderT httpConfig . runExceptT $
        Sidechain.getPastAddressUtxoHandler params

  actual <- Hedgehog.evalIO $ either throwIO pure res >>= either (fail . show) pure

  let
    expected = mapMaybe ((\(WithDistance _ x) -> x) . (^. Core.event)) utxoEvents

  -- TODO: PLT-8634 trying direct query as in handler
  let indexerUtxoQuery = indexersConfig ^. Test.Indexers.testBuildIndexersResultQueryables . Indexers.queryableUtxo

  Right lastSyncPoint <- Hedgehog.evalIO $ runExceptT $ Core.lastSyncPoint indexerUtxoQuery
  Right lastStablePoint <- Hedgehog.evalIO $ runExceptT $ Core.lastStablePoint indexerUtxoQuery

  -- Analogous to 'Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.getUtxoQueryInputHandler'
  -- for querying a single address.
  directRes <-
    Hedgehog.evalIO $
      runExceptT (Core.queryEither lastSyncPoint query indexerUtxoQuery)
        >>= either undefined pure
        >>= either undefined pure

  -- TODO: PLT-8634 direct query on utxo db
  indexerUtxo <-
    Hedgehog.evalIO $
      readMVar $
        indexersConfig ^. Test.Indexers.testBuildIndexersResultUtxo

  -- TODO: PLT-8634 old code for querying raw utxo indexer
  allUtxo :: [Core.Timed C.ChainPoint Utxo.UtxoEvent] <-
    Hedgehog.evalIO $
      runExceptT (Core.queryLatest (Core.EventsMatchingQuery Just) indexerUtxo) >>= either throwIO pure

  -- TODO: PLT-8634
  -- Hedgehog.evalIO $ do
  --  putStrLn "Tmp dir: " >> print tmp
  --  putStrLn "Last sync: " >> print lastSyncPoint
  --  putStrLn "Last stable: " >> print lastStablePoint
  --  putStrLn "Generated events: " >> print events
  --  putStrLn "Uniformized direct UtxoQuery result: "
  --    >> print (map (\x -> (x ^. UtxoQuery.utxo . Utxo.txIn, x ^. UtxoQuery.utxo . Utxo.value)) directRes)
  --  -- putStrLn "Actual raw: " >> print actual
  --  -- putStrLn "Expected raw: " >> print expected
  --  putStrLn "Addr string: " >> print addrString
  --  putStrLn "Query from Utxo db directly: " >> print allUtxo
  -- putStrLn "Uniformized direct query result: "
  --  >> print (map (fmap (map (\x -> (x ^. Utxo.txIn, x ^. Utxo.value)) . NEList.toList)) allUtxo)

  Hedgehog.evalIO $ Test.Indexers.closeIndexers indexersConfig

  uncurry (===) $
    Utils.uniformGetUtxosFromAddressResult addr actual expected
