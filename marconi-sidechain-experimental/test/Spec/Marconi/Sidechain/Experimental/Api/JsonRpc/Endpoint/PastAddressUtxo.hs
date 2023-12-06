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
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
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

-- TODO: PLT8634 remove the option setting

tests :: TestTree
tests =
  -- TODO: PLT-8634 investigating issue with open files
  -- localOption (HedgehogTestLimit $ Just 1) $
  localOption (HedgehogShrinkLimit $ Just 5) $
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

  -- TODO: PLT-8634
  addr <- Hedgehog.forAll $ Hedgehog.Gen.element (Utils.addressesFromTimedUtxoEvent e)
  -- Hedgehog.forAll $ do
  --  addrs <-
  --    Utils.addressesFromTimedUtxoEvent <$> Hedgehog.Gen.element utxoEvents
  --  Hedgehog.Gen.element addrs

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

  -- Index the utxo events directly
  Hedgehog.evalIO $ Test.Indexers.indexAllWithMockchain indexersConfig events

  Hedgehog.evalIO $ threadDelay 1_000_000

  -- TODO: PLT-8634 add coverage
  let
    params = ChainIndex.GetUtxosFromAddressParams addrString Nothing Nothing

  res <-
    Hedgehog.evalIO $
      flip runReaderT httpConfig . runExceptT $
        Sidechain.getPastAddressUtxoHandler params

  -- TODO: PLT-8634 manually close the indexers needed?
  actual <- Hedgehog.evalIO $ either throwIO pure res >>= either (fail . show) pure

  let
    expected = mapMaybe ((\(WithDistance _ x) -> x) . (^. Core.event)) utxoEvents

  -- TODO: PLT-8634 trying direct query
  indexer <-
    Hedgehog.evalIO $
      readMVar $
        indexersConfig ^. Test.Indexers.testBuildIndexersResultUtxo

  allUtxo :: [Core.Timed C.ChainPoint Utxo.UtxoEvent] <-
    Hedgehog.evalIO $
      runExceptT (Core.queryLatest (Core.EventsMatchingQuery Just) indexer) >>= either throwIO pure

  -- TODO: PLT-8634
  Hedgehog.evalIO $ do
    putStrLn "Tmp dir: " >> print tmp
    putStrLn "Actual raw: " >> print actual
    putStrLn "Expected raw: " >> print expected
    putStrLn "Addr raw: " >> print addr
    putStrLn "Query from Utxo db directly: " >> print allUtxo
    -- TODO: PLT-8634 compare this to the expected result from hedgehog
    putStrLn "Uniformized direct query result: "
      >> print (map (fmap (map (\x -> (x ^. Utxo.txIn, x ^. Utxo.value)) . NEList.toList)) allUtxo)

  uncurry (===) $
    Utils.uniformGetUtxosFromAddressResult addr actual expected
