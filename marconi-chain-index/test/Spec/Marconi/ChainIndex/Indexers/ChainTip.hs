{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Indexers.ChainTip (
  tests,
) where

import Cardano.Api qualified as C
import Cardano.BM.Data.Tracer qualified as BM
import Control.Exception (throw)
import Control.Lens qualified as Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Hedgehog ((===))
import Hedgehog qualified
import Marconi.ChainIndex.Indexers.ChainTip (ChainTipIndexer)
import Marconi.ChainIndex.Indexers.ChainTip qualified as ChainTip
import Marconi.Core qualified as Core
import System.IO.Temp qualified as Tmp
import Test.Gen.Marconi.ChainIndex.Mockchain qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Indexers.ChainTip"
    [ testGroup
        "Indexer"
        [ testPropertyNamed
            "GetLast works with queryLatest"
            "propGetLastLatest"
            propGetLastAtLatest
        ]
    ]

withIndexer
  :: Gen.MockchainWithInfo C.BabbageEra
  -> ( ChainTipIndexer IO -> ExceptT (Core.QueryError q) IO a
     )
  -> ExceptT (Core.QueryError q) IO a
withIndexer events f = Tmp.withSystemTempDirectory "chainTipIndexer" $ \file -> do
  let config = ChainTip.ChainTipConfig file 1
  indexerE <- runExceptT $ ChainTip.mkChainTipIndexer BM.nullTracer config
  indexer <- case indexerE of
    Left err -> throw err
    Right res -> pure res
  indexer' <- liftIO $ indexMockchain events indexer
  f indexer'

indexMockchain
  :: Gen.MockchainWithInfo C.BabbageEra
  -> ChainTipIndexer IO
  -> IO (ChainTipIndexer IO)
indexMockchain events =
  fmap (either (error . show) id) . Core.indexAllEither (getTimedChainTipEvents events)

propGetLastAtLatest :: Hedgehog.Property
propGetLastAtLatest = Hedgehog.property $ do
  events <- Hedgehog.forAll Gen.genMockchainWithInfo
  (Right res) :: Either (Core.QueryError (Core.GetLastQuery C.ChainTip)) (Maybe C.ChainTip) <-
    liftIO $ runExceptT $ withIndexer events $ Core.queryLatest Core.GetLastQuery
  Lens.view Core.event (last $ getTimedChainTipEvents events) === res

getTimedChainTipEvents
  :: Gen.MockchainWithInfo era
  -> [Core.Timed C.ChainPoint (Maybe C.ChainTip)]
getTimedChainTipEvents =
  let getBlockTimedChainTip block =
        Core.Timed
          (C.chainTipToChainPoint $ Gen.mockBlockInfoChainTip block)
          (Just $ Gen.mockBlockInfoChainTip block)
   in fmap getBlockTimedChainTip
