{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Coordinator (tests) where

import Cardano.Api qualified as C

import Cardano.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, readMVar, signalQSemN)
import Control.Concurrent.STM (atomically, dupTChan, readTChan)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Word (Word64)
import Gen.Marconi.ChainIndex.Types (genChainPoint, genChainSyncEvents, genHashBlockHeader)
import Hedgehog (MonadGen, Property, footnote, forAll, property, (===))
import Marconi.ChainIndex.Indexers (Coordinator', barrier, channel, errorVar, failWhenFull, initialCoordinator,
                                    mkIndexerStream')
import Streaming.Prelude qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Coordinator"
    [ testPropertyNamed
        "Don't buffer anything on a 0-size buffer"
        "propEmptyBuffer"
        propEmptyBuffer
    ]

propEmptyBuffer :: Property
propEmptyBuffer = property $ do
    chain <- forAll $ genChainSyncEventsWithChainPoint 10 30
    footnote $ show chain
    c <- lift $ initialCoordinator 1 0
    store <- lift $ newMVar []
    lift $ storeWorker store c
    void $ lift $ mkIndexerStream' getSlotNo c (S.each chain)
    res <- lift $ readMVar store
    resolve chain [] === res

genChainSyncEventsWithChainPoint :: MonadGen m => Word64 -> Word64 -> m [ChainSyncEvent C.ChainPoint]
genChainSyncEventsWithChainPoint lo hi = do
    c <- genChainPoint
    genChainSyncEvents id nextChainPoint c lo hi

nextChainPoint :: MonadGen m => C.ChainPoint -> m C.ChainPoint
nextChainPoint (C.ChainPoint cp _)   = C.ChainPoint (succ cp) <$> genHashBlockHeader
nextChainPoint C.ChainPointAtGenesis = C.ChainPoint 0 <$> genHashBlockHeader

getSlotNo :: C.ChainPoint -> C.SlotNo
getSlotNo (C.ChainPoint s _)    = s
getSlotNo C.ChainPointAtGenesis = C.SlotNo 0

type LocalWorker a = Coordinator' a -> IO ()

storeWorker :: MVar [C.ChainPoint] -> LocalWorker C.ChainPoint
storeWorker store c = do
    workerChannel <- atomically . dupTChan $ c ^. channel
    void . forkIO $ innerLoop workerChannel
    where
        innerLoop ch = do
          signalQSemN (c ^. barrier) 1
          failWhenFull (c ^. errorVar)
          event <- atomically $ readTChan ch
          case event of
            RollForward cp _ct -> do
              modifyMVar_ store (pure . (cp:))
              innerLoop ch
            RollBackward cp _ct -> do
              modifyMVar_ store (pure . dropWhile (> cp))

resolve :: [ChainSyncEvent C.ChainPoint] -> [C.ChainPoint] -> [C.ChainPoint]
resolve [] acc                           = acc
resolve ((RollForward cp _ct) : xs) acc  = resolve xs (cp:acc)
resolve ((RollBackward cp _ct) : xs) acc = resolve xs (dropWhile (< cp) acc)


