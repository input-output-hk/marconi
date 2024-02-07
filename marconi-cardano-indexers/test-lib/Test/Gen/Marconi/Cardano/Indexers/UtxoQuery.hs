{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Helpers and generators for testing the aggregate indexer of @Marconi.Cardano.Indexers.UtxoQuery.'UtxoQueryIndexer'@.
module Test.Gen.Marconi.Cardano.Indexers.UtxoQuery where

import Cardano.Api qualified as C
import Control.Concurrent qualified as Concurrent
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hedgehog qualified
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Cardano.Indexers.Datum qualified as Datum
import Marconi.Cardano.Indexers.Spent qualified as Spent
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Cardano.Indexers.UtxoQuery (UtxoQueryEvent)
import Marconi.Cardano.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.Core qualified as Core
import System.FilePath ((</>))
import System.IO.Temp qualified as Tmp
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain
import Test.Gen.Marconi.Cardano.Core.Types qualified as Test.Core
import Test.Gen.Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Test.Gen.Marconi.Cardano.Indexers.Datum qualified as Datum
import Test.Gen.Marconi.Cardano.Indexers.Spent qualified as Spent
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Utxo

{- | Wrapper for a query action that indexes the provided events,
runs the query and closes the indexers.
-}
withIndexer
  :: Mockchain.MockchainWithInfo C.BabbageEra
  -> ( Core.SQLiteAggregateQuery
        IO
        C.ChainPoint
        UtxoQueryEvent
       -> (ExceptT (Core.QueryError UtxoQuery.UtxoQueryInput) IO) a
     )
  -> IO a
withIndexer events f = Tmp.withSystemTempDirectory "testUtxoQuery" $ \dir -> do
  (indexers, utxoQuery) <- liftIO $ mkUtxoQuery dir
  liftIO $ indexMockchain indexers events
  Right res <- liftIO $ runExceptT $ f utxoQuery
  liftIO $ closeIndexers indexers utxoQuery
  pure res

-- | Index the provided events in the individual indexers of 'UtxoQueryIndexers'.
indexMockchain
  :: UtxoQueryIndexers -> Mockchain.MockchainWithInfo C.BabbageEra -> IO ()
indexMockchain
  (UtxoQueryIndexers utxoVar spentVar datumVar blockInfoVar)
  chainWithInfo = do
    let chain = Mockchain.mockchainWithInfoAsMockchain chainWithInfo
    Concurrent.modifyMVar_ utxoVar $
      fmap (either (error . show) id) . Core.indexAllEither (Utxo.getTimedUtxosEvents chain)
    Concurrent.modifyMVar_ spentVar $
      fmap (either (error . show) id) . Core.indexAllEither (Spent.getTimedSpentsEvents chain)
    Concurrent.modifyMVar_ datumVar $
      fmap (either (error . show) id) . Core.indexAllEither (Datum.getTimedDatumsEvents chain)
    Concurrent.modifyMVar_ blockInfoVar $
      fmap (either (error . show) id)
        . Core.indexAllEither (BlockInfo.getTimedBlockInfoEvents chainWithInfo)

closeIndexers
  :: (MonadIO m)
  => UtxoQueryIndexers
  -> Core.SQLiteAggregateQuery m C.ChainPoint UtxoQueryEvent
  -> m ()
closeIndexers
  (UtxoQueryIndexers utxoVar spentVar datumVar blockInfoVar)
  aggregate = do
    liftIO $ Concurrent.withMVar utxoVar Core.close
    liftIO $ Concurrent.withMVar spentVar Core.close
    liftIO $ Concurrent.withMVar datumVar Core.close
    liftIO $ Concurrent.withMVar blockInfoVar Core.close
    Core.close aggregate

{- | Analog to @UtxoQuery.'UtxoQueryAggregate'@ with concrete types,
to facilitate testing.
-}
data UtxoQueryIndexers = UtxoQueryIndexers
  { _utxoIndexer :: Concurrent.MVar Utxo.UtxoIndexer
  , _spentIndexer :: Concurrent.MVar Spent.SpentIndexer
  , _datumIndexer :: Concurrent.MVar Datum.DatumIndexer
  , _blockInfoIndexer :: Concurrent.MVar BlockInfo.BlockInfoIndexer
  }

utxoQueryIndexersAsAggregate :: (MonadIO m) => UtxoQueryIndexers -> UtxoQuery.UtxoQueryAggregate m
utxoQueryIndexersAsAggregate
  (UtxoQueryIndexers utxoIndexer spentIndexer datumIndexer blockInfoIndexer) =
    UtxoQuery.UtxoQueryAggregate
      { UtxoQuery.blockInfoIndexer = blockInfoIndexer
      , UtxoQuery.datumIndexer = datumIndexer
      , UtxoQuery.spentIndexer = spentIndexer
      , UtxoQuery.utxoIndexer = utxoIndexer
      }

mkUtxoQuery
  :: FilePath
  -> IO
      ( UtxoQueryIndexers
      , Core.SQLiteAggregateQuery
          IO
          C.ChainPoint
          UtxoQueryEvent
      )
mkUtxoQuery dir = do
  let blockInfoPath = dir </> "blockInfo.db"
  Right blockInfoIndexer <-
    runExceptT $ BlockInfo.mkBlockInfoIndexer (Core.parseDBLocation blockInfoPath)
  blockInfoVar <- liftIO $ Concurrent.newMVar blockInfoIndexer
  let datumPath = dir </> "datum.db"
  Right datumIndexer <- runExceptT $ Datum.mkDatumIndexer (Core.parseDBLocation datumPath)
  datumVar <- liftIO $ Concurrent.newMVar datumIndexer
  let spentPath = dir </> "spent.db"
  Right spentIndexer <- runExceptT $ Spent.mkSpentIndexer (Core.parseDBLocation spentPath)
  spentVar <- liftIO $ Concurrent.newMVar spentIndexer
  let utxoPath = dir </> "utxo.db"
  Right utxoIndexer <- runExceptT $ Utxo.mkUtxoIndexer (Core.parseDBLocation utxoPath)
  utxoVar <- liftIO $ Concurrent.newMVar utxoIndexer
  let indexers = UtxoQueryIndexers utxoVar spentVar datumVar blockInfoVar
  utxoQuery <- UtxoQuery.mkUtxoSQLiteQuery $ utxoQueryIndexersAsAggregate indexers
  pure (indexers, utxoQuery)

genTimed :: Hedgehog.Gen a -> Hedgehog.Gen (Core.Timed C.ChainPoint a)
genTimed gen = Core.Timed <$> Test.Core.genChainPoint <*> gen

genUtxoResult :: Hedgehog.Gen UtxoQuery.UtxoResult
genUtxoResult =
  UtxoQuery.UtxoResult
    <$> Utxo.genUtxo
    <*> Hedgehog.Gen.maybe (C.getScriptData <$> CGen.genHashableScriptData)
    <*> genTimed BlockInfo.genBlockInfo
    <*> Hedgehog.Gen.maybe (genTimed $ (,) <$> BlockInfo.genBlockInfo <*> CGen.genTxId)
    <*> Hedgehog.Gen.list (Hedgehog.Range.linear 0 10) CGen.genTxIn
