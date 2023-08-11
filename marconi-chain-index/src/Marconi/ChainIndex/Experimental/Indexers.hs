{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.Streaming (
  BlockEvent (BlockEvent),
 )
import Control.Monad.Except (ExceptT, MonadTrans (lift))
import Data.List.NonEmpty qualified as NonEmpty
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.BlockInfo qualified as Block
import Marconi.ChainIndex.Experimental.Indexers.Coordinator (coordinatorWorker)
import Marconi.ChainIndex.Experimental.Indexers.Datum qualified as Datum
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent qualified as MintToken
import Marconi.ChainIndex.Experimental.Indexers.Spent qualified as Spent
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Experimental.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.ChainIndex.Types (TxIndexInBlock)
import Marconi.Core.Experiment qualified as Core
import System.FilePath ((</>))

data AnyTxBody = forall era. (C.IsCardanoEra era) => AnyTxBody TxIndexInBlock (C.TxBody era)
type instance Core.Point [AnyTxBody] = C.ChainPoint

{- | Build all the indexers of marconi-chain-index
(all those which are available with the new implementation)
and expose a single coordinator to operate them
-}
buildIndexers
  :: Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> FilePath
  -> ExceptT
      Core.IndexerError
      IO
      -- Query part (should probably be wrapped in a more complex object later)
      ( UtxoQuery.UtxoQueryIndexer IO
      , -- Indexing part
        Core.Coordinator (WithDistance BlockEvent)
      )
buildIndexers catchupConfig utxoConfig path = do
  let extractBlockInfo :: BlockEvent -> Block.BlockInfo
      extractBlockInfo (BlockEvent (C.BlockInMode b _) eno t) = Block.fromBlockEratoBlockInfo b eno t
  (blockInfoMVar, blockInfoWorker) <-
    Block.blockInfoWorker "BlockInfo" catchupConfig extractBlockInfo (path </> "blockInfo.db")

  let extractUtxos :: AnyTxBody -> [Utxo.Utxo]
      extractUtxos (AnyTxBody indexInBlock txb) = Utxo.getUtxosFromTxBody indexInBlock txb
  (utxoMVar, utxoWorker) <-
    Utxo.utxoWorker
      "Utxo"
      catchupConfig
      utxoConfig
      (NonEmpty.nonEmpty . (>>= extractUtxos))
      (path </> "utxo.db")

  let extractSpent :: AnyTxBody -> [Spent.SpentInfo]
      extractSpent (AnyTxBody _ txb) = Spent.getInputs txb
  (spentMVar, spentWorker) <-
    Spent.spentWorker
      "Spent"
      catchupConfig
      (NonEmpty.nonEmpty . (>>= extractSpent))
      (path </> "spent.db")

  let extractDatum :: AnyTxBody -> [Datum.DatumInfo]
      extractDatum (AnyTxBody _ txb) = Datum.getDataFromTxBody txb
  (datumMVar, datumWorker) <-
    Datum.datumWorker
      "Datum"
      catchupConfig
      (NonEmpty.nonEmpty . (>>= extractDatum))
      (path </> "datum.db")

  let extractMint :: AnyTxBody -> [MintToken.MintTokenEvent]
      extractMint (AnyTxBody ix txb) = MintToken.extractEventsFromTx ix txb
  (_mintTokenMVar, mintTokenWorker) <-
    MintToken.mintTokenEventWorker
      "MintTokenEvent"
      catchupConfig
      (fmap MintToken.MintTokenEvents . NonEmpty.nonEmpty . (>>= extractMint))
      (path </> "mint.db")

  let getTxBody :: (C.IsCardanoEra era) => TxIndexInBlock -> C.Tx era -> AnyTxBody
      getTxBody ix tx = AnyTxBody ix (C.getTxBody tx)
      toTxBodys :: BlockEvent -> [AnyTxBody]
      toTxBodys (BlockEvent (C.BlockInMode (C.Block _ txs) _) _ _) = zipWith getTxBody [0 ..] txs

  -- Coordinator of a bunch of workers that takes an @AnyTxBody@ as an input
  coordinatorTxBodyWorkers <-
    snd
      <$> coordinatorWorker
        "TxBody coordinator"
        (pure . Just . fmap toTxBodys)
        [utxoWorker, spentWorker, datumWorker, mintTokenWorker]

  queryIndexer <-
    lift $
      UtxoQuery.mkUtxoSQLiteQuery $
        UtxoQuery.UtxoQueryAggregate utxoMVar spentMVar datumMVar blockInfoMVar

  mainCoordinator <- lift $ Core.mkCoordinator [blockInfoWorker, coordinatorTxBodyWorkers]

  pure (queryIndexer, mainCoordinator)
