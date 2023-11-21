{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Marconi.ChainIndex.Indexers.MintTokenEventQuery (
  -- * Indexer
  MintTokenEventIndexerQuery (..),
) where

import Cardano.Api qualified as C
import Control.Concurrent qualified as Con
import Control.Lens (view, (^.), _2, _3)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Marconi.Cardano.Core.Indexer.Worker (StandardIndexer)
import Marconi.Cardano.Core.Types (SecurityParam)
import Marconi.ChainIndex.Indexers.BlockInfo (BlockInfo)
import Marconi.ChainIndex.Indexers.BlockInfo qualified as BI
import Marconi.ChainIndex.Indexers.MintTokenEvent (
  MintTokenBlockEvents,
  QueryByAssetId (QueryByAssetId),
  mintTokenEventBlockNo,
  mintTokenEventLocation,
  mintTokenEvents,
 )
import Marconi.Core qualified as Core

{- If at any point we move to another database backend, we might want to consider changing this to
	use SQLiteAggregateQuery to avoid opening more than one connection -}

-- | The inner state of the 'MintTokenIndexerQuery'
data MintTokenEventIndexerQuery event = MintTokenEventIndexerQuery
  { _mintTokenEventIndexerSecurityParam :: !SecurityParam
  , _mintTokenEventIndexerQueryIndexer :: Con.MVar (StandardIndexer IO Core.SQLiteIndexer event)
  , _mintTokenEventIndexerQueryBlockInfo :: Con.MVar (StandardIndexer IO Core.SQLiteIndexer BlockInfo)
  }

instance
  ( MonadIO m
  , Core.Point event ~ C.ChainPoint
  )
  => Core.IsSync m event MintTokenEventIndexerQuery
  where
  lastSyncPoint idx = do
    mintTokenRes <- liftIO $ Core.lastSyncPoint =<< getMintTokenIndexer idx
    blockInfoRes <- liftIO $ Core.lastSyncPoint =<< getBlockInfoIndexer idx
    pure $ min mintTokenRes blockInfoRes
  lastStablePoint idx = do
    mintTokenIdx <- getMintTokenIndexer idx
    liftIO $ Core.lastStablePoint mintTokenIdx

-- | Helper to get the MintTokenEventIndexer from the query indexer
getMintTokenIndexer
  :: (MonadIO m) => MintTokenEventIndexerQuery event -> m (Core.SQLiteIndexer event)
getMintTokenIndexer = fmap (view _2) . unwrapQueryIndexer

-- | Helper to get the BlockInfoIndexer from the query indexer
getBlockInfoIndexer
  :: (MonadIO m) => MintTokenEventIndexerQuery event -> m (Core.SQLiteIndexer BlockInfo)
getBlockInfoIndexer = fmap (view _3) . unwrapQueryIndexer

-- | Helper to get the values (including reading from the @MVar@s) from the query indexer
unwrapQueryIndexer
  :: (MonadIO m)
  => MintTokenEventIndexerQuery event
  -> m (SecurityParam, Core.SQLiteIndexer event, Core.SQLiteIndexer BlockInfo)
unwrapQueryIndexer (MintTokenEventIndexerQuery sp _mintTokenIndexerMv _blockInfoIndexerMv) =
  liftIO $ do
    mintTokenIdxTrans <- Con.readMVar _mintTokenIndexerMv
    blockInfoIdxTrans <- Con.readMVar _blockInfoIndexerMv
    pure
      ( sp
      , mintTokenIdxTrans ^. Core.unwrap . Core.unwrap . Core.unwrapMap
      , blockInfoIdxTrans ^. Core.unwrap . Core.unwrap . Core.unwrapMap
      )

instance
  ( MonadIO m
  , Core.IsSync m MintTokenBlockEvents Core.SQLiteIndexer
  , Core.IsSync
      ( ExceptT
          (Core.QueryError (BI.BlockInfoBySlotNoQuery BlockInfo))
          m
      )
      BlockInfo
      Core.SQLiteIndexer
  , MonadError (Core.QueryError (Core.WithStability (QueryByAssetId MintTokenBlockEvents))) m
  )
  => Core.Queryable
      m
      MintTokenBlockEvents
      (Core.WithStability (QueryByAssetId MintTokenBlockEvents))
      MintTokenEventIndexerQuery
  where
  query p (Core.WithStability q'@(QueryByAssetId _ _ _ upperSlotNo _)) idx = do
    (securityParam, mintTokenIdx, blockInfoIdx) <- unwrapQueryIndexer idx

    let fromError :: Core.QueryError a -> Core.QueryError b
        fromError = \case
          Core.NotStoredAnymore -> Core.NotStoredAnymore
          (Core.IndexerQueryError t) -> Core.IndexerQueryError t
          (Core.AheadOfLastSync _) -> Core.IndexerQueryError "Upper slot no. ahead of last sync"
          (Core.SlotNoBoundsInvalid r) -> Core.SlotNoBoundsInvalid r

    withStab <-
      case upperSlotNo of
        Nothing -> do
          lsp <- Core.lastStablePoint mintTokenIdx
          pure (Core.calcStability (view Core.point) lsp)
        Just slot -> do
          res <- runExceptT $ Core.queryLatest (BI.BlockInfoBySlotNoQuery slot) blockInfoIdx
          case res of
            Right (Just blockInfo) -> do
              let getBlockNo =
                    view (mintTokenEventLocation . mintTokenEventBlockNo)
                      . NonEmpty.head
                      . view (Core.event . mintTokenEvents)
                  lsp = blockInfo ^. BI.blockNo - fromIntegral securityParam
              pure (Core.calcStability getBlockNo lsp)
            Right Nothing ->
              throwError $
                Core.IndexerQueryError $
                  Text.pack $
                    "BlockInfo at slot " <> show slot <> " not found!"
            Left e -> throwError $ fromError e

    Core.withStabilityM withStab $ Core.query p q' mintTokenIdx
