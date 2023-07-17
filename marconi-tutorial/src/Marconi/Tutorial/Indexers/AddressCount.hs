{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO Make explicit internal modules of marconi-core. Everything expect `Marconi.Core.Experiment`
-- should be in `Marconi.Core.Experiment.Internal.*`.
-- TODO Discenpancy between query of ListIndexer and SqliteIndexer in case where we provide
-- ChainPointAtGenesis. Check test scenario when we provide the ChainPointAtGenesis in the
-- generator.
-- TODO Hard to tell what to provide as Query for singleInsertSQLiteIndexer (especially the lastSync query). Need to change `lastSyncQuery` to `lastResumablePointQuery`.
-- TODO Change back Timed to TimedEvent
module Marconi.Tutorial.Indexers.AddressCount where

import Cardano.Api qualified as C
import Control.Concurrent (MVar)
import Control.Lens (at, folded, sumOf, to, (&), (.~), (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time.Clock.POSIX (POSIXTime)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam))
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Experiment (
  HasGenesis (genesis),
  IndexerError,
  ListIndexer,
  MixedIndexer,
  Point,
  Queryable (query),
  Result,
  SQLInsertPlan (SQLInsertPlan),
  SQLiteIndexer (SQLiteIndexer),
  Timed (Timed),
  event,
  events,
  mkListIndexer,
  mkMixedIndexer,
  mkSqliteIndexer,
  point,
 )
import Marconi.Core.Experiment qualified as Core

addressCountWorker
  :: FilePath
  -> SecurityParam
  -> IO
      ( MVar (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer AddressCountEvent)
      , Core.Worker (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime) C.ChainPoint
      )
addressCountWorker dbPath securityParam = do
  let extract = getEventsFromBlock
  ix <- Utils.toException $ mkAddressCountMixedIndexer dbPath securityParam
  Core.createWorker (pure . extract) ix

newtype AddressCountEvent = AddressCountEvent {unAddressCountEvent :: Map C.AddressAny Int}
  deriving (Show)

type instance Point AddressCountEvent = C.ChainPoint

newtype AddressCountQuery = AddressCountQuery C.AddressAny
  deriving (Eq, Ord, Show)

type instance Result AddressCountQuery = Int

-- Note that for Marconi, it is more optimal to return `Nothing` instead of
-- `Just $ AddressCountEvent mempty`.
getEventsFromBlock :: (C.BlockInMode era, C.EpochNo, POSIXTime) -> Maybe AddressCountEvent
getEventsFromBlock (C.BlockInMode (C.Block _ txs) _, _, _) =
  let addrCounts = Map.fromListWith (+) $ concatMap getEventsFromTx txs
   in if Map.null addrCounts
        then Nothing
        else Just $ AddressCountEvent addrCounts
  where
    getEventsFromTx :: C.Tx era -> [(C.AddressAny, Int)]
    getEventsFromTx (C.Tx (C.TxBody C.TxBodyContent{..}) _) =
      fmap (\(C.TxOut (C.AddressInEra _ addr) _ _ _) -> (C.toAddressAny addr, 1)) txOuts

{- | Query the in-memory indexer

TODO The 'Point AddressCountEvent' param is not used in this query yet.
That parameter is necessary in the scenario where there is more than one indexer and each run at different speeds.
Marconi uses that parameter to make sure that the result are consistent accross the different query calls.
-}
instance (Monad m) => Queryable m AddressCountEvent AddressCountQuery ListIndexer where
  query
    :: Point AddressCountEvent -- give me what you know up to this point, potentially a point in future
    -> AddressCountQuery
    -> ListIndexer AddressCountEvent -- get the point for ListIndexer
    -> m (Result AddressCountQuery)
  query C.ChainPointAtGenesis _ _ = pure 0
  query (C.ChainPoint _ _) (AddressCountQuery addr) lsIndexer = do
    pure $
      sumOf
        ( events
            . traverse
            . event
            . to unAddressCountEvent
            . at addr
            . folded
        )
        lsIndexer

-- * SQLite indexer

{- | Query the in-memory indexer
 TODO The 'Point AddressCountEvent' param should be removed or at the very least really make a
 valid point on why we need it
-}
instance (MonadIO m) => Queryable m AddressCountEvent AddressCountQuery SQLiteIndexer where
  query
    :: Point AddressCountEvent -- give me what you know up to this point, potentially a point in future
    -> AddressCountQuery
    -> SQLiteIndexer AddressCountEvent -- get the point for ListIndexer
    -> m (Result AddressCountQuery)
  query C.ChainPointAtGenesis _ _ = pure 0
  query (C.ChainPoint _ _) (AddressCountQuery addr) (SQLiteIndexer c _ _) = do
    (results :: [[Int]]) <-
      liftIO $
        SQL.query
          c
          [sql|SELECT count FROM address_count
               WHERE address = ?
               LIMIT 10|]
          (SQL.Only addr)
    pure $ sum $ concat results

data AddressCountRow = AddressCountRow
  { addressCountRowAddr :: !C.AddressAny
  , addressCountRowCount :: !Int
  }
  deriving (Generic, SQL.ToRow)

instance SQL.ToRow (Timed C.ChainPoint AddressCountRow) where
  toRow u =
    let (AddressCountRow addr count) = u ^. event
        (snoField, bhhField) = case u ^. point of
          C.ChainPointAtGenesis -> (SQL.SQLNull, SQL.SQLNull)
          (C.ChainPoint sno bhh) -> (SQL.toField sno, SQL.toField bhh)
     in SQL.toRow
          ( SQL.toField addr
          , SQL.toField count
          , snoField
          , bhhField
          )

eventToRows :: Timed C.ChainPoint AddressCountEvent -> [Timed C.ChainPoint AddressCountRow]
eventToRows te =
  let addrCounts = Map.toList $ unAddressCountEvent $ te ^. event
      cp = te ^. point
   in case cp of
        C.ChainPointAtGenesis ->
          []
        C.ChainPoint _ _ ->
          fmap (\(addr, count) -> Timed cp (AddressCountRow addr count)) addrCounts

instance HasGenesis C.ChainPoint where
  genesis = C.ChainPointAtGenesis

mkAddressCountSqliteIndexer
  :: (MonadIO m, MonadError IndexerError m)
  => FilePath
  -> m (SQLiteIndexer AddressCountEvent)
mkAddressCountSqliteIndexer dbPath = do
  c <- liftIO $ SQL.open dbPath

  liftIO $
    SQL.execute_
      c
      [sql|CREATE TABLE IF NOT EXISTS address_count
                 ( address BLOB NOT NULL
                 , count INT NOT NULL
                 , slotNo INT NOT NULL
                 , blockHeaderHash BLOB
                 )|]

  mkSqliteIndexer
    c
    [
      [ SQLInsertPlan eventToRows addressCountInsertQuery
      ]
    ]
    lastSyncQuery
  where
    addressCountInsertQuery :: SQL.Query -- Utxo table SQL statement
    addressCountInsertQuery =
      [sql|INSERT
               INTO address_count (
                 address,
                 count,
                 slotNo,
                 blockHeaderHash
              ) VALUES (?, ?, ?, ?)|]

    lastSyncQuery :: SQL.Query -- Utxo table SQL statement
    lastSyncQuery =
      [sql|SELECT slotNo, blockHeaderHash
               FROM address_count
               ORDER BY slotNo DESC
               LIMIT 1|]

-- | Make a SQLiteIndexer
mkAddressCountMixedIndexer
  :: (MonadIO m, MonadError IndexerError m)
  => FilePath
  -- ^ SQLite DB path
  -> SecurityParam
  -- ^ We use securityParam to set the Indexers flush sise to database
  -> m (MixedIndexer SQLiteIndexer ListIndexer AddressCountEvent)
mkAddressCountMixedIndexer dbPath (SecurityParam w64) =
  let keepInMemory = fromIntegral w64 -- how much to keep in memory to minimize disk rollbacks
      flushSize = keepInMemory `div` 6 -- how often to flush to disk
   in mkAddressCountMixedIndexer' dbPath keepInMemory flushSize

-- | Make a SQLiteIndexer
mkAddressCountMixedIndexer'
  :: (MonadIO m, MonadError IndexerError m)
  => FilePath
  -- ^ SQLite DB path
  -> Word
  -- ^  events keept in memory post flush
  -> Word
  -- ^ flush size
  -> m (MixedIndexer SQLiteIndexer ListIndexer AddressCountEvent)
mkAddressCountMixedIndexer' dbPath keep flush = do
  sqliteIndexer <- mkAddressCountSqliteIndexer dbPath
  pure $ mkMixedIndexer keep flush sqliteIndexer mkListIndexer

instance (MonadIO m) => Core.Rollbackable m AddressCountEvent Core.SQLiteIndexer where
  rollback C.ChainPointAtGenesis ix = do
    let c = ix ^. Core.handle
    liftIO $ SQL.execute_ c "DELETE FROM address_count"
    pure $ ix & Core.dbLastSync .~ C.ChainPointAtGenesis
  rollback p@(C.ChainPoint sno _) ix = do
    let c = ix ^. Core.handle
    liftIO $ SQL.execute c "DELETE FROM address_count WHERE slotNo > ?" (SQL.Only sno)
    pure $ ix & Core.dbLastSync .~ p
