{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Sidechain.Indexers.LedgerEvent (
  -- * Indexer and worker
  AnchoredEventIndexer,
  mkAnchoredEventIndexerIndexer,
  AnchoredEventWorker,
  StandardAnchoredEventIndexer,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Node.LedgerEvent (AnchoredEvents)
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.TH qualified as Aeson
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Marconi.Core.Indexer.SQLiteIndexer qualified as Core
import Marconi.Core.Type qualified as Core

-- import Marconi.Cardano.Core.Indexer.Worker (
--   StandardSQLiteIndexer,
--   StandardWorker,
--   StandardWorkerConfig,
--   mkStandardWorker,
--  )
-- import Marconi.Cardano.Core.Orphans ()
-- import Marconi.ChainIndex.Indexers.SyncHelper qualified as Sync
-- import Marconi.Core qualified as Core

-- we use deriveJSON to drop the underscore prefix
Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = tail} ''AnchoredEvents
Lens.makeLenses ''AnchoredEvents

instance SQL.FromRow (Core.Timed C.ChainPoint AnchoredEvents) where
  fromRow = do
    datumInfo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point datumInfo

instance SQL.ToRow (Core.Timed C.ChainPoint AnchoredEvents) where
  toRow d = SQL.toRow (d ^. Core.event) ++ SQL.toRow (d ^. Core.point)

type instance Core.Point AnchoredEvents = C.ChainPoint

-- | A raw SQLite indexer for Datum
type AnchoredEventIndexer = Core.SQLiteIndexer AnchoredEvents

-- | A SQLite AnchoredEvent indexer with Catchup
type StandardAnchoredEventIndexer m = StandardSQLiteIndexer m AnchoredEvents

-- | A smart constructor for 'AnchoredEventIndexer'
mkAnchoredEventIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.SQLiteIndexer AnchoredEvents)
mkAnchoredEventIndexer path = do
  let createAnchoredEventQuery =
        [sql|CREATE TABLE IF NOT EXISTS anchored_event
             ( blockNo BIGINT
             , ledgerEvent BLOB
             , slotNo BIGINT
             , blockHeaderHash BLOB
						 , prevBlockHeaderHash BLOB
						)|]
      datumInsertQuery :: SQL.Query
      datumInsertQuery =
        [sql|INSERT OR IGNORE INTO anchored_event
             ( blockNo
             , ledgerEvent
             , slotNo
             , blockHeaderHash
						 , prevBlockHeaderHash
             )
          VALUES (?, ?, ?, ?, ?)|]
      createAnchoredEventTables = [createAnchoredEventQuery]
  Sync.mkSyncedSqliteIndexer
    path
    createAnchoredEventTables
    [[Core.SQLInsertPlan (traverse NonEmpty.toList) anchoredEventInsertQuery]]
    [Core.SQLRollbackPlan "anchoredEvent" "slotNo" C.chainPointToSlotNo]

-- | A worker with catchup for a 'AnchoredEventIndexer'
anchoredEventWorker
  :: (MonadIO m, MonadIO n, MonadError Core.IndexerError n)
  => StandardWorkerConfig m input AnchoredEvents
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> FilePath
  -- ^ SQLite database location
  -> n (StandardWorker m input AnchoredEvents Core.SQLiteIndexer)
anchoredEventWorker workerConfig path = do
  indexer <- mkDatumIndexer path
  mkStandardWorker workerConfig indexer
