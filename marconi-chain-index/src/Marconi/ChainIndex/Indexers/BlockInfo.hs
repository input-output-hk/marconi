{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | An Indexer that stores BlockInfo
module Marconi.ChainIndex.Indexers.BlockInfo (
  -- * Event
  BlockInfo (BlockInfo),
  blockNo,
  timestamp,
  epochNo,

  -- * Indexer and worker
  BlockInfoIndexer,
  mkBlockInfoIndexer,
  blockInfoWorker,
  StandardBlockInfoIndexer,

  -- * Extractor
  fromBlockEratoBlockInfo,
) where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError)
import Data.Aeson.TH qualified as Aeson
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers.Orphans ()
import Marconi.ChainIndex.Indexers.SyncHelper (mkSingleInsertSyncedSqliteIndexer)
import Marconi.ChainIndex.Indexers.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig,
  mkStandardWorker,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.Core qualified as Core

data BlockInfo = BlockInfo
  { _blockNo :: !C.BlockNo
  , _timestamp :: !Word64
  , _epochNo :: !C.EpochNo
  }
  deriving (Eq, Show, Ord, Generic, SQL.FromRow)

-- we use deriveJSON to drop the underscore prefix
Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = tail} ''BlockInfo

Lens.makeLenses ''BlockInfo

type instance Core.Point BlockInfo = C.ChainPoint

-- | A raw SQLite indexer for BlackInfo
type BlockInfoIndexer = Core.SQLiteIndexer BlockInfo

-- | A SQLite BlockInfo indexer with Catchup
type StandardBlockInfoIndexer m = StandardSQLiteIndexer m BlockInfo

instance SQL.ToRow (Core.Timed C.ChainPoint BlockInfo) where
  toRow b = SQL.toRow (b ^. Core.point) ++ SQL.toRow (b ^. Core.event)

instance SQL.ToRow BlockInfo where
  toRow b =
    SQL.toRow
      [ SQL.toField $ b ^. blockNo
      , SQL.toField $ b ^. timestamp
      , SQL.toField $ b ^. epochNo
      ]

instance SQL.FromRow (Core.Timed C.ChainPoint BlockInfo) where
  fromRow = do
    blockInfo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point blockInfo

-- | A smart constructor for BlockInfoIndexer
mkBlockInfoIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -- ^ SQL connection to database
  -> m (Core.SQLiteIndexer BlockInfo)
mkBlockInfoIndexer path = do
  let createBlockInfoTable =
        [sql|CREATE TABLE IF NOT EXISTS blockInfo
               ( slotNo INT NOT NULL
               , blockHeaderHash BLOB NOT NULL
               , blockNo INT NOT NULL
               , blockTimestamp INT NOT NULL
               , epochNo INT NOT NULL
               , PRIMARY KEY (slotNo, blockHeaderHash)
               )|]
      blockInfoInsertQuery :: SQL.Query
      blockInfoInsertQuery =
        [sql|INSERT INTO blockInfo (slotNo, blockHeaderHash, blockNo, blockTimestamp, epochNo)
             VALUES (?, ?, ?, ?, ?)|]
  mkSingleInsertSyncedSqliteIndexer
    path
    id
    createBlockInfoTable
    blockInfoInsertQuery
    (Core.SQLRollbackPlan "blockInfo" "slotNo" C.chainPointToSlotNo)

-- | Create a worker for 'BlockInfoIndexer' with catchup
blockInfoWorker
  :: ( MonadIO n
     , MonadError Core.IndexerError n
     , MonadIO m
     )
  => StandardWorkerConfig m input BlockInfo
  -- ^ General indexer configuration
  -> FilePath
  -- ^ SQLite database location
  -> n (StandardWorker m input BlockInfo Core.SQLiteIndexer)
blockInfoWorker config path = do
  indexer <- mkBlockInfoIndexer path
  mkStandardWorker config indexer

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery BlockInfo)) m)
  => Core.Queryable m BlockInfo (Core.EventAtQuery BlockInfo) Core.SQLiteIndexer
  where
  query =
    let blockInfoBySlotNoQuery :: SQL.Query
        blockInfoBySlotNoQuery =
          [sql|
          SELECT blockNo, blockTimestamp, epochNo
          FROM blockInfo
          WHERE slotNo == :slotNo
          LIMIT 1
          |]
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const blockInfoBySlotNoQuery)
          (const listToMaybe)

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventsMatchingQuery BlockInfo)) m)
  => Core.Queryable m BlockInfo (Core.EventsMatchingQuery BlockInfo) Core.SQLiteIndexer
  where
  query =
    let blockInfoBeforeOrAtSlotNoQuery :: SQL.Query
        blockInfoBeforeOrAtSlotNoQuery =
          [sql|
          SELECT blockNo, blockTimestamp, epochNo,
                 slotNo, blockHeaderHash
          FROM blockInfo
          WHERE slotNo <= :slotNo
          |]

        parseResult
          :: (a -> Maybe a)
          -> [Core.Timed C.ChainPoint a]
          -> [Core.Timed C.ChainPoint a]
        parseResult = mapMaybe . traverse
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const blockInfoBeforeOrAtSlotNoQuery)
          (\(Core.EventsMatchingQuery p) -> parseResult p)

instance
  (MonadIO m, MonadError (Core.QueryError (Core.LatestEventsQuery BlockInfo)) m)
  => Core.Queryable m BlockInfo (Core.LatestEventsQuery BlockInfo) Core.SQLiteIndexer
  where
  query =
    let blockInfoBeforeOrAtSlotNoQuery :: SQL.Query
        blockInfoBeforeOrAtSlotNoQuery =
          [sql|
          SELECT blockNo, blockTimestamp, epochNo,
                 slotNo, blockHeaderHash
          FROM blockInfo
          WHERE slotNo <= :slotNo
          ORDER BY slotNo DESC
          LIMIT :n
          |]
     in Core.querySyncedOnlySQLiteIndexerWith
          ( \cp (Core.LatestEventsQuery n) ->
              [":slotNo" := C.chainPointToSlotNo cp, ":n" := n]
          )
          (const blockInfoBeforeOrAtSlotNoQuery)
          (const id)

fromBlockEratoBlockInfo
  :: C.Block era -> C.EpochNo -> POSIXTime -> BlockInfo
fromBlockEratoBlockInfo (C.Block (C.BlockHeader _ _ blockNo') _) epochNo' posixTime =
  let (blockTimeStampSeconds, _) = properFraction $ Time.nominalDiffTimeToSeconds posixTime
   in BlockInfo blockNo' blockTimeStampSeconds epochNo'