{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | An Indexer that stores BlockInfo
module Marconi.ChainIndex.Experimental.Indexers.BlockInfo (
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
import Control.Concurrent (MVar)
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
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardSQLiteIndexer,
  StandardWorkerConfig,
  catchupWorker,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Experiment qualified as Core

data BlockInfo = BlockInfo
  { _blockNo :: !C.BlockNo
  , _timestamp :: !Word64
  , _epochNo :: !C.EpochNo
  }
  deriving (Eq, Show, Ord, Generic, SQL.FromRow, SQL.ToRow)

-- we use deriveJSON to drop the underscore prefix
Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = tail} ''BlockInfo

Lens.makeLenses ''BlockInfo

type instance Core.Point BlockInfo = C.ChainPoint

-- | A raw SQLite indexer for BlackInfo
type BlockInfoIndexer = Core.SQLiteIndexer BlockInfo

-- | A SQLite BlockInfo indexer with Catchup
type StandardBlockInfoIndexer m = StandardSQLiteIndexer m BlockInfo

instance SQL.ToRow (Core.Timed C.ChainPoint BlockInfo) where
  toRow b =
    SQL.toRow
      [ SQL.toField $ b ^. Core.point . Lens.to C.chainPointToSlotNo
      , SQL.toField $ b ^. Core.point . Lens.to C.chainPointToHeaderHash
      , SQL.toField $ b ^. Core.event . blockNo
      , SQL.toField $ b ^. Core.event . timestamp
      , SQL.toField $ b ^. Core.event . epochNo
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
  let createBlockInfo =
        [sql|CREATE TABLE IF NOT EXISTS blockInfo
               ( slotNo INT PRIMARY KEY
               , blockHeaderHash BLOB NOT NULL
               , blockNo INT NOT NULL
               , blockTimestamp INT NOT NULL
               , epochNo INT NOT NULL
               )|]
      blockInfoInsertQuery :: SQL.Query
      blockInfoInsertQuery =
        [sql|INSERT INTO blockInfo (slotNo, blockHeaderHash, blockNo, blockTimestamp, epochNo)
             VALUES (?, ?, ?, ?, ?)|]
      lastPointQuery :: Core.GetLastSyncQuery
      lastPointQuery =
        Core.GetLastSyncQuery
          [sql|SELECT slotNo, blockHeaderHash FROM blockInfo ORDER BY slotNo DESC LIMIT 1|]
  Core.mkSingleInsertSqliteIndexer
    path
    id
    createBlockInfo
    blockInfoInsertQuery
    (Core.SQLRollbackPlan "blockInfo" "slotNo" C.chainPointToSlotNo)
    lastPointQuery

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
  -> n
      (MVar (StandardBlockInfoIndexer m), Core.WorkerM m (WithDistance input) (Core.Point BlockInfo))
blockInfoWorker config path = do
  indexer <- mkBlockInfoIndexer path
  catchupWorker config indexer

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

fromBlockEratoBlockInfo
  :: C.Block era -> C.EpochNo -> POSIXTime -> BlockInfo
fromBlockEratoBlockInfo (C.Block (C.BlockHeader _ _ blockNo') _) epochNo' posixTime =
  let (blockTimeStampSeconds, _) = properFraction $ Time.nominalDiffTimeToSeconds posixTime
   in BlockInfo blockNo' blockTimeStampSeconds epochNo'
