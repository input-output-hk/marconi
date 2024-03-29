{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | An Indexer that stores BlockInfo
module Marconi.Cardano.Indexers.BlockInfo (
  -- * Event
  BlockInfo (BlockInfo),
  blockNo,
  timestamp,
  epochNo,

  -- * Indexer and worker
  BlockInfoIndexer,
  mkBlockInfoIndexer,
  blockInfoWorker,
  blockInfoBuilder,
  StandardBlockInfoIndexer,
  catchupConfigEventHook,

  -- * Extractor
  fromBlockEratoBlockInfo,
  extractBlockInfo,

  -- * Query
  BlockInfoBySlotNoQuery (..),

  -- * SQL helpers
  dbName,
) where

import Cardano.Api qualified as C
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Tracing qualified as BM
import Control.Lens ((&), (?~), (^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.TH qualified as Aeson
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig (StandardWorkerConfig),
  mkStandardWorker,
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (BlockEvent (BlockEvent), SecurityParam)
import Marconi.Cardano.Indexers.SyncHelper (mkSingleInsertSyncedSqliteIndexer)
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer (SQLiteDBLocation)
import System.FilePath ((</>))

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

dbName :: String
dbName = "blockInfo.db"

-- | A smart constructor for BlockInfoIndexer
mkBlockInfoIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => SQLiteDBLocation
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

catchupConfigEventHook :: Text -> Trace IO Text -> FilePath -> Core.CatchupEvent -> IO ()
catchupConfigEventHook indexerName stdoutTrace dbPath Core.Synced = do
  SQL.withConnection dbPath $ \c -> do
    let slotNoIndexName = "blockInfo__slotNo"
        createSlotNoIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString slotNoIndexName
            <> " ON blockInfo (slotNo)"
    Core.createIndexTable indexerName stdoutTrace c slotNoIndexName createSlotNoIndexStatement

-- | Create a worker for 'BlockInfoIndexer' with catchup
blockInfoWorker
  :: ( MonadIO n
     , MonadError Core.IndexerError n
     , MonadIO m
     )
  => StandardWorkerConfig m input BlockInfo
  -- ^ General indexer configuration
  -> SQLiteDBLocation
  -- ^ SQLite database location
  -> n (StandardWorker m input BlockInfo Core.SQLiteIndexer)
blockInfoWorker config path = do
  indexer <- mkBlockInfoIndexer path
  mkStandardWorker config indexer

{- | Convenience wrapper around 'blockInfoWorker' with some defaults for
creating 'StandardWorkerConfig', including a preprocessor.
-}
blockInfoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n (StandardWorker IO BlockEvent BlockInfo Core.SQLiteIndexer)
blockInfoBuilder securityParam catchupConfig textLogger path =
  let indexerName = "BlockInfo"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      blockInfoDbPath = path </> dbName
      catchupConfigWithTracer =
        catchupConfig
          & Core.configCatchupEventHook
            ?~ catchupConfigEventHook indexerName textLogger blockInfoDbPath
      blockInfoWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfigWithTracer
          (pure . Just . extractBlockInfo)
          (BM.appendName indexerName indexerEventLogger)
   in blockInfoWorker blockInfoWorkerConfig (Core.parseDBLocation (path </> dbName))

type instance Core.Result (BlockInfoBySlotNoQuery event) = Maybe event

newtype BlockInfoBySlotNoQuery event = BlockInfoBySlotNoQuery C.SlotNo

lastBlockInfoQuery :: SQL.Query
lastBlockInfoQuery =
  [sql|
  SELECT blockNo, blockTimestamp, epochNo
  FROM blockInfo
  ORDER BY slotNo DESC
  LIMIT 1
  |]

blockInfoBySlotNoQuery :: SQL.Query
blockInfoBySlotNoQuery =
  [sql|
  SELECT blockNo, blockTimestamp, epochNo
  FROM blockInfo
  WHERE slotNo == :slotNo
  LIMIT 1
  |]

instance
  (MonadIO m, MonadError (Core.QueryError (BlockInfoBySlotNoQuery BlockInfo)) m)
  => Core.Queryable m BlockInfo (BlockInfoBySlotNoQuery BlockInfo) Core.SQLiteIndexer
  where
  query =
    Core.querySyncedOnlySQLiteIndexerWith
      (const $ \(BlockInfoBySlotNoQuery sn) -> [":slotNo" := sn])
      (const blockInfoBySlotNoQuery)
      (const listToMaybe)

  queryLatest =
    Core.queryLatestSQLiteIndexerWith
      (\(BlockInfoBySlotNoQuery s) -> [":slotNo" := s])
      (const blockInfoBySlotNoQuery)
      (const listToMaybe)

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery BlockInfo)) m)
  => Core.Queryable m BlockInfo (Core.EventAtQuery BlockInfo) Core.SQLiteIndexer
  where
  query =
    Core.querySyncedOnlySQLiteIndexerWith
      (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
      (const blockInfoBySlotNoQuery)
      (const listToMaybe)
  queryLatest =
    Core.queryLatestSQLiteIndexerWith
      (pure [])
      (const lastBlockInfoQuery)
      (const listToMaybe)

queryClosestBlockInfo
  :: ( MonadIO m
     , MonadError (Core.QueryError (Core.EventsMatchingQuery BlockInfo)) m
     )
  => Maybe C.ChainPoint
  -> Core.EventsMatchingQuery BlockInfo
  -> Core.SQLiteIndexer BlockInfo
  -> m (Core.Result (Core.EventsMatchingQuery BlockInfo))
queryClosestBlockInfo =
  let queryPrefix :: SQL.Query
      queryPrefix =
        [sql|
        SELECT blockNo, blockTimestamp, epochNo,
               slotNo, blockHeaderHash
        FROM blockInfo
        |]
      querySpecific :: SQL.Query
      querySpecific = queryPrefix <> [sql| WHERE slotNo <= :slotNo |]
      parseResult
        :: (a -> Maybe a)
        -> [Core.Timed C.ChainPoint a]
        -> [Core.Timed C.ChainPoint a]
      parseResult = mapMaybe . traverse
   in \case
        Nothing ->
          Core.queryLatestSQLiteIndexerWith
            (pure [])
            (const queryPrefix)
            (\(Core.EventsMatchingQuery p) -> parseResult p)
        Just point ->
          Core.querySyncedOnlySQLiteIndexerWith
            (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
            (const querySpecific)
            (\(Core.EventsMatchingQuery p) -> parseResult p)
            point

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventsMatchingQuery BlockInfo)) m)
  => Core.Queryable m BlockInfo (Core.EventsMatchingQuery BlockInfo) Core.SQLiteIndexer
  where
  query = queryClosestBlockInfo . Just
  queryLatest = queryClosestBlockInfo Nothing

queryLatestBlockInfo
  :: ( MonadIO m
     , MonadError (Core.QueryError (Core.LatestEventsQuery BlockInfo)) m
     )
  => Maybe C.ChainPoint
  -> Core.LatestEventsQuery BlockInfo
  -> Core.SQLiteIndexer BlockInfo
  -> m (Core.Result (Core.LatestEventsQuery BlockInfo))
queryLatestBlockInfo =
  let queryPrefix :: SQL.Query
      queryPrefix =
        [sql|
          SELECT blockNo, blockTimestamp, epochNo,
                 slotNo, blockHeaderHash
          FROM blockInfo
        |]
      extraParts :: SQL.Query
      extraParts =
        [sql|
          ORDER BY slotNo DESC
          LIMIT :n
        |]
      queryLatest :: SQL.Query
      queryLatest = queryPrefix <> extraParts
      querySpecific :: SQL.Query
      querySpecific = queryPrefix <> [sql| WHERE slotNo <= :slotNo |] <> extraParts
   in \case
        Nothing ->
          Core.queryLatestSQLiteIndexerWith
            (\(Core.LatestEventsQuery n) -> [":n" := n])
            (const queryLatest)
            (const id)
        Just point ->
          Core.querySyncedOnlySQLiteIndexerWith
            ( \cp (Core.LatestEventsQuery n) ->
                [":slotNo" := C.chainPointToSlotNo cp, ":n" := n]
            )
            (const querySpecific)
            (const id)
            point

instance
  (MonadIO m, MonadError (Core.QueryError (Core.LatestEventsQuery BlockInfo)) m)
  => Core.Queryable m BlockInfo (Core.LatestEventsQuery BlockInfo) Core.SQLiteIndexer
  where
  query = queryLatestBlockInfo . Just
  queryLatest = queryLatestBlockInfo Nothing

extractBlockInfo :: BlockEvent -> BlockInfo
extractBlockInfo (BlockEvent (C.BlockInMode b _) eno t) =
  fromBlockEratoBlockInfo b eno t

fromBlockEratoBlockInfo
  :: C.Block era -> C.EpochNo -> POSIXTime -> BlockInfo
fromBlockEratoBlockInfo (C.Block (C.BlockHeader _ _ blockNo') _) epochNo' posixTime =
  let (blockTimeStampSeconds, _) = properFraction $ Time.nominalDiffTimeToSeconds posixTime
   in BlockInfo blockNo' blockTimeStampSeconds epochNo'
