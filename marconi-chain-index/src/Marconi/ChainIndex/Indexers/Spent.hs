{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Indexers.Spent (
  -- * Event
  SpentInfo (SpentInfo),
  SpentInfoEvent,
  spentTxOutRef,
  spentAtTxId,

  -- * Indexer and worker
  SpentIndexer,
  mkSpentIndexer,
  spentWorker,
  StandardSpentIndexer,
  catchupConfigEventHook,

  -- * Extractor
  getInputs,
) where

import Cardano.Api qualified as C
import Cardano.BM.Trace (Trace)
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError)
import Data.Aeson.TH qualified as Aeson
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig,
  mkStandardWorker,
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.ChainIndex.Indexers.SyncHelper qualified as Sync
import Marconi.Core qualified as Core

data SpentInfo = SpentInfo
  { _spentTxOutRef :: C.TxIn
  -- ^ The Spent tx output
  , _spentAtTxId :: C.TxId
  -- ^ The transaction that spent this tx output
  }
  deriving (Show, Eq, Ord, Generic)

-- | An alias for a non-empty list of @SpentInfo@, it's the event potentially produced on each block
type SpentInfoEvent = NonEmpty SpentInfo

-- we use deriveJSON to drop the underscore prefix
Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = tail} ''SpentInfo
Lens.makeLenses ''SpentInfo

instance SQL.ToRow (Core.Timed C.ChainPoint SpentInfo) where
  toRow s =
    let C.TxIn txid txix = s ^. Core.event . spentTxOutRef
     in SQL.toRow
          [ SQL.toField txid
          , SQL.toField txix
          , SQL.toField $ s ^. Core.event . spentAtTxId
          ]
          ++ SQL.toRow (s ^. Core.point)

instance SQL.FromRow (Core.Timed C.ChainPoint SpentInfo) where
  fromRow = do
    spentInfo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point spentInfo

instance SQL.FromRow SpentInfo where
  fromRow = do
    txId <- SQL.field
    txIx <- SQL.field
    _spentAtTxId <- SQL.field
    pure $ SpentInfo{_spentTxOutRef = C.TxIn txId txIx, _spentAtTxId}

type instance Core.Point SpentInfoEvent = C.ChainPoint

-- | A raw SQLite indexer for Spent
type SpentIndexer = Core.SQLiteIndexer SpentInfoEvent

-- | A SQLite Spent indexer with Catchup
type StandardSpentIndexer m = StandardSQLiteIndexer m SpentInfoEvent

mkSpentIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.SQLiteIndexer SpentInfoEvent)
mkSpentIndexer path = do
  let createSpent =
        [sql|CREATE TABLE IF NOT EXISTS spent
               ( txId TEXT NOT NULL
               , txIx INT NOT NULL
               , spentAtTxId TEXT NOT NULL
               , slotNo INT NOT NULL
               , blockHeaderHash BLOB NOT NULL
               )|]
      spentInsertQuery :: SQL.Query
      spentInsertQuery =
        [sql|INSERT OR IGNORE INTO spent
               ( txId
               , txIx
               , spentAtTxId
               , slotNo
               , blockHeaderHash
               )
               VALUES (?, ?, ?, ?, ?)|]
      createSpentTables = [createSpent]
      spentInsert =
        [Core.SQLInsertPlan (traverse NonEmpty.toList) spentInsertQuery]
  Sync.mkSyncedSqliteIndexer
    path
    createSpentTables
    [spentInsert]
    [Core.SQLRollbackPlan "spent" "slotNo" C.chainPointToSlotNo]

catchupConfigEventHook :: Trace IO Text -> FilePath -> Core.CatchupEvent -> IO ()
catchupConfigEventHook stdoutTrace dbPath Core.Synced = do
  SQL.withConnection dbPath $ \c -> do
    let slotNoIndexName = "spent_slotNo"
        createSlotNoIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString slotNoIndexName
            <> " ON spent (slotNo)"
    Core.createIndexTable "Spent" stdoutTrace c slotNoIndexName createSlotNoIndexStatement

    let txInIndexName = "spent_txIn"
        createTxInIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString txInIndexName
            <> " ON spent (txId, txIx)"
    Core.createIndexTable "Spent" stdoutTrace c txInIndexName createTxInIndexStatement

    let spentAtTxIdIndexName = "spent_spentAtTxId"
        createSpentAtIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString spentAtTxIdIndexName
            <> " ON spent (spentAtTxId)"
    Core.createIndexTable "Spent" stdoutTrace c spentAtTxIdIndexName createSpentAtIndexStatement

-- | A minimal worker for the UTXO indexer, with catchup and filtering.
spentWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => StandardWorkerConfig m input SpentInfoEvent
  -- ^ General configuration of a worker
  -> FilePath
  -- ^ SQLite database location
  -> n (StandardWorker m input SpentInfoEvent Core.SQLiteIndexer)
spentWorker config path = do
  indexer <- mkSpentIndexer path
  mkStandardWorker config indexer

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery SpentInfoEvent)) m)
  => Core.Queryable m SpentInfoEvent (Core.EventAtQuery SpentInfoEvent) Core.SQLiteIndexer
  where
  query =
    let spentInfoQuery :: SQL.Query
        spentInfoQuery =
          [sql|
          SELECT txId, txIx, spentAtTxId
          FROM spent
          WHERE slotNo == :slotNo
          |]
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const spentInfoQuery)
          (const NonEmpty.nonEmpty)

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventsMatchingQuery SpentInfoEvent)) m)
  => Core.Queryable
      m
      SpentInfoEvent
      (Core.EventsMatchingQuery SpentInfoEvent)
      Core.SQLiteIndexer
  where
  query =
    let spentQuery :: SQL.Query
        spentQuery =
          [sql|
          SELECT txId, txIx, spentAtTxId,
                 slotNo, blockHeaderHash
          FROM spent
          WHERE slotNo <= :slotNo
          |]
        groupEvents
          :: NonEmpty (Core.Timed point a)
          -> Core.Timed point (NonEmpty a)
        groupEvents xs@(x :| _) = Core.Timed (x ^. Core.point) (Lens.view Core.event <$> xs)
        parseResult
          :: (NonEmpty a -> Maybe (NonEmpty a))
          -> [Core.Timed C.ChainPoint a]
          -> [Core.Timed C.ChainPoint (NonEmpty a)]
        parseResult p =
          mapMaybe (traverse p . groupEvents)
            . NonEmpty.groupBy ((==) `on` Lens.view Core.point)
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const spentQuery)
          (\(Core.EventsMatchingQuery p) -> parseResult p)

getInputs :: C.TxBody era -> [SpentInfo]
getInputs
  b@( C.TxBody
        C.TxBodyContent
          { C.txIns
          , C.txInsCollateral
          , C.txScriptValidity
          }
      ) =
    let inputs = case C.txScriptValidityToScriptValidity txScriptValidity of
          C.ScriptValid -> fst <$> txIns
          C.ScriptInvalid -> case txInsCollateral of
            C.TxInsCollateralNone -> []
            C.TxInsCollateral _ txins -> txins
     in fmap (\input -> SpentInfo input (C.getTxId b)) inputs
