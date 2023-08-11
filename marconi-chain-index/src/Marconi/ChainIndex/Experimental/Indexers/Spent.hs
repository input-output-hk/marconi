{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Experimental.Indexers.Spent (
  -- * Event
  SpentInfo (SpentInfo),
  SpentInfoEvent,
  spent,
  spentAt,

  -- * Indexer and worker
  SpentIndexer,
  mkSpentIndexer,
  spentWorker,
  StandardSpentIndexer,

  -- * Extractor
  getInputs,
) where

import Cardano.Api qualified as C
import Control.Concurrent (MVar)
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError)
import Data.Aeson.TH qualified as Aeson
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (StandardSQLiteIndexer, catchupWorker)
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Experiment qualified as Core

data SpentInfo = SpentInfo
  { _spent :: C.TxIn
  -- ^ The Spent tx output
  , _spentAt :: C.TxId
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
    let C.TxIn txid txix = s ^. Core.event . spent
     in SQL.toRow
          [ SQL.toField txid
          , SQL.toField txix
          , SQL.toField $ s ^. Core.event . spentAt
          , SQL.toField $ s ^. Core.point . Lens.to C.chainPointToSlotNo
          ]

instance SQL.FromRow SpentInfo where
  fromRow = do
    txId <- SQL.field
    txIx <- SQL.field
    _spentAt <- SQL.field
    pure $ SpentInfo{_spent = C.TxIn txId txIx, _spentAt}

instance SQL.FromRow (Core.Timed C.ChainPoint SpentInfo) where
  fromRow = do
    spentInfo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point spentInfo

type instance Core.Point SpentInfoEvent = C.ChainPoint

-- | A raw SQLite indexer for Spent
type SpentIndexer = Core.SQLiteIndexer SpentInfoEvent

-- | A SQLite Spent indexer with Catchup
type StandardSpentIndexer = StandardSQLiteIndexer SpentInfoEvent

mkSpentIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.SQLiteIndexer SpentInfoEvent)
mkSpentIndexer path = do
  let createSpent =
        [sql|CREATE TABLE IF NOT EXISTS spent
               ( txId TEXT NOT NULL
               , txIx INT NOT NULL
               , spentAt TEXT NOT NULL
               , slotNo INT NOT NULL
               )|]
      createSlotNoIndex =
        [sql|CREATE INDEX IF NOT EXISTS spent_slotNo ON spent (slotNo)|]
      createTxInIndex =
        [sql|CREATE INDEX IF NOT EXISTS spent_txIn ON spent (txId, txIx)|]
      createSpentAtIndex =
        [sql|CREATE INDEX IF NOT EXISTS spent_spentAt ON spent (spentAt)|]
      spentInsertQuery :: SQL.Query
      spentInsertQuery =
        [sql|INSERT OR IGNORE INTO spent
               ( txId
               , txIx
               , spentAt
               , slotNo
               )
               VALUES (?, ?, ?, ?)|]
  Core.mkSqliteIndexer
    path
    [createSpent, createSlotNoIndex, createTxInIndex, createSpentAtIndex, Sync.syncTableCreation]
    [
      [ Core.SQLInsertPlan (traverse NonEmpty.toList) spentInsertQuery
      , Sync.syncInsertPlan
      ]
    ]
    [ Core.SQLRollbackPlan "spent" "slotNo" C.chainPointToSlotNo
    , Sync.syncRollbackPlan
    ]
    Sync.syncLastPointQuery

-- | A minimal worker for the UTXO indexer, with catchup and filtering.
spentWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Text
  -- ^ Name of the indexer (mostly for logging purpose)
  -> Core.CatchupConfig
  -> (input -> Maybe SpentInfoEvent)
  -- ^ event extractor
  -> FilePath
  -- ^ SQLite database location
  -> n (MVar StandardSpentIndexer, Core.WorkerM m (WithDistance input) (Core.Point SpentInfoEvent))
spentWorker name catchupConfig extractor path = do
  indexer <- mkSpentIndexer path
  catchupWorker name catchupConfig (pure . extractor) indexer

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery SpentInfoEvent)) m)
  => Core.Queryable m SpentInfoEvent (Core.EventAtQuery SpentInfoEvent) Core.SQLiteIndexer
  where
  query =
    let spentInfoQuery :: SQL.Query
        spentInfoQuery =
          [sql|
          SELECT txId, txIx, spentAt
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
          SELECT txId, txIx, spentAt,
                 sync.slotNo, sync.blockHeaderHash
          FROM spent
          JOIN sync ON spent.slotNo == sync.slotNo
          WHERE spent.slotNo <= :slotNo
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
        spentAt' = C.getTxId b
     in flip SpentInfo spentAt' <$> inputs
