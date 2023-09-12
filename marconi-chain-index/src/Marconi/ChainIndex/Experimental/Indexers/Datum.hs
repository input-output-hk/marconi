{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Experimental.Indexers.Datum (
  -- * Event
  DatumInfo (DatumInfo),
  DatumEvent,
  datumHash,
  datum,

  -- * Indexer and worker
  DatumIndexer,
  mkDatumIndexer,
  datumWorker,
  StandardDatumIndexer,

  -- * Extract
  getDataFromTxBody,

  -- * Query
  ResolveDatumQuery (ResolveDatumQuery),
  ResolvedData (ResolvedData, getData),
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as Ledger
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError)
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
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig,
  mkStandardWorker,
 )
import Marconi.ChainIndex.Indexers.Utxo (getTxOutFromTxBodyContent)
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Experiment qualified as Core

data DatumInfo = DatumInfo
  { _datumHash :: C.Hash C.ScriptData
  , _datum :: C.ScriptData
  }
  deriving (Generic, Show, Eq, SQL.FromRow, SQL.ToRow)

-- | An alias for a non-empty list of @DatumInfo@, it's the event potentially produced on each block
type DatumEvent = NonEmpty DatumInfo

-- we use deriveJSON to drop the underscore prefix
Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = tail} ''DatumInfo
Lens.makeLenses ''DatumInfo

instance SQL.ToRow (Core.Timed C.ChainPoint DatumInfo) where
  toRow d =
    let snoField = case d ^. Core.point of
          C.ChainPointAtGenesis -> SQL.SQLNull
          C.ChainPoint sno _ -> SQL.toField sno
     in SQL.toRow
          [ SQL.toField (d ^. Core.event . datumHash)
          , SQL.toField (d ^. Core.event . datum)
          , snoField
          ]

type instance Core.Point DatumEvent = C.ChainPoint

-- | A raw SQLite indexer for Datum
type DatumIndexer = Core.SQLiteIndexer DatumEvent

-- | A SQLite Datum indexer with Catchup
type StandardDatumIndexer m = StandardSQLiteIndexer m DatumEvent

instance SQL.FromRow (Core.Timed C.ChainPoint DatumInfo) where
  fromRow = do
    utxo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point utxo

-- | A smart constructor for 'DatumIndexer'
mkDatumIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.SQLiteIndexer DatumEvent)
mkDatumIndexer path = do
  let createDatumQuery =
        [sql|CREATE TABLE IF NOT EXISTS datum (datumHash BLOB PRIMARY KEY, datum BLOB, slotNo Int)|]
      datumInsertQuery :: SQL.Query
      datumInsertQuery =
        [sql|INSERT OR IGNORE INTO datum (datumHash, datum, slotNo)
          VALUES (?, ?, ?)|]
      createDatumTables = [createDatumQuery, Sync.syncTableCreation]
  Core.mkSqliteIndexer
    path
    createDatumTables
    [[Core.SQLInsertPlan (traverse NonEmpty.toList) datumInsertQuery]]
    (Just Sync.syncInsertPlan)
    [ Core.SQLRollbackPlan "datum" "slotNo" C.chainPointToSlotNo
    , Sync.syncRollbackPlan
    ]
    Sync.syncLastPointsQuery

-- | A worker with catchup for a 'DatumIndexer'
datumWorker
  :: (MonadIO m, MonadIO n, MonadError Core.IndexerError n)
  => StandardWorkerConfig m input DatumEvent
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> FilePath
  -- ^ SQLite database location
  -> n (StandardWorker m input DatumEvent Core.SQLiteIndexer)
datumWorker workerConfig path = do
  indexer <- mkDatumIndexer path
  mkStandardWorker workerConfig indexer

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery DatumEvent)) m)
  => Core.Queryable m DatumEvent (Core.EventAtQuery DatumEvent) Core.SQLiteIndexer
  where
  query =
    let datumQuery :: SQL.Query
        datumQuery =
          [sql|
          SELECT datumHash, datum
          FROM datum
          WHERE slotNo == :slotNo
          |]
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const datumQuery)
          (const NonEmpty.nonEmpty)

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventsMatchingQuery DatumEvent)) m)
  => Core.Queryable
      m
      DatumEvent
      (Core.EventsMatchingQuery DatumEvent)
      Core.SQLiteIndexer
  where
  query =
    let datumQuery :: SQL.Query
        datumQuery =
          [sql|
          SELECT datumHash, datum,
                 sync.slotNo, sync.blockHeaderHash
          FROM datum
          JOIN sync ON datum.slotNo == sync.slotNo
          WHERE datum.slotNo <= :slotNo
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
          (const datumQuery)
          (\(Core.EventsMatchingQuery p) -> parseResult p)

-- | Entry type for datum hash resolution query
newtype ResolveDatumQuery = ResolveDatumQuery (C.Hash C.ScriptData)

newtype ResolvedData = ResolvedData {getData :: C.ScriptData}
  deriving stock (Generic)
  deriving anyclass (SQL.FromRow)

type instance Core.Result ResolveDatumQuery = Maybe C.ScriptData

instance
  (MonadIO m, MonadError (Core.QueryError ResolveDatumQuery) m)
  => Core.Queryable m DatumEvent ResolveDatumQuery Core.SQLiteIndexer
  where
  query =
    let utxoQuery :: SQL.Query
        utxoQuery =
          [sql| SELECT datum FROM datum
          WHERE datumHash == :datumHash
          AND slotNo <= :slotNo
          |]
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp (ResolveDatumQuery dh) -> [":slotNo" := C.chainPointToSlotNo cp, ":datumHash" := dh])
          (const utxoQuery)
          (const $ fmap getData . listToMaybe)

getDataFromTxBody :: C.TxBody era -> [DatumInfo]
getDataFromTxBody txBody@(C.TxBody txBodyContent) =
  fmap (uncurry DatumInfo) $
    Map.toList $
      getDatumMapFromTxBody txBody <> getInlineDatum (getTxOutFromTxBodyContent txBodyContent)

getInlineDatum :: [C.TxOut C.CtxTx era] -> Map (C.Hash C.ScriptData) C.ScriptData
getInlineDatum =
  let getTxOutDatum (C.TxOut _ _ dat _) = dat
      extractInlineDatum = \case
        C.TxOutDatumInline _ d -> Just (C.hashScriptDataBytes d, C.getScriptData d)
        _other -> Nothing
   in Map.fromList . mapMaybe (extractInlineDatum . getTxOutDatum)

getDatumMapFromTxBody
  :: C.TxBody era
  -> Map (C.Hash C.ScriptData) C.ScriptData
getDatumMapFromTxBody = \case
  C.ShelleyTxBody _ _ _ (C.TxBodyScriptData C.ScriptDataInAlonzoEra (Ledger.TxDats' data_) _) _ _ ->
    Map.mapKeys C.ScriptDataHash $ C.getScriptData . C.fromAlonzoData <$> data_
  C.ShelleyTxBody _ _ _ (C.TxBodyScriptData C.ScriptDataInBabbageEra (Ledger.TxDats' data_) _) _ _ ->
    Map.mapKeys C.ScriptDataHash $ C.getScriptData . C.fromAlonzoData <$> data_
  C.ShelleyTxBody _ _ _ (C.TxBodyScriptData C.ScriptDataInConwayEra (Ledger.TxDats' data_) _) _ _ ->
    Map.mapKeys C.ScriptDataHash $ C.getScriptData . C.fromAlonzoData <$> data_
  _ -> mempty
