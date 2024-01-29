{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Cardano.Indexers.Datum (
  -- * Event
  DatumInfo (DatumInfo),
  DatumEvent,
  datumHash,
  datum,

  -- * Indexer and worker
  DatumIndexer,
  mkDatumIndexer,
  datumWorker,
  datumBuilder,
  StandardDatumIndexer,

  -- * Extract
  getDataFromTxBody,

  -- * Query
  ResolveDatumQuery (ResolveDatumQuery),
  ResolvedData (ResolvedData, getData),
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Tracing qualified as BM
import Cardano.Ledger.Api qualified as Ledger
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
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig (StandardWorkerConfig),
  mkStandardWorker,
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (
  AnyTxBody (AnyTxBody),
  SecurityParam,
 )
import Marconi.Cardano.Indexers.SyncHelper qualified as Sync
import Marconi.Core (SQLiteDBLocation)
import Marconi.Core qualified as Core
import System.FilePath ((</>))

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

instance SQL.FromRow (Core.Timed C.ChainPoint DatumInfo) where
  fromRow = do
    datumInfo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point datumInfo

instance SQL.ToRow (Core.Timed C.ChainPoint DatumInfo) where
  toRow d = SQL.toRow (d ^. Core.event) ++ SQL.toRow (d ^. Core.point)

type instance Core.Point DatumEvent = C.ChainPoint

-- | A raw SQLite indexer for Datum
type DatumIndexer = Core.SQLiteIndexer DatumEvent

-- | A SQLite Datum indexer with Catchup
type StandardDatumIndexer m = StandardSQLiteIndexer m DatumEvent

-- | A smart constructor for 'DatumIndexer'
mkDatumIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => SQLiteDBLocation
  -> m (Core.SQLiteIndexer DatumEvent)
mkDatumIndexer path = do
  let createDatumQuery =
        [sql|CREATE TABLE IF NOT EXISTS datum
             ( datumHash BLOB PRIMARY KEY
             , datum BLOB
             , slotNo INT
             , blockHeaderHash BLOB
             )|]
      datumInsertQuery :: SQL.Query
      datumInsertQuery =
        [sql|INSERT OR IGNORE INTO datum
             ( datumHash
             , datum
             , slotNo
             , blockHeaderHash
             )
          VALUES (?, ?, ?, ?)|]
      createDatumTables = [createDatumQuery]
  Sync.mkSyncedSqliteIndexer
    path
    createDatumTables
    [[Core.SQLInsertPlan (traverse NonEmpty.toList) datumInsertQuery]]
    [Core.SQLRollbackPlan (Core.defaultRollbackPlan "datum" "slotNo" C.chainPointToSlotNo)]

-- | A worker with catchup for a 'DatumIndexer'
datumWorker
  :: (MonadIO m, MonadIO n, MonadError Core.IndexerError n)
  => StandardWorkerConfig m Core.SQLiteIndexer input DatumEvent
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> SQLiteDBLocation
  -- ^ SQLite database location
  -> n (StandardWorker m input DatumEvent Core.SQLiteIndexer)
datumWorker workerConfig path = do
  indexer <- mkDatumIndexer path
  mkStandardWorker workerConfig indexer

{- | Convenience wrapper around 'datumWorker' with some defaults for
creating 'StandardWorkerConfig', including a preprocessor.
-}
datumBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
      (Core.WithTransform Core.SQLiteIndexer (NonEmpty DatumInfo))
      (WithDistance (Maybe (NonEmpty DatumInfo)))
  -> Trace m Text
  -> FilePath
  -> n (StandardWorker m [AnyTxBody] DatumEvent Core.SQLiteIndexer)
datumBuilder securityParam catchupConfig textLogger path =
  let indexerName = "Datum"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      extractDatum :: AnyTxBody -> [DatumInfo]
      extractDatum (AnyTxBody _ _ txb) = getDataFromTxBody txb
      datumWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractDatum))
          (BM.appendName indexerName indexerEventLogger)
   in datumWorker datumWorkerConfig (Core.parseDBLocation (path </> "datum.db"))

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
                 slotNo, blockHeaderHash
          FROM datum
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

getTxOutFromTxBodyContent :: C.TxBodyContent build era -> [C.TxOut C.CtxTx era]
getTxOutFromTxBodyContent C.TxBodyContent{C.txOuts, C.txReturnCollateral, C.txScriptValidity} =
  case C.txScriptValidityToScriptValidity txScriptValidity of
    C.ScriptValid -> txOuts -- When transaction is valid, only transaction fee is collected
    C.ScriptInvalid -> collateral txReturnCollateral -- failed Tx, we collect from collateral and return excess collateral
  where
    collateral C.TxReturnCollateralNone = []
    collateral (C.TxReturnCollateral _ txout) = [txout]
