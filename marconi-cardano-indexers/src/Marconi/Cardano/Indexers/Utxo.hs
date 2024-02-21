{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
 Host an indexer that stores all the transaction outputs and
 many of their relative information

 * transaction that created them
 * hash of their datum datum
 * script info

 It also includes helper function to extract the corresponding
 data from a block or from a TxBody.

 Note that we don't remove spent tx outputs from this indexer.
-}
module Marconi.Cardano.Indexers.Utxo (
  -- * Event
  Utxo (Utxo),
  UtxoEvent,
  address,
  txIndex,
  txIn,
  datumHash,
  value,
  inlineScript,
  inlineScriptHash,

  -- * Indexer and runner
  UtxoIndexer,
  mkUtxoIndexer,
  UtxoIndexerConfig (UtxoIndexerConfig),
  StandardUtxoIndexer,
  utxoWorker,
  utxoBuilder,
  catchupConfigEventHook,
  trackedAddresses,
  includeScript,

  -- * Extractors
  getUtxoEventsFromBlock,
  getUtxosFromTx,
  getUtxosFromTxBody,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Tracing qualified as BM
import Control.Lens (
  (&),
  (.~),
  (?~),
  (^.),
 )
import Control.Lens qualified as Lens
import Control.Monad (guard)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.TH qualified as Aeson
import Data.Either (fromRight)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig (StandardWorkerConfig),
  mkStandardWorkerWithFilter,
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (
  AnyTxBody (AnyTxBody),
  SecurityParam,
  TxIndexInBlock,
  TxOut,
 )
import Marconi.Cardano.Indexers.SyncHelper qualified as Sync
import Marconi.Core (SQLiteDBLocation)
import Marconi.Core qualified as Core
import System.FilePath ((</>))

-- | Indexer representation of an UTxO
data Utxo = Utxo
  { _address :: !C.AddressAny
  , _txIndex :: TxIndexInBlock
  , _txIn :: !C.TxIn
  , _datumHash :: !(Maybe (C.Hash C.ScriptData))
  , _value :: !C.Value
  , _inlineScript :: !(Maybe C.ScriptInAnyLang)
  , _inlineScriptHash :: !(Maybe C.ScriptHash)
  }
  deriving (Show, Eq, Generic)

-- | An alias for a non-empty list of @Utxo@, it's the event potentially produced on each block
type UtxoEvent = NonEmpty Utxo

-- we use deriveJSON to drop the underscore prefix
Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = tail} ''Utxo

Lens.makeLenses ''Utxo

data UtxoIndexerConfig = UtxoIndexerConfig
  { _trackedAddresses :: [C.AddressAny]
  -- ^ Allow us to filter the UTxOs we want to index
  , _includeScript :: Bool
  -- ^ Do we store script info
  }

Lens.makeLenses ''UtxoIndexerConfig

type instance Core.Point UtxoEvent = C.ChainPoint
type UtxoIndexer = Core.SQLiteIndexer UtxoEvent
type StandardUtxoIndexer m = StandardSQLiteIndexer m UtxoEvent

-- | Make a SQLiteIndexer for Utxos
mkUtxoIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => SQLiteDBLocation
  -- ^ SQL connection to database
  -> m UtxoIndexer
mkUtxoIndexer path = do
  let createUtxo =
        [sql|CREATE TABLE IF NOT EXISTS utxo
                 ( address BLOB NOT NULL
                 , txIndex INT NOT NULL
                 , txId TEXT NOT NULL
                 , txIx INT NOT NULL
                 , datumHash BLOB
                 , value BLOB
                 , inlineScript BLOB
                 , inlineScriptHash BLOB
                 , slotNo INT NOT NULL
                 , blockHeaderHash BLOB NOT NULL
                 )|]
      utxoInsertQuery :: SQL.Query
      utxoInsertQuery =
        [sql|INSERT INTO utxo (
                 address,
                 txIndex,
                 txId,
                 txIx,
                 datumHash,
                 value,
                 inlineScript,
                 inlineScriptHash,
                 slotNo,
                 blockHeaderHash
              ) VALUES
              (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|]
      createUtxoTables = [createUtxo]
      insertEvent = [Core.SQLInsertPlan (traverse NonEmpty.toList) utxoInsertQuery]

  Sync.mkSyncedSqliteIndexer
    path
    createUtxoTables
    [insertEvent]
    [Core.SQLRollbackPlan "utxo" "slotNo" C.chainPointToSlotNo]

catchupConfigEventHook :: Trace IO Text -> FilePath -> Core.CatchupEvent -> IO ()
catchupConfigEventHook stdoutTrace dbPath Core.Synced = do
  SQL.withConnection dbPath $ \c -> do
    let addressIndexName = "utxo_address"
        createAddressIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString addressIndexName
            <> " ON utxo (address)"
    Core.createIndexTable "Utxo" stdoutTrace c addressIndexName createAddressIndexStatement

    let slotNoIndexName = "utxo_slotNo"
        createSlotNoIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString slotNoIndexName
            <> " ON utxo (slotNo)"
    Core.createIndexTable "Utxo" stdoutTrace c slotNoIndexName createSlotNoIndexStatement

-- | A minimal worker for the UTXO indexer, with catchup and filtering.
utxoWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => StandardWorkerConfig m input UtxoEvent
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> UtxoIndexerConfig
  -- ^ Specific configuration of the indexer (mostly for logging purpose)
  -> SQLiteDBLocation
  -- ^ SQLite database location
  -> n (StandardWorker m input UtxoEvent Core.SQLiteIndexer)
utxoWorker workerConfig utxoConfig path = do
  indexer <- mkUtxoIndexer path
  -- A helper to filter the provided utxos
  let utxoTransform :: UtxoIndexerConfig -> Utxo -> Maybe Utxo
      utxoTransform cfg utxo = do
        let addressFilter = case cfg ^. trackedAddresses of
              [] -> pure ()
              xs -> guard (utxo ^. address `elem` xs)
            scriptFilter =
              if cfg ^. includeScript
                then utxo
                else
                  utxo
                    & inlineScript .~ Nothing
                    & inlineScriptHash .~ Nothing
        addressFilter
        pure scriptFilter
      filtering :: UtxoEvent -> Maybe UtxoEvent
      filtering =
        NonEmpty.nonEmpty
          . mapMaybe (utxoTransform utxoConfig)
          . NonEmpty.toList
  liftIO $
    mkStandardWorkerWithFilter
      workerConfig
      filtering
      indexer

{- | Convenience wrapper around 'utxoWorker' with some defaults for
creating 'StandardWorkerConfig', including a preprocessor for resuming and an address filter.
-}
utxoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> UtxoIndexerConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n (StandardWorker IO [AnyTxBody] UtxoEvent Core.SQLiteIndexer)
utxoBuilder securityParam catchupConfig utxoConfig textLogger path =
  let indexerName = "Utxo"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      utxoDbPath = path </> "utxo.db"
      extractUtxos :: AnyTxBody -> [Utxo]
      extractUtxos (AnyTxBody _ indexInBlock txb) = getUtxosFromTxBody indexInBlock txb
      catchupConfigWithTracer =
        catchupConfig
          & Core.configCatchupEventHook ?~ catchupConfigEventHook textLogger utxoDbPath
      utxoWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfigWithTracer
          (pure . NonEmpty.nonEmpty . (>>= extractUtxos))
          (BM.appendName indexerName indexerEventLogger)
   in utxoWorker utxoWorkerConfig utxoConfig (Core.parseDBLocation utxoDbPath)

instance ToRow (Core.Timed C.ChainPoint Utxo) where
  toRow u =
    let (C.TxIn txid txix) = u ^. Core.event . txIn
     in toRow
          [ toField $ u ^. Core.event . address
          , toField $ u ^. Core.event . txIndex
          , toField txid
          , toField txix
          , toField $ u ^. Core.event . datumHash
          , toField $ u ^. Core.event . value
          , toField $ u ^. Core.event . inlineScript
          , toField $ u ^. Core.event . inlineScriptHash
          ]
          ++ toRow (u ^. Core.point)

instance SQL.FromRow (Core.Timed C.ChainPoint Utxo) where
  fromRow = do
    utxo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point utxo

instance SQL.FromRow Utxo where
  fromRow = do
    _address <- SQL.field
    _txIndex <- SQL.field
    txId <- SQL.field
    txIx <- SQL.field
    _datumHash <- SQL.field
    _value <- SQL.field
    _inlineScript <- SQL.field
    _inlineScriptHash <- SQL.field
    pure $
      Utxo
        { _address
        , _txIndex
        , _txIn = C.TxIn txId txIx
        , _datumHash
        , _value
        , _inlineScript
        , _inlineScriptHash
        }

queryUtxoAt
  :: (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery UtxoEvent)) m)
  => Maybe C.ChainPoint
  -> Core.EventAtQuery UtxoEvent
  -> Core.SQLiteIndexer UtxoEvent
  -> m (Core.Result (Core.EventAtQuery UtxoEvent))
queryUtxoAt =
  let
    queryPrefix =
      [sql|
    SELECT address, txIndex, txId, txIx, datumHash, value, inlineScript, inlineScriptHash
    FROM utxo
    |]
    queryLatest =
      queryPrefix
        <> [sql|
      ORDER BY slotNo DESC
      LIMIT 1
      |]
    querySpecific = queryPrefix <> [sql| WHERE slotNo == :slotNo |]
   in
    \case
      Nothing ->
        Core.queryLatestSQLiteIndexerWith
          (pure [])
          (const queryLatest)
          (const NonEmpty.nonEmpty)
      Just point ->
        Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const querySpecific)
          (const NonEmpty.nonEmpty)
          point

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery UtxoEvent)) m)
  => Core.Queryable m UtxoEvent (Core.EventAtQuery UtxoEvent) Core.SQLiteIndexer
  where
  query = queryUtxoAt . Just
  queryLatest = queryUtxoAt Nothing

queryMatching
  :: (MonadIO m, MonadError (Core.QueryError (Core.EventsMatchingQuery UtxoEvent)) m)
  => Maybe C.ChainPoint
  -> Core.EventsMatchingQuery UtxoEvent
  -> Core.SQLiteIndexer UtxoEvent
  -> m (Core.Result (Core.EventsMatchingQuery UtxoEvent))
queryMatching =
  let
    queryPrefix =
      [sql|
    SELECT address, txIndex, txId, txIx, datumHash, value, inlineScript, inlineScriptHash,
           slotNo, blockHeaderHash
    FROM utxo
    |]
    orderPart = [sql| ORDER BY slotNo ASC |]
    queryLatest = queryPrefix <> orderPart
    querySpecific = queryPrefix <> [sql| WHERE slotNo <= :slotNo |] <> orderPart

    -- Group utxos that are of the same block in the same event
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
   in
    \case
      Nothing ->
        Core.queryLatestSQLiteIndexerWith
          (pure [])
          (const queryLatest)
          (\(Core.EventsMatchingQuery p) -> parseResult p)
      Just point ->
        Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const querySpecific)
          (\(Core.EventsMatchingQuery p) -> parseResult p)
          point

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventsMatchingQuery UtxoEvent)) m)
  => Core.Queryable m UtxoEvent (Core.EventsMatchingQuery UtxoEvent) Core.SQLiteIndexer
  where
  query = queryMatching . Just
  queryLatest = queryMatching Nothing

{- | Extract UtxoEvents from Cardano Block

 Returns @Nothing@ if the block doesn't consume or spend any utxo
-}
getUtxoEventsFromBlock
  :: (C.IsCardanoEra era)
  => C.Block era
  -> [Utxo]
  -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEventsFromBlock (C.Block _ txs) =
  zip [0 ..] txs >>= uncurry getUtxosFromTx

getUtxosFromTx :: (C.IsCardanoEra era) => TxIndexInBlock -> C.Tx era -> [Utxo]
getUtxosFromTx ix (C.Tx txBody _) = getUtxosFromTxBody ix txBody

{- | Extract TxOut from Cardano TxBodyContent
TODO move to extract
-}
getTxOutFromTxBodyContent :: C.TxBodyContent build era -> [C.TxOut C.CtxTx era]
getTxOutFromTxBodyContent C.TxBodyContent{C.txOuts, C.txReturnCollateral, C.txScriptValidity} =
  case C.txScriptValidityToScriptValidity txScriptValidity of
    -- When transaction is valid, only transaction fee is collected
    C.ScriptValid -> txOuts
    -- failed Tx, we collect from collateral and return excess collateral
    C.ScriptInvalid -> collateral txReturnCollateral
  where
    collateral C.TxReturnCollateralNone = []
    collateral (C.TxReturnCollateral _ txout) = [txout]

-- | Extract Utxos from Cardano TxBody
getUtxosFromTxBody :: (C.IsCardanoEra era) => TxIndexInBlock -> C.TxBody era -> [Utxo]
getUtxosFromTxBody txIndex' txBody@(C.TxBody txBodyContent@C.TxBodyContent{}) =
  fromRight mempty (getUtxos $ getTxOutFromTxBodyContent txBodyContent)
  where
    getUtxos :: (C.IsCardanoEra era) => [C.TxOut C.CtxTx era] -> Either C.EraCastError [Utxo]
    getUtxos =
      fmap (Lens.imap txoutToUtxo)
        . traverse (C.eraCast C.ConwayEra)

    txid = C.getTxId txBody
    txoutToUtxo :: Int -> TxOut -> Utxo
    txoutToUtxo ix txout =
      let txin = C.TxIn txid (C.TxIx (fromIntegral ix))
       in getUtxoFromTxOut txIndex' txin txout

-- | Extract Utxos from Cardano TxOut
getUtxoFromTxOut
  :: TxIndexInBlock
  -> C.TxIn
  -- ^ unique id and position of this transaction
  -> C.TxOut C.CtxTx era
  -- ^ Cardano TxOut
  -> Utxo
  -- ^ Utxo
getUtxoFromTxOut _txIndex _txIn (C.TxOut addr val datum refScript) =
  Utxo
    { _address = addrAny
    , _txIn
    , _txIndex
    , _value = C.txOutValueToValue val
    , _datumHash
    , _inlineScript
    , _inlineScriptHash
    }
  where
    addrAny = toAddr addr
    _datumHash = getScriptHash datum
    (_inlineScript, _inlineScriptHash) = case getRefScriptAndHash refScript of
      Nothing -> (Nothing, Nothing)
      Just (is, ish) -> (Just is, Just ish)

-- | Get the datum hash and datum or a transaction output.
getScriptHash :: C.TxOutDatum C.CtxTx era -> Maybe (C.Hash C.ScriptData)
getScriptHash C.TxOutDatumNone = Nothing
getScriptHash (C.TxOutDatumHash _ h) = Just h
getScriptHash (C.TxOutDatumInTx _ d) = Just (C.hashScriptDataBytes d)
getScriptHash (C.TxOutDatumInline _ d) = Just (C.hashScriptDataBytes d)

-- | get the inlineScript and inlineScriptHash
getRefScriptAndHash :: C.ReferenceScript era -> Maybe (C.ScriptInAnyLang, C.ScriptHash)
getRefScriptAndHash refScript = case refScript of
  C.ReferenceScriptNone -> Nothing
  C.ReferenceScript _ s@(C.ScriptInAnyLang C.SimpleScriptLanguage script) ->
    Just (s, C.hashScript script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script) ->
    Just (s, C.hashScript script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script) ->
    Just (s, C.hashScript script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) script) ->
    Just (s, C.hashScript script)

-- | Convert from 'AddressInEra' of the 'CurrentEra' to 'AddressAny'.
toAddr :: C.AddressInEra era -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr) = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr
