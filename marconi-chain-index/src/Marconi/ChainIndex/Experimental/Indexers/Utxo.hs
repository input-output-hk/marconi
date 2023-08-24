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
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Experimental.Indexers.Utxo (
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

  -- * Extractors
  getUtxoEventsFromBlock,
  getUtxosFromTx,
  getUtxosFromTxBody,
) where

import Control.Lens (
  (&),
  (.~),
  (^.),
 )
import Control.Lens qualified as Lens
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson.TH qualified as Aeson
import Data.Either (fromRight)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Concurrent (MVar)
import Control.Monad.Except (MonadError, guard)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardSQLiteIndexer,
  catchupWorkerWithFilter,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (TxIndexInBlock, TxOut, pattern CurrentEra)
import Marconi.Core.Experiment (CatchupConfig)
import Marconi.Core.Experiment qualified as Core

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
  , _includeScript :: Bool
  }

Lens.makeLenses ''UtxoIndexerConfig

type instance Core.Point UtxoEvent = C.ChainPoint
type UtxoIndexer = Core.SQLiteIndexer UtxoEvent
type StandardUtxoIndexer = StandardSQLiteIndexer UtxoEvent

-- | Make a SQLiteIndexer for Utxos
mkUtxoIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
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
                 , slotNo INT
                 , blockHeaderHash BLOB
                 )|]
      createAddressIndex = [sql|CREATE INDEX IF NOT EXISTS utxo_address ON utxo (address)|]
      createSlotNoIndex = [sql|CREATE INDEX IF NOT EXISTS utxo_slotNo ON utxo (slotNo)|]
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
                 slotNo
              ) VALUES
              (?, ?, ?, ?, ?, ?, ?, ?, ?)|]
      createUtxoTables =
        [ Sync.syncTableCreation
        , createUtxo
        , createAddressIndex
        , createSlotNoIndex
        ]
      insertEvent = [Core.SQLInsertPlan (traverse NonEmpty.toList) utxoInsertQuery]
  Core.mkSqliteIndexer
    path
    createUtxoTables
    [insertEvent]
    (Just Sync.syncInsertPlan)
    [ Core.SQLRollbackPlan "utxo" "slotNo" C.chainPointToSlotNo
    , Sync.syncRollbackPlan
    ]
    Sync.syncLastPointQuery

-- | A minimal worker for the UTXO indexer, with catchup and filtering.
utxoWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Text
  -- ^ Name of the indexer (mostly for logging purpose)
  -> CatchupConfig
  -> UtxoIndexerConfig
  -> (input -> Maybe UtxoEvent)
  -- ^ event extractor
  -> FilePath
  -- ^ SQLite database location
  -> n (MVar StandardUtxoIndexer, Core.WorkerM m (WithDistance input) (Core.Point UtxoEvent))
utxoWorker name catchupConfig utxoConfig extractor path = do
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
  liftIO $ catchupWorkerWithFilter name filtering catchupConfig (pure . extractor) indexer

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
          , toField $ u ^. Core.point . Lens.to C.chainPointToSlotNo
          ]

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

instance SQL.FromRow (Core.Timed C.ChainPoint Utxo) where
  fromRow = do
    utxo <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point utxo

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery UtxoEvent)) m)
  => Core.Queryable m UtxoEvent (Core.EventAtQuery UtxoEvent) Core.SQLiteIndexer
  where
  query =
    let utxoQuery :: SQL.Query
        utxoQuery =
          [sql|
          SELECT address, txIndex, txId, txIx, datumHash, value, inlineScript, inlineScriptHash
          FROM utxo
          WHERE slotNo == :slotNo
          |]
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const utxoQuery)
          (const NonEmpty.nonEmpty)

instance
  (MonadIO m, MonadError (Core.QueryError (Core.EventsMatchingQuery UtxoEvent)) m)
  => Core.Queryable m UtxoEvent (Core.EventsMatchingQuery UtxoEvent) Core.SQLiteIndexer
  where
  query =
    let utxoQuery :: SQL.Query
        utxoQuery =
          [sql|
          SELECT address, txIndex, txId, txIx, datumHash, value, inlineScript, inlineScriptHash,
                 sync.slotNo, sync.blockHeaderHash
          FROM utxo
          JOIN sync ON utxo.slotNo == sync.slotNo
          WHERE utxo.slotNo <= :slotNo
          |]
        -- \| Group utxos that are of the same block in the same event
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
          (const utxoQuery)
          (\(Core.EventsMatchingQuery p) -> parseResult p)

{- | Extract UtxoEvents from Cardano Block

 Returns @Nothing@ if the block doesn't consume or spend any utxo
-}
getUtxoEventsFromBlock
  :: (C.IsCardanoEra era)
  => C.Block era
  -> [Utxo]
  -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEventsFromBlock (C.Block _ txs) = zip [0 ..] txs >>= uncurry getUtxosFromTx

getUtxosFromTx
  :: (C.IsCardanoEra era)
  => TxIndexInBlock
  -> C.Tx era
  -> [Utxo]
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
getUtxosFromTxBody
  :: (C.IsCardanoEra era)
  => TxIndexInBlock
  -> C.TxBody era
  -> [Utxo]
getUtxosFromTxBody txIndex' txBody@(C.TxBody txBodyContent@C.TxBodyContent{}) =
  fromRight mempty (getUtxos $ getTxOutFromTxBodyContent txBodyContent)
  where
    getUtxos :: (C.IsCardanoEra era) => [C.TxOut C.CtxTx era] -> Either C.EraCastError [Utxo]
    getUtxos =
      fmap (Lens.imap txoutToUtxo)
        . traverse (C.eraCast CurrentEra)

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
getScriptHash
  :: C.TxOutDatum C.CtxTx era
  -> Maybe (C.Hash C.ScriptData)
getScriptHash C.TxOutDatumNone = Nothing
getScriptHash (C.TxOutDatumHash _ h) = Just h
getScriptHash (C.TxOutDatumInTx _ d) = Just (C.hashScriptDataBytes d)
getScriptHash (C.TxOutDatumInline _ d) = Just (C.hashScriptDataBytes d)

-- | get the inlineScript and inlineScriptHash
getRefScriptAndHash
  :: C.ReferenceScript era
  -> Maybe (C.ScriptInAnyLang, C.ScriptHash)
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
