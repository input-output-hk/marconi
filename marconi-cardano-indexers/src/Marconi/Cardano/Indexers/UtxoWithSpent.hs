{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Cardano.Indexers.UtxoWithSpent (

) where

import Cardano.Api qualified as C
import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (MonadIO)
import Data.Aeson.TH qualified as Aeson
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Database.SQLite.Simple (FromRow (fromRow), NamedParam ((:=)), ToRow (toRow))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (toField))
import Marconi.Cardano.Core.Indexer.Worker (StandardSQLiteIndexer)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (TxIndexInBlock)
import Marconi.Cardano.Indexers.Spent (SpentInfo (SpentInfo))
import Marconi.Cardano.Indexers.SyncHelper qualified as Sync
import Marconi.Cardano.Indexers.Utxo (Utxo)
import Marconi.Core.Indexer.SQLiteIndexer (SQLiteDBLocation)
import Marconi.Core.Indexer.SQLiteIndexer qualified as Core
import Marconi.Core.Type qualified as Core

{- | Indexer representation of a UTxO together with the information
 about where it was spent. If this information is missing, then it isn't
 spent and we consider it active.
-}
data UtxoWithSpent = UtxoWithSpent
  { _txIn :: !C.TxIn
  -- ^ the tx id and tx index at which the tx out is created
  , _address :: !C.AddressAny
  -- ^ the address of the tx out
  , _value :: !C.Value
  -- ^ the value of the tx out
  , _datumHash :: !(Maybe (C.Hash C.ScriptData))
  -- ^ the datum hash of the tx out
  , _inlineScript :: !(Maybe C.ScriptInAnyLang)
  -- ^ the inline script of the tx out
  , _inlineScriptHash :: !(Maybe C.ScriptHash)
  -- ^ the inline script hash of the tx out
  , _txIndex :: TxIndexInBlock
  -- ^ the index at which the tx is present in the block
  , _spentAt :: !(Maybe C.TxId)
  -- ^ the tx id and index at which the tx out is spent
  }
  deriving (Show, Eq)

Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = tail} ''UtxoWithSpent

Lens.makeLenses ''UtxoWithSpent

instance SQL.ToRow (Core.Timed C.ChainPoint UtxoWithSpent) where
  toRow u =
    let (C.TxIn txid txix) = u ^. Core.event . txIn
        spentAtFields =
          case u ^. Core.event . spentAt of
            (Just txIdSpent) -> [toField txIdSpent]
            Nothing -> []
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
          <> toRow spentAtFields
          <> toRow (u ^. Core.point)

instance FromRow (Core.Timed C.ChainPoint UtxoWithSpent) where
  fromRow = do
    utxo <- fromRow
    point <- fromRow
    pure $ Core.Timed point utxo

instance FromRow UtxoWithSpent where
  fromRow = do
    _address <- SQL.field
    _txIndex <- SQL.field
    txId <- SQL.field
    txIx <- SQL.field
    _datumHash <- SQL.field
    _value <- SQL.field
    _inlineScript <- SQL.field
    _inlineScriptHash <- SQL.field
    _spentAt <- SQL.field
    pure $
      UtxoWithSpent
        { _address
        , _txIndex
        , _txIn = C.TxIn txId txIx
        , _datumHash
        , _value
        , _inlineScript
        , _inlineScriptHash
        , _spentAt
        }

type UtxoOrSpentEvent = NonEmpty UtxoOrSpent

data UtxoOrSpent
  = UtxoEvent Utxo
  | SpentEvent SpentInfo

instance SQL.ToRow (Core.Timed C.ChainPoint UtxoOrSpent) where
  toRow (Core.Timed cp (UtxoEvent utxoEvent)) = toRow (Core.Timed cp utxoEvent)
  toRow (Core.Timed cp (SpentEvent spentEvent)) = toRow (Core.Timed cp spentEvent)

instance FromRow (Core.Timed C.ChainPoint UtxoOrSpent) where
  fromRow = fmap UtxoEvent <$> fromRow <|> fmap SpentEvent <$> fromRow

type instance Core.Point UtxoOrSpentEvent = C.ChainPoint
type UtxoWithSpentIndexer = Core.SQLiteIndexer UtxoOrSpentEvent
type StandardUtxoWithSpentIndexer m = StandardSQLiteIndexer m UtxoOrSpentEvent

-- | Make a SQLiteIndexer for UtxoWithSpent
mkUtxoWithSpentIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => SQLiteDBLocation
  -- ^ SQL connection to database
  -> m UtxoWithSpentIndexer
mkUtxoWithSpentIndexer path = do
  let createUtxoWithSpent =
        [sql|CREATE TABLE IF NOT EXISTS utxo_with_spent
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
                 , txIdSpent TEXT
                 )
            as SELECT * FROM utxo LEFT OUTER JOIN spent ON utxo.txId == spent.txId AND utxo.txIx == spent.txIx
            |]
      createUtxoTables = [createUtxoWithSpent]
      insertEvent = [Core.SQLInsertPlan insertPlan]
  Sync.mkSyncedSqliteIndexer
    path
    createUtxoTables
    [insertEvent]
    -- TODO: fix
    [Core.SQLRollbackPlan (Core.defaultRollbackPlan "utxo" "slotNo" C.chainPointToSlotNo)]

insertPlan :: [Core.Timed C.ChainPoint UtxoOrSpentEvent] -> SQL.Connection -> IO ()
insertPlan events conn = do
  let rows = traverse NonEmpty.toList =<< events
  traverse_ buildAndExecuteQuery rows
  where
    buildAndExecuteQuery :: Core.Timed C.ChainPoint UtxoOrSpent -> IO ()
    buildAndExecuteQuery row@(Core.Timed _ utxoOrSpent) =
      case utxoOrSpent of
        UtxoEvent _ -> do
          let query =
                [sql|INSERT INTO utxo_with_spent (
                       address,
                       txIndex,
                       txId,
                       txIx,
                       datumHash,
                       value,
                       inlineScript,
                       inlineScriptHash,
                       slotNo,
                       blockHeaderHash,
                       txIdSpent,
                    ) VALUES
                    (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|]
          SQL.execute conn query row
        SpentEvent (SpentInfo (C.TxIn txId txIx) txIdSpent) -> do
          let query =
                [sql|UPDATE utxo_with_spent
                   SET txIdSpent = :txIdSpent
                   WHERE txId = :txId AND txIx = :txIx|]
              params =
                [ ":txId" := txId
                , ":txIx" := txIx
                , ":txIdSpent" := txIdSpent
                ]
          SQL.executeNamed conn query params
