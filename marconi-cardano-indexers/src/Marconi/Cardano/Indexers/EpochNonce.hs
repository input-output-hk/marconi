{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Store Nonce in a SQLIte table
module Marconi.Cardano.Indexers.EpochNonce (
  -- * Event types and lenses
  EpochNonce (EpochNonce),
  nonceEpochNo,
  nonceNonce,
  nonceBlockNo,

  -- * Create an indexer
  mkEpochNonceIndexer,

  -- * Create a worker
  EpochNonceWorkerConfig (..),
  epochNonceWorker,

  -- * Queries
  NonceByEpochNoQuery (NonceByEpochNoQuery),

  -- * Extract event
  getEpochNonce,
  getEpochNo,
) where

import Cardano.Api qualified as C
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Protocol.TPraos.API qualified as Shelley
import Cardano.Protocol.TPraos.Rules.Tickn qualified as Shelley
import Control.Arrow ((<<<))
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorkerConfig (eventExtractor, logger, workerName),
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  ExtLedgerStateEvent (ExtLedgerStateEvent),
  newEpochPreprocessor,
 )
import Marconi.Cardano.Indexers.SyncHelper qualified as Sync
import Marconi.Core qualified as Core
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.HeaderValidation qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Protocol.Praos qualified as O
import Ouroboros.Consensus.Protocol.TPraos qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O

-- | Event for @Nonce@ storage
data EpochNonce = EpochNonce
  { _nonceEpochNo :: !C.EpochNo
  , _nonceNonce :: !Ledger.Nonce
  , _nonceBlockNo :: !C.BlockNo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SQL.FromRow, SQL.ToRow, FromJSON, ToJSON)

instance SQL.ToRow (Core.Timed C.ChainPoint EpochNonce) where
  toRow epochNonce =
    SQL.toRow (epochNonce ^. Core.event)
      <> SQL.toRow (epochNonce ^. Core.point)

instance SQL.FromRow (Core.Timed C.ChainPoint EpochNonce) where
  fromRow = do
    nonce <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point nonce

type instance Core.Point EpochNonce = C.ChainPoint

Lens.makeLenses ''EpochNonce
type ExtLedgerState = O.ExtLedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto))

-- | Smart constructor that creates a 'FileIndexer' that stores Nonce.
mkEpochNonceIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.SQLiteIndexer EpochNonce)
mkEpochNonceIndexer path = do
  let createNonce =
        [sql|CREATE TABLE IF NOT EXISTS epoch_nonce
              ( epochNo INT NOT NULL
              , nonce BLOB NOT NULL
              , blockNo INT NOT NULL
              , slotNo INT NOT NULL
              , blockHeaderHash BLOB NOT NULL
              )|]
      nonceInsertQuery =
        [sql|INSERT INTO epoch_nonce
                ( epochNo
                , nonce
                , blockNo
                , slotNo
                , blockHeaderHash
                ) VALUES (?, ?, ?, ?, ?)|]
      insertEvent = [Core.SQLInsertPlan pure nonceInsertQuery]
  Sync.mkSyncedSqliteIndexer
    path
    [createNonce]
    [insertEvent]
    [Core.SQLRollbackPlan "epoch_nonce" "slotNo" C.chainPointToSlotNo]

newtype EpochNonceWorkerConfig input = EpochNonceWorkerConfig
  { epochNonceWorkerConfigExtractor :: input -> C.EpochNo
  }

epochNonceWorker
  :: forall input m n
   . (MonadIO m, MonadError Core.IndexerError m, MonadIO n)
  => StandardWorkerConfig n input EpochNonce
  -> EpochNonceWorkerConfig input
  -> FilePath
  -> m (Core.WorkerIndexer n input EpochNonce (Core.WithTrace n Core.SQLiteIndexer))
epochNonceWorker workerConfig nonceConfig path = do
  indexer <- Core.withTrace (logger workerConfig) <$> mkEpochNonceIndexer path
  let preprocessor =
        Core.traverseMaybeEvent (lift . eventExtractor workerConfig)
          <<< newEpochPreprocessor (epochNonceWorkerConfigExtractor nonceConfig)
  Core.createWorkerWithPreprocessing (workerName workerConfig) preprocessor indexer

newtype NonceByEpochNoQuery = NonceByEpochNoQuery C.EpochNo
  deriving newtype (FromJSON, ToJSON)

type instance Core.Result NonceByEpochNoQuery = Maybe (Core.Timed C.ChainPoint EpochNonce)

instance
  (MonadIO m, MonadError (Core.QueryError NonceByEpochNoQuery) m)
  => Core.Queryable m event NonceByEpochNoQuery Core.SQLiteIndexer
  where
  query = do
    let epochSDDQuery =
          [sql|SELECT epochNo, nonce, blockNo, slotNo, blockHeaderHash
                FROM epoch_nonce
                WHERE epochNo = :epochNo
            |]
        getParams _ (NonceByEpochNoQuery epochNo) = [":epochNo" SQL.:= epochNo]
    Core.querySyncedOnlySQLiteIndexerWith
      getParams
      (const epochSDDQuery)
      (const listToMaybe)

getEpochNonce :: ExtLedgerStateEvent -> Maybe EpochNonce
getEpochNonce (ExtLedgerStateEvent ledgerState blockNo) = do
  epochNo <- getEpochNo ledgerState
  pure $ EpochNonce epochNo (getNonce ledgerState) blockNo

{- | Get Nonce per epoch given an extended ledger state. The Nonce is only available starting at
 Shelley era. Byron era has the neutral nonce.
-}
getNonce :: ExtLedgerState -> Ledger.Nonce
getNonce extLedgerState' =
  case O.headerStateChainDep (O.headerState extLedgerState') of
    O.ChainDepStateByron _ -> Ledger.NeutralNonce
    O.ChainDepStateShelley st -> extractNonce st
    O.ChainDepStateAllegra st -> extractNonce st
    O.ChainDepStateMary st -> extractNonce st
    O.ChainDepStateAlonzo st -> extractNonce st
    O.ChainDepStateBabbage st -> extractNoncePraos st
    O.ChainDepStateConway st -> extractNoncePraos st
  where
    extractNonce :: O.TPraosState c -> Ledger.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . O.tpraosStateChainDepState
    extractNoncePraos :: O.PraosState c -> Ledger.Nonce
    extractNoncePraos = O.praosStateEpochNonce

getEpochNo
  :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)
  -> Maybe C.EpochNo
getEpochNo extLedgerState' = case O.ledgerState extLedgerState' of
  O.LedgerStateByron _st -> Nothing
  O.LedgerStateShelley st -> getEpochNoFromShelleyBlock st
  O.LedgerStateAllegra st -> getEpochNoFromShelleyBlock st
  O.LedgerStateMary st -> getEpochNoFromShelleyBlock st
  O.LedgerStateAlonzo st -> getEpochNoFromShelleyBlock st
  O.LedgerStateBabbage st -> getEpochNoFromShelleyBlock st
  O.LedgerStateConway st -> getEpochNoFromShelleyBlock st
  where
    getEpochNoFromShelleyBlock = Just . Ledger.nesEL . O.shelleyLedgerState
