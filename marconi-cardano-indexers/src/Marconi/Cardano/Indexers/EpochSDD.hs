{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Store SDD in a SQLIte table
module Marconi.Cardano.Indexers.EpochSDD (
  -- * Event types and lenses
  EpochSDD (EpochSDD),
  sddEpochNo,
  sddPoolId,
  sddLovelace,
  sddBlockNo,

  -- * Create an indexer
  mkEpochSDDIndexer,

  -- * Create a worker
  EpochSDDWorkerConfig (..),
  epochSDDWorker,

  -- * Queries
  ActiveSDDByEpochNoQuery (..),

  -- * Extract event
  getEpochSDD,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.UMap qualified as Ledger
import Control.Arrow ((<<<))
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.VMap (VMap)
import Data.VMap qualified as VMap
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorkerConfig (eventExtractor, logger, workerName),
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  ExtLedgerStateEvent (ExtLedgerStateEvent),
  getEpochNo,
  newEpochPreprocessor,
 )
import Marconi.Cardano.Indexers.SyncHelper qualified as Sync
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer (SQLiteDBLocation)
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O

-- | Event for @SDD@ storage
data EpochSDD = EpochSDD
  { _sddEpochNo :: !C.EpochNo
  , _sddPoolId :: !C.PoolId
  , _sddLovelace :: !C.Lovelace
  , _sddBlockNo :: !C.BlockNo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SQL.FromRow, SQL.ToRow, FromJSON, ToJSON)

type instance Core.Point EpochSDD = C.ChainPoint
type instance Core.Point (NonEmpty EpochSDD) = C.ChainPoint

Lens.makeLenses ''EpochSDD

instance SQL.ToRow (Core.Timed C.ChainPoint EpochSDD) where
  toRow epochSDD =
    SQL.toRow (epochSDD ^. Core.event)
      <> SQL.toRow (epochSDD ^. Core.point)

instance SQL.FromRow (Core.Timed C.ChainPoint EpochSDD) where
  fromRow = Core.Timed <$> SQL.fromRow <*> SQL.fromRow

-- | Smart constructor that creates a 'FileIndexer' that stores SDD.
mkEpochSDDIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => SQLiteDBLocation
  -> m (Core.SQLiteIndexer (NonEmpty EpochSDD))
mkEpochSDDIndexer path = do
  let createSDD =
        [sql|CREATE TABLE IF NOT EXISTS epoch_sdd
              ( epochNo INT NOT NULL
              , poolId BLOB NOT NULL
              , lovelace INT NOT NULL
              , blockNo INT NOT NULL
              , slotNo INT NOT NULL
              , blockHeaderHash BLOB NOT NULL
              )|]
      sddInsertQuery =
        [sql|INSERT INTO epoch_sdd
                ( epochNo
                , poolId
                , lovelace
                , blockNo
                , slotNo
                , blockHeaderHash
                ) VALUES (?, ?, ?, ?, ?, ?)|]
      insertEvent = [Core.SQLInsertPlan (traverse NonEmpty.toList) (pure sddInsertQuery)]
  Sync.mkSyncedSqliteIndexer
    path
    [createSDD]
    [insertEvent]
    [Core.SQLRollbackPlan (Core.defaultRollbackPlan "epoch_sdd" "slotNo" C.chainPointToSlotNo)]

newtype EpochSDDWorkerConfig input = EpochSDDWorkerConfig
  { epochSDDWorkerConfigExtractor :: input -> C.EpochNo
  }

epochSDDWorker
  :: forall indexer input m n
   . (MonadIO m, MonadError Core.IndexerError m, MonadIO n)
  => StandardWorkerConfig n indexer input (NonEmpty EpochSDD)
  -> EpochSDDWorkerConfig input
  -> SQLiteDBLocation
  -> m (Core.WorkerIndexer n input (NonEmpty EpochSDD) (Core.WithTrace n Core.SQLiteIndexer))
epochSDDWorker workerConfig nonceConfig path = do
  indexer <- Core.withTrace (logger workerConfig) <$> mkEpochSDDIndexer path
  let preprocessor =
        Core.traverseMaybeEvent (lift . eventExtractor workerConfig)
          <<< newEpochPreprocessor (epochSDDWorkerConfigExtractor nonceConfig)
  Core.createWorkerWithPreprocessing (workerName workerConfig) preprocessor indexer

newtype ActiveSDDByEpochNoQuery = ActiveSDDByEpochNoQuery C.EpochNo
  deriving newtype (FromJSON, ToJSON)

type instance Core.Result ActiveSDDByEpochNoQuery = [Core.Timed C.ChainPoint EpochSDD]

instance
  (MonadIO m, MonadError (Core.QueryError ActiveSDDByEpochNoQuery) m)
  => Core.Queryable m event ActiveSDDByEpochNoQuery Core.SQLiteIndexer
  where
  query = do
    let epochSDDQuery =
          [sql|SELECT epochNo, poolId, lovelace, blockNo, slotNo, blockHeaderHash
            FROM epoch_sdd
            WHERE epochNo == :epochNo
          |]
        -- See Note [Active stake pool delegation query] for why we do 'epochNo - 2' for the query.
        getParams _ (ActiveSDDByEpochNoQuery epochNo) = [":epochNo" SQL.:= epochNo - 2]
    Core.querySyncedOnlySQLiteIndexerWith
      getParams
      (const epochSDDQuery)
      (const id)

getEpochSDD :: ExtLedgerStateEvent -> Maybe (NonEmpty EpochSDD)
getEpochSDD (ExtLedgerStateEvent ledgerState blockNo) = NonEmpty.nonEmpty $ do
  epochNo <- toList $ getEpochNo ledgerState
  (poolId, lovelace) <- Map.toList $ getStakeMap ledgerState
  pure $ EpochSDD epochNo poolId lovelace blockNo

{- | From LedgerState, get epoch stake pool delegation: a mapping of pool ID to amount staked in
 lovelace. We do this by getting the 'ssStakeMark stake snapshot and then use 'ssDelegations' and
 'ssStake' to resolve it into the desired mapping.
-}
getStakeMap
  :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)
  -> Map C.PoolId C.Lovelace
getStakeMap extLedgerState' = case O.ledgerState extLedgerState' of
  O.LedgerStateByron _ -> mempty
  O.LedgerStateShelley st -> getStakeMapFromShelleyBlock st
  O.LedgerStateAllegra st -> getStakeMapFromShelleyBlock st
  O.LedgerStateMary st -> getStakeMapFromShelleyBlock st
  O.LedgerStateAlonzo st -> getStakeMapFromShelleyBlock st
  O.LedgerStateBabbage st -> getStakeMapFromShelleyBlock st
  O.LedgerStateConway st -> getStakeMapFromShelleyBlock st
  where
    getStakeMapFromShelleyBlock
      :: forall proto era c
       . (c ~ O.EraCrypto era, c ~ O.StandardCrypto)
      => O.LedgerState (O.ShelleyBlock proto era)
      -> Map C.PoolId C.Lovelace
    getStakeMapFromShelleyBlock st = sdd'
      where
        newEpochState :: Ledger.NewEpochState era
        newEpochState = O.shelleyLedgerState st

        stakeSnapshot :: Ledger.SnapShot c
        stakeSnapshot = Ledger.ssStakeMark . Ledger.esSnapshots . Ledger.nesEs $ newEpochState

        stakes
          :: VMap VMap.VB VMap.VP (Ledger.Credential 'Ledger.Staking c) (Ledger.CompactForm Ledger.Coin)
        stakes = Ledger.unStake $ Ledger.ssStake stakeSnapshot

        delegations
          :: VMap VMap.VB VMap.VB (Ledger.Credential 'Ledger.Staking c) (Ledger.KeyHash 'Ledger.StakePool c)
        delegations = Ledger.ssDelegations stakeSnapshot

        sdd' :: Map C.PoolId C.Lovelace
        sdd' =
          Map.fromListWith (+) $
            catMaybes $
              VMap.elems $
                VMap.mapWithKey
                  ( \cred spkHash ->
                      ( \c ->
                          ( C.StakePoolKeyHash spkHash
                          , C.Lovelace $ coerce $ Ledger.fromCompact c
                          )
                      )
                        <$> VMap.lookup cred stakes
                  )
                  delegations
