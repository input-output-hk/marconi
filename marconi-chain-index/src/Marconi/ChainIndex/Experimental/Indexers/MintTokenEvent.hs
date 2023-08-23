{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |

Mint/burn event indexer, the result of which is an sqlite database 'minttokenevent.db' which has a
table 'minting_policy_events' with the following fields:

@
( slotNo INT NOT NULL
, txId BLOB NOT NULL
, txIndexInBlock BLOB NOT NULL
, policyId BLOB NOT NULL
, assetName TEXT NOT NULL
, quantity INT NOT NULL
, redeemerHash BLOB
, redeemerData BLOB
)
@
-}
module Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent (
  -- * Events
  MintTokenEvents (MintTokenEvents),
  mintTokenEventsAssets,
  MintTokenEvent,
  mintTokenEventLocation,
  mintTokenEventAsset,
  MintTokenEventLocation,
  mintTokenEventIndexInBlock,
  mintTokenEventTxId,
  MintAssetRedeemer,
  mintAssetRedeemerData,
  mintAssetRedeemerHash,

  -- * Indexer and worker
  MintTokenEventIndexer,
  StandardMintTokenEventIndexer,
  mkMintTokenIndexer,
  mintTokenEventWorker,

  -- * Extract events
  extractEventsFromTx,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Api (
  RdmrPtr (RdmrPtr),
  Redeemers (Redeemers),
  StandardCrypto,
 )
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Babbage.Tx qualified as LB
import Cardano.Ledger.Conway.TxBody qualified as LC
import Cardano.Ledger.Mary.Value (
  AssetName (AssetName),
  MultiAsset,
  PolicyID (PolicyID),
  flattenMultiAsset,
 )
import Control.Concurrent (MVar)
import Control.Lens (folded, toListOf, (^.), (^?))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError)
import Data.ByteString.Short qualified as Short
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Text (Text)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (StandardSQLiteIndexer, catchupWorker)
import Marconi.ChainIndex.Types (
  TxIndexInBlock,
 )
import Marconi.Core.Experiment qualified as Core

-- | A raw SQLite indexer for 'MintTokenEvents'
type MintTokenEventIndexer = Core.SQLiteIndexer MintTokenEvents

-- | A SQLite Spent indexer with Catchup
type StandardMintTokenEventIndexer = StandardSQLiteIndexer MintTokenEvents

-- | Minting events given for each block.
newtype MintTokenEvents = MintTokenEvents
  { _mintTokenEventsAssets :: NonEmpty MintTokenEvent
  }
  deriving (Show, Eq, Ord)

data MintTokenEvent = MintTokenEvent
  { _mintTokenEventLocation :: !MintTokenEventLocation
  , _mintTokenEventAsset :: !MintAsset
  }
  deriving (Show, Eq, Ord, Generic)

data MintTokenEventLocation = MintTokenEventLocation
  { _mintTokenEventIndexInBlock :: !TxIndexInBlock
  , _mintTokenEventTxId :: !C.TxId
  }
  deriving (Show, Eq, Ord, Generic)

data MintAsset = MintAsset
  { _mintAssetPolicyId :: !C.PolicyId
  , _mintAssetAssetName :: !C.AssetName
  , _mintAssetQuantity :: !C.Quantity
  , _mintAssetRedeemer :: !(Maybe MintAssetRedeemer)
  -- ^ Nothing if the 'PolicyId' is a simple script, so no redeemers are provided
  }
  deriving (Show, Eq, Ord, Generic)

-- The redeemer of a Plutus minting script along with it's hash.
data MintAssetRedeemer = MintAssetRedeemer
  { _mintAssetRedeemerData :: !C.ScriptData
  , _mintAssetRedeemerHash :: !(C.Hash C.ScriptData)
  }
  deriving (Eq, Ord, Show)

Lens.makeLenses ''MintTokenEvents
Lens.makeLenses ''MintTokenEvent
Lens.makeLenses ''MintTokenEventLocation
Lens.makeLenses ''MintAsset
Lens.makeLenses ''MintAssetRedeemer

instance SQL.ToRow (Core.Timed C.ChainPoint MintTokenEvent) where
  toRow te =
    let snoField = case te ^. Core.point of
          C.ChainPointAtGenesis -> SQL.SQLNull
          C.ChainPoint sno _ -> SQL.toField sno
     in snoField : SQL.toRow (te ^. Core.event)

instance SQL.ToRow MintTokenEvent where
  toRow e =
    SQL.toRow (e ^. mintTokenEventLocation)
      ++ SQL.toRow (e ^. mintTokenEventAsset)

instance SQL.ToRow MintTokenEventLocation where
  toRow l =
    SQL.toRow
      [ SQL.toField $ l ^. mintTokenEventIndexInBlock
      , SQL.toField $ l ^. mintTokenEventTxId
      ]

instance SQL.ToRow MintAsset where
  toRow ma =
    SQL.toRow
      [ SQL.toField $ ma ^. mintAssetPolicyId
      , SQL.toField $ ma ^. mintAssetAssetName
      , SQL.toField $ ma ^. mintAssetQuantity
      , SQL.toField $ ma ^? mintAssetRedeemer . traverse . mintAssetRedeemerHash
      , SQL.toField $ ma ^? mintAssetRedeemer . traverse . mintAssetRedeemerData
      ]

type instance Core.Point MintTokenEvents = C.ChainPoint

-- | Create a worker for the MintTokenEvent indexer
mintTokenEventWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Text
  -> Core.CatchupConfig
  -> (input -> Maybe MintTokenEvents)
  -> FilePath
  -> n (MVar StandardMintTokenEventIndexer, Core.WorkerM m (WithDistance input) C.ChainPoint)
mintTokenEventWorker name catchupConfig extractor dbPath = do
  sqliteIndexer <- mkMintTokenIndexer dbPath
  catchupWorker name catchupConfig (pure . extractor) sqliteIndexer

-- Events extraction

extractEventsFromTx :: TxIndexInBlock -> C.TxBody era -> [MintTokenEvent]
extractEventsFromTx txIndexInBlock txb =
  let mintEventLocation = MintTokenEventLocation txIndexInBlock (C.getTxId txb)

      extactMintEventsFromTxBody :: [MintAsset]
      extactMintEventsFromTxBody = case txb of
        C.ShelleyTxBody era shelleyTx _ _ _ _ -> case era of
          C.ShelleyBasedEraShelley -> []
          C.ShelleyBasedEraAllegra -> []
          C.ShelleyBasedEraMary -> []
          C.ShelleyBasedEraAlonzo -> getPolicyData txb $ LA.atbMint shelleyTx
          C.ShelleyBasedEraBabbage -> getPolicyData txb $ LB.btbMint shelleyTx
          C.ShelleyBasedEraConway -> getPolicyData txb $ LC.ctbMint shelleyTx
        _byronTxBody -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.
   in MintTokenEvent mintEventLocation <$> extactMintEventsFromTxBody

getPolicyData
  :: forall era
   . ( Ledger.Era (C.ShelleyLedgerEra era)
     )
  => C.TxBody era
  -> MultiAsset StandardCrypto
  -> [MintAsset]
getPolicyData txb m = do
  let txRedeemers :: C.TxBody era -> [(RdmrPtr, LB.Data (C.ShelleyLedgerEra era))]
      txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
        C.TxBodyScriptData _proof _datum (Redeemers redeemers) -> Map.toList $ fmap fst redeemers
        C.TxBodyNoScriptData -> mempty
      txRedeemers _ = mempty
      findRedeemerByIndex ix (RdmrPtr _ w, _) = w == ix
      findRedeemer ix = snd <$> List.find (findRedeemerByIndex ix) (txRedeemers txb)
      toAssetRedeemer r = MintAssetRedeemer (C.getScriptData r) (C.hashScriptDataBytes r)
  (ix, (policyId', assetName, quantity)) <- zip [0 ..] $ flattenMultiAsset m
  let redeemer = findRedeemer ix
  pure $
    MintAsset
      (fromMaryPolicyID policyId')
      (fromMaryAssetName assetName)
      (C.Quantity quantity)
      (toAssetRedeemer . C.fromAlonzoData <$> redeemer)

fromMaryPolicyID :: PolicyID StandardCrypto -> C.PolicyId
fromMaryPolicyID (PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- from cardano-api:src/Cardano/Api/Value.hs

fromMaryAssetName :: AssetName -> C.AssetName
fromMaryAssetName (AssetName n) = C.AssetName $ Short.fromShort n -- from cardano-api:src/Cardano/Api/Value.hs

mkMintTokenIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m MintTokenEventIndexer
mkMintTokenIndexer dbPath = do
  let createMintPolicyEvent =
        [sql|CREATE TABLE IF NOT EXISTS
              minting_policy_events
              ( slotNo INT NOT NULL
              , txId BLOB NOT NULL
              , txIndexInBlock BLOB NOT NULL
              , policyId BLOB NOT NULL
              , assetName TEXT NOT NULL
              , quantity INT NOT NULL
              , redeemerHash BLOB
              , redeemerData BLOB
              )|]
      createMintPolicyIdIndex =
        [sql|CREATE INDEX IF NOT EXISTS
             minting_policy_events__txId_policyId
             ON minting_policy_events (txId, policyId)|]

  let mintEventInsertQuery :: SQL.Query -- Utxo table SQL statement
      mintEventInsertQuery =
        [sql|INSERT
               INTO minting_policy_events (
                 slotNo,
                 txIndexInBlock,
                 txId,
                 policyId,
                 assetName,
                 quantity,
                 redeemerHash,
                 redeemerData
              ) VALUES
              (?, ?, ?, ?, ?, ?, ?, ?)|]
      mintCreation = [createMintPolicyEvent, createMintPolicyIdIndex, Sync.syncTableCreation]
      mintInsert = [Core.SQLInsertPlan timedMintEventsToTimedMintEventList mintEventInsertQuery]

  Core.mkSqliteIndexer
    dbPath
    mintCreation
    [mintInsert]
    (Just Sync.syncInsertPlan)
    [ Core.SQLRollbackPlan "minting_policy_events" "slotNo" C.chainPointToSlotNo
    , Sync.syncRollbackPlan
    ]
    Sync.syncLastPointQuery

timedMintEventsToTimedMintEventList
  :: Core.Timed point MintTokenEvents
  -> [Core.Timed point MintTokenEvent]
timedMintEventsToTimedMintEventList =
  traverse $ toListOf $ mintTokenEventsAssets . folded
