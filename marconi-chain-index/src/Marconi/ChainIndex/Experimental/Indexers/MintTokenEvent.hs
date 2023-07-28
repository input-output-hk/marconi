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
, blockHeaderHash INT NOT NULL
, blockNo INT NOT NULL
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
module Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Api (
  EraCrypto,
  RdmrPtr (RdmrPtr),
  Redeemers (Redeemers),
  StandardCrypto,
  getPlutusData,
  hashData,
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
import Control.Lens (folded, makeLenses, toListOf, (^.), (^?))
import Data.ByteString.Short qualified as Short
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Time.Clock.POSIX (POSIXTime)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers.LastSync (createLastSyncTable)
import Marconi.ChainIndex.Types (
  TxIndexInBlock,
 )
import Marconi.Core.Experiment qualified as Core

type MintTokenEventIndexer = Core.SQLiteIndexer MintTokenEvents

-- | Minting events given for each block.
newtype MintTokenEvents = MintTokenEvents
  { _mintTokenEventsAssets :: [MintTokenEvent]
  }
  deriving (Show, Eq, Ord)

data MintTokenEvent = MintTokenEvent
  { _mintBurnTokenEventLocation :: !MintTokenEventLocation
  , _mintBurnTokenEventAsset :: !MintAsset
  }
  deriving (Show, Eq, Ord, Generic)

data MintTokenEventLocation = MintTokenEventLocation
  { _mintBurnTokenEventBlockNo :: !C.BlockNo
  , _mintBurnTokenEventIndexInBlock :: !TxIndexInBlock
  , _mintBurnTokenEventTxId :: !C.TxId
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

$(makeLenses ''MintTokenEvents)
$(makeLenses ''MintTokenEvent)
$(makeLenses ''MintTokenEventLocation)
$(makeLenses ''MintAsset)
$(makeLenses ''MintAssetRedeemer)

instance SQL.ToRow (Core.Timed C.ChainPoint MintTokenEvent) where
  toRow te = SQL.toRow (te ^. Core.point) ++ SQL.toRow (te ^. Core.event)

instance SQL.ToRow MintTokenEvent where
  toRow e =
    SQL.toRow (e ^. mintBurnTokenEventLocation)
      ++ SQL.toRow (e ^. mintBurnTokenEventAsset)

instance SQL.ToRow MintTokenEventLocation where
  toRow l =
    SQL.toRow
      [ SQL.toField $ l ^. mintBurnTokenEventBlockNo
      , SQL.toField $ l ^. mintBurnTokenEventIndexInBlock
      , SQL.toField $ l ^. mintBurnTokenEventTxId
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

type instance Core.Point (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime) = C.ChainPoint

type instance Core.Point MintTokenEvents = C.ChainPoint

-- TODO Move to 'marconi-cardano'. This is repetitive between indexers.
instance Core.HasGenesis C.ChainPoint where
  genesis = C.ChainPointAtGenesis

-- | Create a worker for the MintTokenEvent indexer
mintBurnTokenEventWorker
  :: FilePath
  -> IO
      ( MVar MintTokenEventIndexer
      , Core.Worker (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime) C.ChainPoint
      )
mintBurnTokenEventWorker dbPath = do
  sqliteIndexer <- mkSqliteIndexer dbPath -- TODO handle error
  Core.createWorker "MintTokenEvent" extractEvents sqliteIndexer

extractEvents :: (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime) -> IO (Maybe MintTokenEvents)
extractEvents (C.BlockInMode (C.Block (C.BlockHeader _slotNo _bhh blockNo) txs) _, _, _) = do
  pure $ Just $ MintTokenEvents $ concatMap (uncurry extractEventsFromTx) $ zip [0 ..] txs
  where
    extractEventsFromTx :: TxIndexInBlock -> C.Tx era -> [MintTokenEvent]
    extractEventsFromTx txIndexInBlock (C.Tx txb _) =
      let mintEventLocation = MintTokenEventLocation blockNo txIndexInBlock (C.getTxId txb)
       in fmap (MintTokenEvent mintEventLocation) $ extactMintEventsFromTxBody txb

    extactMintEventsFromTxBody :: C.TxBody era -> [MintAsset]
    extactMintEventsFromTxBody txb = case txb of
      C.ShelleyTxBody era shelleyTx _ _ _ _ -> case era of
        C.ShelleyBasedEraShelley -> []
        C.ShelleyBasedEraAllegra -> []
        C.ShelleyBasedEraMary -> []
        C.ShelleyBasedEraAlonzo -> getPolicyData txb $ LA.atbMint shelleyTx
        C.ShelleyBasedEraBabbage -> getPolicyData txb $ LB.btbMint shelleyTx
        C.ShelleyBasedEraConway -> getPolicyData txb $ LC.ctbMint shelleyTx
      _byronTxBody -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

getPolicyData
  :: forall era
   . ( Ledger.Era (C.ShelleyLedgerEra era)
     , EraCrypto (C.ShelleyLedgerEra era) ~ StandardCrypto
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
      redeemerHash = C.ScriptDataHash . hashData
      toAssetRedeemer r = MintAssetRedeemer (fromAlonzoData r) (redeemerHash r)
  (ix, (policyId', assetName, quantity)) <- zip [0 ..] $ flattenMultiAsset m
  let redeemer = findRedeemer ix
  pure $
    MintAsset
      (fromMaryPolicyID policyId')
      (fromMaryAssetName assetName)
      (C.Quantity quantity)
      (toAssetRedeemer <$> redeemer)

fromMaryPolicyID :: PolicyID StandardCrypto -> C.PolicyId
fromMaryPolicyID (PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- from cardano-api:src/Cardano/Api/Value.hs

fromMaryAssetName :: AssetName -> C.AssetName
fromMaryAssetName (AssetName n) = C.AssetName $ Short.fromShort n -- from cardano-api:src/Cardano/Api/Value.hs

fromAlonzoData :: LA.Data ledgerera -> C.ScriptData
fromAlonzoData = C.fromPlutusData . getPlutusData -- from cardano-api:src/Cardano/Api/ScriptData.hs

mkSqliteIndexer :: FilePath -> IO MintTokenEventIndexer
mkSqliteIndexer dbPath = do
  c <- SQL.open dbPath

  SQL.execute_ c "PRAGMA journal_mode=WAL"

  SQL.execute_
    c
    [sql|CREATE TABLE IF NOT EXISTS
          minting_policy_events
          ( slotNo INT NOT NULL
          , blockHeaderHash INT NOT NULL
          , blockNo INT NOT NULL
          , txId BLOB NOT NULL
          , txIndexInBlock BLOB NOT NULL
          , policyId BLOB NOT NULL
          , assetName TEXT NOT NULL
          , quantity INT NOT NULL
          , redeemerHash BLOB
          , redeemerData BLOB
          )|]

  SQL.execute_
    c
    [sql|CREATE INDEX IF NOT EXISTS
         minting_policy_events__txId_policyId
         ON minting_policy_events (txId, policyId)|]

  createLastSyncTable c

  let mintEventInsertQuery :: SQL.Query -- Utxo table SQL statement
      mintEventInsertQuery =
        [sql|INSERT
               INTO minting_policy_events (
                 slotNo,
                 blockHeaderHash,
                 blockNo,
                 txIndexInBlock,
                 txId,
                 policyId,
                 assetName,
                 quantity,
                 redeemerHash,
                 redeemerData
              ) VALUES
              (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|]

  pure $
    Core.SQLiteIndexer
      c
      [
        [ Core.SQLInsertPlan timedMintEventsToTimedMintEventList mintEventInsertQuery
        ]
      ]
      [ Core.SQLRollbackPlan "minting_policy_events" "slotNo" C.chainPointToSlotNo
      ]
      Core.genesis

timedMintEventsToTimedMintEventList
  :: Core.Timed point MintTokenEvents
  -> [Core.Timed point MintTokenEvent]
timedMintEventsToTimedMintEventList =
  traverse $ toListOf $ mintTokenEventsAssets . folded
