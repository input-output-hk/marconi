{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Mint/burn event indexer, the result of which is an sqlite database
   'mintburn.db' which has a table 'minting_policy_events' with the
   following fields:

     - slotNo          INT NOT NULL
     - blockHeaderHash INT NOT NULL
     - txId            BLOB NOT NULL
     - txIndexInBlock  INT NOT NULL
     - policyId        BLOB NOT NULL
     - assetName       TEXT NOT NULL
     - quantity        INT NOT NULL
     - redeemerIx      INT NOT NULL
     - redeemerData    BLOB NOT NULL
     - redeemerHash    BLOB NOT NULL
-}
module Marconi.ChainIndex.Indexers.MintBurn where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as LA
import Cardano.Ledger.Alonzo.Scripts.Data qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWits qualified as LA
import Cardano.Ledger.Api.Scripts.Data qualified as Ledger.Api
import Cardano.Ledger.Babbage.Tx qualified as LB
import Cardano.Ledger.Conway.TxBody qualified as LC
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as LM
import Control.Lens (makeLenses, view, (&), (^.))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.ByteString.Short qualified as Short
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Debug.Trace qualified
import GHC.Generics (Generic)
import Marconi.ChainIndex.Error (
  IndexerError (CantInsertEvent, CantQueryIndexer, CantRollback, CantStartIndexer),
  liftSQLError,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam, TxIndexInBlock)
import Marconi.ChainIndex.Utils (chainPointOrGenesis)
import Marconi.Core.Storable (StorableMonad)
import Marconi.Core.Storable qualified as RI
import Ouroboros.Consensus.Shelley.Eras qualified as OEra

-- * Event

-- | The info about the tx that does the minting
data TxMintInfo = TxMintInfo
  { txMintTxId :: C.TxId
  , txMintIndexInBlock :: TxIndexInBlock
  , txMintAsset :: NE.NonEmpty MintAsset
  }
  deriving (Show, Eq, Ord)

-- | Gather all the relevant minting in a block
data TxMintEvent = TxMintEvent
  { txMintEventSlotNo :: !C.SlotNo
  , txMintEventBlockHeaderHash :: !(C.Hash C.BlockHeader)
  , txMintEventBlockNo :: !C.BlockNo
  , txMintEventTxAssets :: ![TxMintInfo]
  }
  deriving (Show, Eq, Ord)

data MintAsset = MintAsset
  { mintAssetPolicyId :: !C.PolicyId
  , mintAssetAssetName :: !C.AssetName
  , mintAssetQuantity :: !C.Quantity
  , mintAssetRedeemerIdx :: !Word64
  , mintAssetRedeemerData :: !C.ScriptData
  , mintAssetRedeemerHash :: !(C.Hash C.ScriptData)
  }
  deriving (Show, Eq, Ord)

-- | Extract the mint events from a block
toUpdate :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)) -> C.BlockInMode C.CardanoMode -> TxMintEvent
toUpdate mAssets (C.BlockInMode (C.Block (C.BlockHeader slotNo blockHeaderHash blockNo) txs) _) =
  let assets = mapMaybe (uncurry $ txMints mAssets) $ zip [0 ..] txs
   in TxMintEvent slotNo blockHeaderHash blockNo assets

-- | Extracs TxMintInfo from a Tx
txMints :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)) -> TxIndexInBlock -> C.Tx era -> Maybe TxMintInfo
txMints mAssets ix (C.Tx txb _) =
  let isTargetOf token pId an =
        mintAssetPolicyId token == pId
          && maybe True (mintAssetAssetName token ==) an
      isTarget m = case mAssets of
        Just xs -> any (uncurry (isTargetOf m)) xs
        Nothing -> True
      assets = txbMints txb
   in case filter isTarget assets of
        x : xs -> Just $ TxMintInfo (C.getTxId txb) ix (x NE.:| xs)
        [] -> Nothing

txbMints :: C.TxBody era -> [MintAsset]
txbMints txb = case txb of
  C.ShelleyTxBody era shelleyTx _ _ _ _ -> case era of
    C.ShelleyBasedEraShelley -> []
    C.ShelleyBasedEraAllegra -> []
    C.ShelleyBasedEraMary -> []
    C.ShelleyBasedEraAlonzo -> getPolicyData txb $ LA.atbMint shelleyTx
    C.ShelleyBasedEraBabbage -> getPolicyData txb $ LB.btbMint shelleyTx
    C.ShelleyBasedEraConway -> getPolicyData txb $ LC.ctbMint shelleyTx
  _byronTxBody -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

-- * Helpers

getPolicyData
  :: forall era
   . (Ledger.Era (C.ShelleyLedgerEra era), OEra.EraCrypto (C.ShelleyLedgerEra era) ~ OEra.StandardCrypto)
  => C.TxBody era
  -> LM.MultiAsset OEra.StandardCrypto
  -> [MintAsset]
getPolicyData txb (LM.MultiAsset m) = do
  let policyIdList = Map.toList m
      getPolicyId index' = policyIdList !! fromIntegral index'

  ((maryPolicyID, assets), index'', (redeemer, _)) <- map (\(index', data_) -> (getPolicyId index', index', data_)) mintRedeemers
  let redeemerHash = C.ScriptDataHash $ Ledger.Api.hashData redeemer
  (assetName, quantity) :: (LM.AssetName, Integer) <- Map.toList assets
  pure $
    MintAsset
      (fromMaryPolicyID maryPolicyID)
      (fromMaryAssetName assetName)
      (C.Quantity quantity)
      index''
      (fromAlonzoData redeemer)
      redeemerHash
  where
    mintRedeemers :: [(Word64, (LB.Data (C.ShelleyLedgerEra era), LA.ExUnits))]
    mintRedeemers =
      txRedeemers txb
        & Map.toList
        & filter (\(LA.RdmrPtr tag _, _) -> tag == LA.Mint)
        & map (\(LA.RdmrPtr _ w, a) -> (w, a))

    txRedeemers :: C.TxBody era -> Map.Map LA.RdmrPtr (LB.Data (C.ShelleyLedgerEra era), LA.ExUnits)
    txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
      C.TxBodyScriptData _proof _datum (LA.Redeemers redeemers) -> redeemers
      C.TxBodyNoScriptData -> mempty
    txRedeemers _ = mempty

-- ** Copy-paste

fromMaryPolicyID :: LM.PolicyID OEra.StandardCrypto -> C.PolicyId
fromMaryPolicyID (LM.PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- from cardano-api:src/Cardano/Api/Value.hs

fromMaryAssetName :: LM.AssetName -> C.AssetName
fromMaryAssetName (LM.AssetName n) = C.AssetName $ Short.fromShort n -- from cardano-api:src/Cardano/Api/Value.hs

fromAlonzoData :: LA.Data ledgerera -> C.ScriptData
fromAlonzoData = C.fromPlutusData . LA.getPlutusData -- from cardano-api:src/Cardano/Api/ScriptData.hs

-- * Sqlite

data TxMintRow = TxMintRow
  { _txMintRowSlotNo :: !C.SlotNo
  , _txMintRowBlockHeaderHash :: !(C.Hash C.BlockHeader)
  , _txMintRowBlockNo :: !C.BlockNo
  , _txMintRowTxIx :: !TxIndexInBlock
  , _txMintRowTxId :: !C.TxId
  , _txMintRowPolicyId :: !C.PolicyId
  , _txMintRowAssetName :: !C.AssetName
  , _txMintRowQuantity :: !C.Quantity
  , _txMintRowRedeemerIdx :: !Word64
  , _txMintRowRedeemerData :: !C.ScriptData
  , _txMintRowRedeemerHash :: !(C.Hash C.ScriptData)
  }
  deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

makeLenses 'TxMintRow

instance FromJSON TxMintRow where
  parseJSON (Object v) =
    TxMintRow
      <$> v .: "slotNo"
      <*> v .: "blockHeaderHash"
      <*> v .: "blockNo"
      <*> v .: "txIndexInBlock"
      <*> v .: "txId"
      <*> v .: "policyId"
      <*> v .: "assetName"
      <*> v .: "quantity"
      <*> v .: "redeemerIdx"
      <*> v .: "redeemerData"
      <*> v .: "redeemerHash"
  parseJSON _ = mempty

instance ToJSON TxMintRow where
  toJSON row =
    object
      [ "slotNo" .= _txMintRowSlotNo row
      , "blockHeaderHash" .= _txMintRowBlockHeaderHash row
      , "blockNo" .= _txMintRowBlockNo row
      , "txIndexInBlock" .= _txMintRowTxIx row
      , "txId" .= _txMintRowTxId row
      , "policyId" .= _txMintRowPolicyId row
      , "assetName" .= _txMintRowAssetName row
      , "quantity" .= _txMintRowQuantity row
      , "redeemerIdx" .= _txMintRowRedeemerIdx row
      , "redeemerData" .= _txMintRowRedeemerData row
      , "redeemerHash" .= _txMintRowRedeemerHash row
      ]

sqliteInit :: SQL.Connection -> IO ()
sqliteInit c = liftIO $ do
  SQL.execute_
    c
    " CREATE TABLE IF NOT EXISTS        \
    \   minting_policy_events           \
    \   ( slotNo          INT NOT NULL  \
    \   , blockHeaderHash INT NOT NULL  \
    \   , blockNo         INT NOT NULL  \
    \   , txId            BLOB NOT NULL \
    \   , txIndexInBlock  BLOB NOT NULL \
    \   , policyId        BLOB NOT NULL \
    \   , assetName       TEXT NOT NULL \
    \   , quantity        INT NOT NULL  \
    \   , redeemerIx      INT NOT NULL  \
    \   , redeemerData    BLOB NOT NULL \
    \   , redeemerHash    BLOB NOT NULL)"
  SQL.execute_
    c
    " CREATE INDEX IF NOT EXISTS               \
    \    minting_policy_events__txId_policyId  \
    \ ON minting_policy_events (txId, policyId)"

sqliteInsert :: SQL.Connection -> [TxMintEvent] -> IO ()
sqliteInsert c es =
  let rows = toRows =<< toList es
      template =
        "INSERT INTO minting_policy_events          \
        \( slotNo, blockHeaderHash, blockNo,        \
        \txIndexInBlock, txId, policyId, assetName, \
        \ quantity, redeemerIx, redeemerData,       \
        \ redeemerHash )                            \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   in case rows of
        [] -> pure ()
        xs -> Debug.Trace.traceShow xs $ SQL.executeMany c template xs

toRows :: TxMintEvent -> [TxMintRow]
toRows e = do
  TxMintInfo txId txIx txMintAssets <- txMintEventTxAssets e
  mintAsset <- NE.toList txMintAssets
  pure $
    TxMintRow
      (txMintEventSlotNo e)
      (txMintEventBlockHeaderHash e)
      (txMintEventBlockNo e)
      txIx
      txId
      (mintAssetPolicyId mintAsset)
      (mintAssetAssetName mintAsset)
      (mintAssetQuantity mintAsset)
      (mintAssetRedeemerIdx mintAsset)
      (mintAssetRedeemerData mintAsset)
      (mintAssetRedeemerHash mintAsset)

-- | Input rows must be sorted by C.SlotNo.
fromRows :: [TxMintRow] -> [TxMintEvent]
fromRows rows = do
  rs@(r :| _) <- NE.groupBy ((==) `on` slotNo) rows -- group by SlotNo
  pure $ TxMintEvent (slotNo r) (hash r) (blockNo r) $ do
    rs' <- NE.groupBy ((==) `on` assetKey) rs
    pure $ TxMintInfo (txId r) (txIx r) (rowToMintAsset <$> rs')
  where
    assetKey row = (txId row, txIx row)
    txId = view txMintRowTxId
    txIx = view txMintRowTxIx
    slotNo = view txMintRowSlotNo
    blockNo = view txMintRowBlockNo
    hash = view txMintRowBlockHeaderHash
    rowToMintAsset :: TxMintRow -> MintAsset
    rowToMintAsset row =
      MintAsset
        (row ^. txMintRowPolicyId)
        (row ^. txMintRowAssetName)
        (row ^. txMintRowQuantity)
        (row ^. txMintRowRedeemerIdx)
        (row ^. txMintRowRedeemerData)
        (row ^. txMintRowRedeemerHash)

queryStoredTxMintEvents
  :: SQL.Connection
  -> ([SQL.Query], [NamedParam])
  -> IO [TxMintEvent]
queryStoredTxMintEvents sqlCon (conditions, params) =
  fmap fromRows $ SQL.queryNamed sqlCon (SQL.Query query) params
  where
    allConditions = Text.intercalate " AND " $ fmap SQL.fromQuery conditions
    whereClause = if allConditions == "" then "" else "WHERE " <> allConditions
    query =
      " SELECT slotNo, blockHeaderHash, blockNo, txIndexInBlock, txId, \
      \        policyId, assetName, quantity, redeemerIx, redeemerData \
      \      , redeemerHash                                            \
      \ FROM minting_policy_events                                     \
      \   "
        <> whereClause
        <> "                                        \
           \ ORDER BY blockNo ASC, txIndexInBlock ASC"

groupBySlotAndHash :: [TxMintEvent] -> [TxMintEvent]
groupBySlotAndHash events =
  events
    & sort
    & groupBy (\e1 e2 -> txMintEventSlotNo e1 == txMintEventSlotNo e2 && txMintEventBlockHeaderHash e1 == txMintEventBlockHeaderHash e2)
    & mapMaybe buildTxMintEvent
  where
    buildTxMintEvent [] = Nothing
    buildTxMintEvent (e : es) =
      Just $
        TxMintEvent (txMintEventSlotNo e) (txMintEventBlockHeaderHash e) (txMintEventBlockNo e) $
          txMintEventTxAssets =<< (e : es)

-- * Indexer

data MintBurnHandle = MintBurnHandle
  { sqlConnection :: !SQL.Connection
  , securityParam :: !SecurityParam
  }

type MintBurnIndexer = RI.State MintBurnHandle

type instance RI.StorablePoint MintBurnHandle = C.ChainPoint

type instance RI.StorableMonad MintBurnHandle = ExceptT IndexerError IO

newtype instance RI.StorableEvent MintBurnHandle
  = MintBurnEvent TxMintEvent
  deriving (Show)

data instance RI.StorableQuery MintBurnHandle
  = -- | Query all transactions that minted a specific 'AssetId' until an upper bound slot in the
    -- blockchain. If the upper bound slot is 'Nothing', then we return everything.
    QueryByAssetId C.PolicyId C.AssetName (Maybe C.SlotNo)
  | -- | Query all transactions that minted 'AssetId's until an upper bound slot in the blockchain. If
    -- the upper bound slot is 'Nothing', then we return everything.
    QueryAllMintBurn (Maybe C.SlotNo)
  | -- | Query all transactions that burned a specific 'AssetId' until an upper bound slot in the
    -- blockchain. If the upper bound slot is 'Nothing', then we return everything.
    QueryBurnByAssetId C.PolicyId C.AssetName (Maybe C.SlotNo)
  | -- | Query all transactions that burned 'AssetId's until an upper bound slot in the blockchain. If
    -- the upper bound slot is 'Nothing', then we return everything.
    QueryAllBurn (Maybe C.SlotNo)
  deriving (Show)

newtype instance RI.StorableResult MintBurnHandle
  = MintBurnResult [TxMintRow]
  deriving (Show)

instance RI.Queryable MintBurnHandle where
  queryStorage memoryEvents (MintBurnHandle sqlCon _k) query = liftSQLError CantQueryIndexer $ do
    storedEvents <- queryStoredTxMintEvents sqlCon $ mkSqliteConditions query
    pure $ MintBurnResult $ do
      TxMintEvent slotNo blockHeaderHash blockNo txAssets <- storedEvents <> filteredMemoryEvents
      TxMintInfo txId txIx mintAssets <- txAssets
      MintAsset policyId assetName quantity redeemerIx redeemerData redeemerHash <- NE.toList mintAssets
      pure $
        TxMintRow
          slotNo
          blockHeaderHash
          blockNo
          txIx
          txId
          policyId
          assetName
          quantity
          redeemerIx
          redeemerData
          redeemerHash
    where
      filteredMemoryEvents :: [TxMintEvent]
      filteredMemoryEvents = coerce $ fromRows $ filter rowFilter $ toRows =<< (coerce $ toList memoryEvents)

      -- Applies every predicate to row, when all are True, then result is True.
      rowFilter :: TxMintRow -> Bool
      rowFilter row = let filters = mkRowPredicates query in all ($ row) filters

      -- \* Filter in-memory events

      upToSlot :: Maybe C.SlotNo -> [TxMintRow -> Bool]
      upToSlot = \case
        Just slotNo -> [\row -> _txMintRowSlotNo row <= slotNo]
        Nothing -> []

      matchesAssetId :: C.PolicyId -> C.AssetName -> TxMintRow -> Bool
      matchesAssetId policyId assetName row =
        _txMintRowPolicyId row == policyId && _txMintRowAssetName row == assetName

      isBurn :: TxMintRow -> Bool
      isBurn row = _txMintRowQuantity row < 0

      mkRowPredicates :: RI.StorableQuery MintBurnHandle -> [TxMintRow -> Bool]
      mkRowPredicates = \case
        QueryAllMintBurn maybeSlotNo -> upToSlot maybeSlotNo
        QueryAllBurn maybeSlotNo -> upToSlot maybeSlotNo <> [isBurn]
        QueryByAssetId policyId assetName maybeSlotNo -> upToSlot maybeSlotNo <> [matchesAssetId policyId assetName]
        QueryBurnByAssetId policyId assetName maybeSlotNo -> upToSlot maybeSlotNo <> [matchesAssetId policyId assetName, isBurn]

      -- \* Filter sqlite events

      mkSqliteConditions :: RI.StorableQuery MintBurnHandle -> ([SQL.Query], [NamedParam])
      mkSqliteConditions = \case
        QueryAllMintBurn slotNo ->
          mkUpperBoundCondition slotNo
        QueryByAssetId policyId assetName slotNo ->
          mkUpperBoundCondition slotNo
            <> mkAssetIdCondition policyId assetName
        QueryAllBurn slotNo ->
          mkUpperBoundCondition slotNo
            <> (["quantity < 0"], [])
        QueryBurnByAssetId policyId assetName slotNo ->
          mkUpperBoundCondition slotNo
            <> mkAssetIdCondition policyId assetName
            <> (["quantity < 0"], [])

      mkAssetIdCondition :: C.PolicyId -> C.AssetName -> ([SQL.Query], [NamedParam])
      mkAssetIdCondition policyId assetName =
        ( ["policyId = :policyId", "assetName = :assetName"]
        , [":policyId" := policyId, ":assetName" := assetName]
        )

      mkUpperBoundCondition :: Maybe C.SlotNo -> ([SQL.Query], [NamedParam])
      mkUpperBoundCondition = \case
        Nothing -> ([], [])
        Just s -> (["slotNo <= :slotNo"], [":slotNo" := s])

instance RI.HasPoint (RI.StorableEvent MintBurnHandle) C.ChainPoint where
  getPoint (MintBurnEvent e) = C.ChainPoint (txMintEventSlotNo e) (txMintEventBlockHeaderHash e)

instance RI.Buffered MintBurnHandle where
  persistToStorage events h@(MintBurnHandle sqlCon _k) =
    liftSQLError CantInsertEvent $
      do
        sqliteInsert sqlCon (map coerce $ toList events)
        pure h

  getStoredEvents (MintBurnHandle sqlCon k) =
    liftSQLError CantQueryIndexer $
      do
        fmap MintBurnEvent . fromRows <$> SQL.query sqlCon query (SQL.Only k)
    where
      query =
        " SELECT slotNo, blockHeaderHash, blockNo, txIndexInBlock, txId,       \
        \        policyId, assetName, quantity, redeemerIx, redeemerData       \
        \   FROM minting_policy_events                                         \
        \  WHERE slotNo >= (SELECT MAX(slotNo) - ? FROM minting_policy_events) \
        \  ORDER BY slotNo DESC, txId                                          "

instance RI.Resumable MintBurnHandle where
  resumeFromStorage (MintBurnHandle c _) =
    liftSQLError CantQueryIndexer $
      fmap chainPointOrGenesis $
        SQL.query_ c "SELECT slotNo, blockHeaderHash FROM minting_policy_events ORDER BY slotNo DESC LIMIT 1"

instance RI.Rewindable MintBurnHandle where
  rewindStorage cp h@(MintBurnHandle sqlCon _k) =
    liftSQLError CantRollback $ doRewind >> pure h
    where
      doRewind = case cp of
        C.ChainPoint slotNo _ ->
          SQL.execute sqlCon "DELETE FROM minting_policy_events WHERE slotNo > ?" (SQL.Only slotNo)
        C.ChainPointAtGenesis ->
          SQL.execute_ sqlCon "DELETE FROM minting_policy_events"

open :: FilePath -> SecurityParam -> StorableMonad MintBurnHandle MintBurnIndexer
open dbPath bufferSize = do
  c <- liftSQLError CantStartIndexer $ SQL.open dbPath
  lift $ SQL.execute_ c "PRAGMA journal_mode=WAL"
  lift $ sqliteInit c
  RI.emptyState (fromEnum bufferSize) (MintBurnHandle c bufferSize)
