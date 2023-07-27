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
     - redeemerData    BLOB NOT NULL
     - redeemerHash    BLOB NOT NULL
-}
module Marconi.ChainIndex.Indexers.MintBurn where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Scripts.Data qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWits qualified as LA
import Cardano.Ledger.Api.Scripts.Data qualified as Ledger.Api
import Cardano.Ledger.Babbage.Tx qualified as LB
import Cardano.Ledger.Conway.TxBody qualified as LC
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as LA
import Cardano.Ledger.Mary.Value qualified as LM
import Control.Exception (Exception)
import Control.Lens (makeLenses, view, (^.))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString.Short qualified as Short
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (find)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as Text
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.FromRow qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Error (
  IndexerError (
    CantInsertEvent,
    CantQueryIndexer,
    CantRollback,
    CantStartIndexer,
    QueryError
  ),
  liftSQLError,
 )
import Marconi.ChainIndex.Indexers.LastSync (
  createLastSyncTable,
  insertLastSyncPoints,
  queryLastSyncPoint,
  rollbackLastSyncPoints,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam, TxIndexInBlock)
import Marconi.Core.Storable (StorableMonad)
import Marconi.Core.Storable qualified as RI
import Ouroboros.Consensus.Shelley.Eras qualified as OEra

-- * Event

-- | Gather all the relevant minting in a block
data TxMintEvent = TxMintEvent
  { txMintEventSlotNo :: !C.SlotNo
  , txMintEventBlockHeaderHash :: !(C.Hash C.BlockHeader)
  , txMintEventBlockNo :: !C.BlockNo
  , txMintEventTxAssets :: ![TxMintInfo]
  }
  deriving (Show, Eq, Ord)

-- | The info about the tx that does the minting
data TxMintInfo = TxMintInfo
  { txMintTxId :: C.TxId
  , txMintIndexInBlock :: TxIndexInBlock
  , txMintAsset :: NE.NonEmpty MintAsset
  }
  deriving (Show, Eq, Ord)

data MintAsset = MintAsset
  { mintAssetPolicyId :: !C.PolicyId
  , mintAssetAssetName :: !C.AssetName
  , mintAssetQuantity :: !C.Quantity
  , mintAssetRedeemer :: !(Maybe MintAssetRedeemer)
  -- ^ Nothing if  the policyId is a simple script
  }
  deriving (Show, Eq, Ord)

data MintAssetRedeemer = MintAssetRedeemer
  { mintAssetRedeemerData :: !C.ScriptData
  , mintAssetRedeemerHash :: !(C.Hash C.ScriptData)
  }
  deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

-- | Errors that can occurs when you query the indexer
data MintBurnQueryError
  = -- | The lower slot that correspond to the given TxId is greater or equal to the provider higher
    -- slot
    InvalidInterval C.TxId C.SlotNo C.SlotNo
  | -- | The provided @TxId@ for the lower slot cannot be resolved
    TxNotIndexed C.TxId
  deriving (Show, Eq)

instance Exception MintBurnQueryError

-- | Extract the mint events from a block
toUpdate
  :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -> C.BlockInMode C.CardanoMode
  -> RI.StorableEvent MintBurnHandle
toUpdate mAssets (C.BlockInMode (C.Block (C.BlockHeader slotNo blockHeaderHash blockNo) txs) _) =
  let assets = mapMaybe (uncurry $ txMints mAssets) $ zip [0 ..] txs
   in MintBurnEvent $ TxMintEvent slotNo blockHeaderHash blockNo assets

-- | Extracs TxMintInfo from a Tx
txMints
  :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)) -> TxIndexInBlock -> C.Tx era -> Maybe TxMintInfo
txMints mAssets ix (C.Tx txb _) =
  let isTargetOf token pId an =
        mintAssetPolicyId token == pId
          && maybe True (mintAssetAssetName token ==) an
      isTarget m = case mAssets of
        Just xs -> any (uncurry (isTargetOf m)) xs
        Nothing -> True
      assets = txbMints txb
      txid = C.getTxId txb
   in case filter isTarget assets of
        x : xs -> Just $ TxMintInfo txid ix (x NE.:| xs)
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
getPolicyData txb m = do
  let txRedeemers :: C.TxBody era -> [(LA.RdmrPtr, LB.Data (C.ShelleyLedgerEra era))]
      txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
        C.TxBodyScriptData _proof _datum (LA.Redeemers redeemers) -> Map.toList $ fmap fst redeemers
        C.TxBodyNoScriptData -> mempty
      txRedeemers _ = mempty
      findRedeemerByIndex ix (LA.RdmrPtr _ w, _) = w == ix
      findRedeemer ix = snd <$> List.find (findRedeemerByIndex ix) (txRedeemers txb)
      redeemerHash = C.ScriptDataHash . Ledger.Api.hashData
      toAssetRedeemer r = MintAssetRedeemer (fromAlonzoData r) (redeemerHash r)
  (ix, (policyId', assetName, quantity)) <- zip [0 ..] $ LA.flattenMultiAsset m
  let redeemer = findRedeemer ix
  pure $
    MintAsset
      (fromMaryPolicyID policyId')
      (fromMaryAssetName assetName)
      (C.Quantity quantity)
      (toAssetRedeemer <$> redeemer)

-- ** Copy-paste

fromMaryPolicyID :: LM.PolicyID OEra.StandardCrypto -> C.PolicyId
fromMaryPolicyID (LM.PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- from cardano-api:src/Cardano/Api/Value.hs

fromMaryAssetName :: LM.AssetName -> C.AssetName
fromMaryAssetName (LM.AssetName n) = C.AssetName $ Short.fromShort n -- from cardano-api:src/Cardano/Api/Value.hs

fromAlonzoData :: LA.Data ledgerera -> C.ScriptData
fromAlonzoData = C.fromPlutusData . LA.getPlutusData -- from cardano-api:src/Cardano/Api/ScriptData.hs

-- * Indexer

data MintBurnHandle = MintBurnHandle
  { sqlConnection :: !SQL.Connection
  , securityParam :: !SecurityParam
  }

type MintBurnIndexer = RI.State MintBurnHandle

type instance RI.StorablePoint MintBurnHandle = C.ChainPoint

type instance RI.StorableMonad MintBurnHandle = ExceptT (IndexerError MintBurnQueryError) IO

newtype instance RI.StorableEvent MintBurnHandle = MintBurnEvent {getEvent :: TxMintEvent}
  deriving (Show)

data instance RI.StorableQuery MintBurnHandle
  = -- | Query all transactions that minted a specific 'AssetId' until an upper bound slot in the
    -- blockchain. If the upper bound slot is 'Nothing', then we return everything.
    QueryByAssetId C.PolicyId (Maybe C.AssetName) (Maybe C.SlotNo)
  | -- | Query all transactions that minted 'AssetId's until an upper bound slot in the blockchain. If
    -- the upper bound slot is 'Nothing', then we return everything.
    QueryAllMintBurn (Maybe C.SlotNo)
  | -- | Query all transactions that burned until an upper bound slot
    -- blockchain. If the upper bound slot is 'Nothing', then we return everything.
    QueryBurnByAssetId C.PolicyId (Maybe C.AssetName) (Maybe C.SlotNo) (Maybe C.TxId)
  | -- | Query all transactions that burned 'AssetId's until an upper bound slot in the blockchain. If
    -- the upper bound slot is 'Nothing', then we return everything.
    QueryAllBurn (Maybe C.SlotNo)
  deriving (Show)

newtype instance RI.StorableResult MintBurnHandle
  = MintBurnResult [TxMintRow]
  deriving (Show)

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
  , _txMintRowRedeemer :: !(Maybe MintAssetRedeemer)
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses 'TxMintRow

instance SQL.FromRow TxMintRow where
  fromRow = do
    let redeemerFromRow (Just d) (Just h) = pure $ Just $ MintAssetRedeemer d h
        redeemerFromRow Nothing Nothing = pure Nothing
        redeemerFromRow _ _ =
          SQL.fieldWith $ \field' ->
            SQL.returnError
              SQL.UnexpectedNull
              field'
              "Invalid redeemer: Some fields are null, other aren't"
    slotNo <- SQL.field
    blockHeaderHash <- SQL.field
    blockNo <- SQL.field
    txIx <- SQL.field
    txId <- SQL.field
    policyId' <- SQL.field
    assetName <- SQL.field
    qty <- SQL.field
    redeemerData <- SQL.field
    redeemerHash <- SQL.field
    redeemer <- redeemerFromRow redeemerData redeemerHash
    pure $ TxMintRow slotNo blockHeaderHash blockNo txIx txId policyId' assetName qty redeemer

instance FromJSON MintAssetRedeemer where
  parseJSON (Object v) = do
    redeemerData <- v .:? "redeemerData"
    redeemerHash <- v .:? "redeemerHash"
    case MintAssetRedeemer <$> redeemerData <*> redeemerHash of
      Just x -> pure x
      Nothing -> fail "invalid object for asset redeemer"
  parseJSON _ = mempty

instance FromJSON TxMintRow where
  parseJSON (Object v) = do
    redeemerData <- v .:? "redeemerData"
    redeemerHash <- v .:? "redeemerHash"
    let redeemer = case (redeemerData, redeemerHash) of
          (Nothing, Nothing) -> Nothing
          (Just d, Just h) -> pure $ MintAssetRedeemer d h
          _ -> fail "Redeemer is partially encoded"
    TxMintRow
      <$> v .: "slotNo"
      <*> v .: "blockHeaderHash"
      <*> v .: "blockNo"
      <*> v .: "txIndexInBlock"
      <*> v .: "txId"
      <*> v .: "policyId"
      <*> v .: "assetName"
      <*> v .: "quantity"
      <*> pure redeemer
  parseJSON _ = mempty

instance SQL.ToRow TxMintRow where
  toRow m =
    SQL.toRow
      [ SQL.toField $ m ^. txMintRowSlotNo
      , SQL.toField $ m ^. txMintRowBlockHeaderHash
      , SQL.toField $ m ^. txMintRowBlockNo
      , SQL.toField $ m ^. txMintRowTxIx
      , SQL.toField $ m ^. txMintRowTxId
      , SQL.toField $ m ^. txMintRowPolicyId
      , SQL.toField $ m ^. txMintRowAssetName
      , SQL.toField $ m ^. txMintRowQuantity
      , SQL.toField $ mintAssetRedeemerData <$> m ^. txMintRowRedeemer
      , SQL.toField $ mintAssetRedeemerHash <$> m ^. txMintRowRedeemer
      ]

mintAssetRedeemerPairs :: MintAssetRedeemer -> [Pair]
mintAssetRedeemerPairs row =
  [ "redeemerData" .= mintAssetRedeemerData row
  , "redeemerHash" .= mintAssetRedeemerHash row
  ]

instance ToJSON MintAssetRedeemer where
  toJSON = object . mintAssetRedeemerPairs

instance ToJSON TxMintRow where
  toJSON row =
    object $
      [ "slotNo" .= _txMintRowSlotNo row
      , "blockHeaderHash" .= _txMintRowBlockHeaderHash row
      , "blockNo" .= _txMintRowBlockNo row
      , "txIndexInBlock" .= _txMintRowTxIx row
      , "txId" .= _txMintRowTxId row
      , "policyId" .= _txMintRowPolicyId row
      , "assetName" .= _txMintRowAssetName row
      , "quantity" .= _txMintRowQuantity row
      ]
        <> maybe [] mintAssetRedeemerPairs (_txMintRowRedeemer row)

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
    \   , redeemerData    BLOB          \
    \   , redeemerHash    BLOB)"
  SQL.execute_
    c
    " CREATE INDEX IF NOT EXISTS               \
    \    minting_policy_events__txId_policyId  \
    \ ON minting_policy_events (txId, policyId)"
  createLastSyncTable c

sqliteInsert :: SQL.Connection -> [TxMintEvent] -> IO ()
sqliteInsert c es =
  let rows = toRows =<< toList es
      template =
        "INSERT INTO minting_policy_events          \
        \( slotNo, blockHeaderHash, blockNo,        \
        \txIndexInBlock, txId, policyId, assetName, \
        \ quantity, redeemerData,  redeemerHash )   \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   in case rows of
        [] -> pure ()
        xs -> SQL.executeMany c template xs

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
      (mintAssetRedeemer mintAsset)

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
        (row ^. txMintRowRedeemer)

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
      " SELECT slotNo, blockHeaderHash, blockNo, txIndexInBlock, txId,   \
      \        policyId, assetName, quantity, redeemerData, redeemerHash \
      \ FROM minting_policy_events                                       \
      \   "
        <> whereClause
        <> "                                        \
           \ ORDER BY blockNo ASC, txIndexInBlock ASC"

instance RI.Queryable MintBurnHandle where
  queryStorage memoryEvents (MintBurnHandle sqlCon _k) query = do
    sqliteCondition <- mkSqliteConditions
    storedEvents <- liftSQLError CantQueryIndexer $ queryStoredTxMintEvents sqlCon sqliteCondition
    pure $ MintBurnResult $ do
      TxMintEvent slotNo blockHeaderHash blockNo txAssets <- storedEvents <> filteredMemoryEvents
      TxMintInfo txId txIx mintAssets <- txAssets
      MintAsset policyId assetName quantity redeemer <- NE.toList mintAssets
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
          redeemer
    where
      -- filter the rows of a given predicate from in-memory events
      filteredInMemoryRows :: (TxMintRow -> Bool) -> [TxMintRow]
      filteredInMemoryRows f = filter f $ toRows =<< (coerce $ toList memoryEvents)
      -- filter in-memory events for a given predicate
      filteredMemoryEvents :: [TxMintEvent]
      filteredMemoryEvents = coerce $ fromRows $ filteredInMemoryRows rowFilter
      -- find txId's corresponding slotNo from in-memory events
      findInMemorySlotNoFromTxId :: C.TxId -> Maybe C.SlotNo
      findInMemorySlotNoFromTxId txid =
        fmap _txMintRowSlotNo
          . find (\r -> r ^. txMintRowTxId == txid)
          $ toRows =<< (coerce . toList $ memoryEvents)
      -- Applies every predicate to row, when all are True, then result is True.
      rowFilter :: TxMintRow -> Bool
      rowFilter row = all ($ row) mkRowPredicates

      -- Resolve constraints param

      upperSlot = case query of
        QueryAllMintBurn maybeSlotNo -> maybeSlotNo
        QueryAllBurn maybeSlotNo -> maybeSlotNo
        QueryByAssetId _policyId _assetName maybeSlotNo -> maybeSlotNo
        QueryBurnByAssetId _policyId _maybeAssetName maybeSlotNo _maybeTxId -> maybeSlotNo

      lowerTxId = case query of
        QueryBurnByAssetId _policyId _maybeAssetName _maybeSlotNo maybeTxId -> maybeTxId
        _other -> Nothing

      policyIdParam = case query of
        QueryByAssetId pId _assetName _maybeSlotNo -> Just pId
        QueryBurnByAssetId pId _maybeAssetName _maybeSlotNo _maybeTxId -> Just pId
        _other -> Nothing

      assetNameParam = case query of
        QueryByAssetId _policyId maybeAssetName _maybeSlotNo -> maybeAssetName
        QueryBurnByAssetId _policyId maybeAssetName _maybeSlotNo _maybeTxId -> maybeAssetName
        _other -> Nothing

      burnOnly = case query of
        QueryAllBurn _ -> True
        QueryBurnByAssetId{} -> True
        _other -> False

      -- Filter in-memory events

      upToSlot :: [TxMintRow -> Bool]
      upToSlot = case upperSlot of
        Just slotNo -> [\row -> row ^. txMintRowSlotNo <= slotNo]
        Nothing -> []

      matchesPolicyId :: TxMintRow -> Bool
      matchesPolicyId row = maybe True (row ^. txMintRowPolicyId ==) policyIdParam

      matchesAssetName :: TxMintRow -> Bool
      matchesAssetName row = maybe True (row ^. txMintRowAssetName ==) assetNameParam

      matchAfterTxId :: TxMintRow -> Bool
      matchAfterTxId row =
        let maybeSlot :: Maybe C.SlotNo
            maybeSlot = lowerTxId >>= findInMemorySlotNoFromTxId
         in maybe True (row ^. txMintRowSlotNo >=) maybeSlot

      isBurn :: TxMintRow -> Bool
      isBurn row = not burnOnly || row ^. txMintRowQuantity < 0

      mkRowPredicates :: [TxMintRow -> Bool]
      mkRowPredicates = [matchesPolicyId, isBurn, matchesAssetName, matchAfterTxId] <> upToSlot

      -- Filter sqlite events

      mkSqliteConditions :: ExceptT (IndexerError MintBurnQueryError) IO ([SQL.Query], [NamedParam])
      mkSqliteConditions = do
        cond <- mkAfterTxCondtion
        pure $
          mkUpperBoundCondition
            <> cond
            <> mkAssetIdCondition
            <> if burnOnly then (["quantity < 0"], []) else mempty

      -- Filter for transaction at or after
      mkAfterTxCondtion :: ExceptT (IndexerError MintBurnQueryError) IO ([SQL.Query], [NamedParam])
      mkAfterTxCondtion =
        let
          getSlotNoFromTxId :: C.TxId -> IO (Maybe C.SlotNo)
          getSlotNoFromTxId txid =
            case findInMemorySlotNoFromTxId txid of
              Nothing ->
                listToMaybe
                  <$> ( SQL.query sqlCon "SELECT slotNo FROM minting_policy_events WHERE txId = ?" (SQL.Only txid)
                          :: IO [C.SlotNo]
                      )
              s -> pure s
         in
          case lowerTxId of
            Nothing -> pure mempty
            Just txId -> do
              maybeSlotNo <- lift $ getSlotNoFromTxId txId
              case maybeSlotNo of
                Nothing ->
                  throwError $
                    QueryError $
                      TxNotIndexed txId
                Just sn ->
                  case upperSlot of
                    Nothing -> pure (["slotNo >= :loSlotNo"], [":loSlotNo" := sn])
                    Just hi
                      | sn < hi -> pure (["slotNo >= :loSlotNo"], [":loSlotNo" := sn])
                      | otherwise -> throwError $ QueryError $ InvalidInterval txId sn hi

      mkAssetIdCondition :: ([SQL.Query], [NamedParam])
      mkAssetIdCondition =
        unzip $
          maybe [] (\pId -> [("policyId = :policyId", ":policyId" := pId)]) policyIdParam
            <> maybe [] (\name -> [("assetName = :assetName", ":assetName" := name)]) assetNameParam

      -- Filter for slotNo interval
      mkUpperBoundCondition :: ([SQL.Query], [NamedParam])
      mkUpperBoundCondition = case upperSlot of
        Nothing -> mempty
        Just s -> (["slotNo <= :hiSlotNo"], [":hiSlotNo" := s])

instance RI.HasPoint (RI.StorableEvent MintBurnHandle) C.ChainPoint where
  getPoint (MintBurnEvent e) = C.ChainPoint (txMintEventSlotNo e) (txMintEventBlockHeaderHash e)

instance RI.Buffered MintBurnHandle where
  persistToStorage events h@(MintBurnHandle sqlCon _k) =
    liftSQLError CantInsertEvent $
      do
        SQL.withTransaction sqlCon $ do
          sqliteInsert sqlCon (map coerce $ toList events)
          let chainPoints =
                (C.ChainPoint <$> txMintEventSlotNo <*> txMintEventBlockHeaderHash)
                  . getEvent
                  <$> toList events
          insertLastSyncPoints sqlCon chainPoints
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
    liftSQLError CantQueryIndexer $ queryLastSyncPoint c

instance RI.Rewindable MintBurnHandle where
  rewindStorage cp h@(MintBurnHandle sqlCon _k) =
    liftSQLError CantRollback $ doRewind >> rollbackLastSyncPoints sqlCon cp >> pure h
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
