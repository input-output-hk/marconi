{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

{- | Mint/burn event indexer, the result of which is an sqlite database
   'mintburn.db' which has a table 'minting_policy_events' with the
   following fields:

     - slotNo          INT NOT NULL
     - blockHeaderHash INT NOT NULL
     - txId            BLOB NOT NULL
     - policyId        BLOB NOT NULL
     - assetName       TEXT NOT NULL
     - quantity        INT NOT NULL
     - redeemerIx      INT NOT NULL
     - redeemerData    BLOB NOT NULL
-}

module Marconi.ChainIndex.Indexers.MintBurn where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as LA
import Cardano.Ledger.Alonzo.Scripts.Data qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWits qualified as LA
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
import GHC.Generics (Generic)
import Marconi.ChainIndex.Error (IndexerError (CantInsertEvent, CantQueryIndexer, CantRollback, CantStartIndexer),
                                 liftSQLError)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.ChainIndex.Utils (chainPointOrGenesis)
import Marconi.Core.Storable (StorableMonad)
import Marconi.Core.Storable qualified as RI
import Ouroboros.Consensus.Shelley.Eras qualified as OEra

-- * Event

data TxMintEvent = TxMintEvent
  { txMintEventSlotNo          :: !C.SlotNo
  , txMintEventBlockHeaderHash :: !(C.Hash C.BlockHeader)
  , txMintEventTxAssets        :: !(NE.NonEmpty (C.TxId, NE.NonEmpty MintAsset))
  } deriving (Show, Eq, Ord)

data MintAsset = MintAsset
  { mintAssetPolicyId     :: !C.PolicyId
  , mintAssetAssetName    :: !C.AssetName
  , mintAssetQuantity     :: !C.Quantity
  , mintAssetRedeemerIdx  :: !Word64
  , mintAssetRedeemerData :: !C.ScriptData
  } deriving (Show, Eq, Ord)

toUpdate :: C.BlockInMode C.CardanoMode -> Maybe TxMintEvent
toUpdate (C.BlockInMode (C.Block (C.BlockHeader slotNo blockHeaderHash _blockNo) txs) _) =
  case mapMaybe txMints txs of
    x : xs -> Just $ TxMintEvent slotNo blockHeaderHash (x NE.:| xs)
    []     -> Nothing

txMints :: C.Tx era -> Maybe (C.TxId, NE.NonEmpty MintAsset)
txMints (C.Tx txb _) = case txbMints txb of
  x : xs -> Just (C.getTxId txb, x NE.:| xs )
  []     -> Nothing

txbMints :: C.TxBody era -> [MintAsset]
txbMints txb = case txb of
  C.ShelleyTxBody era shelleyTx _ _ _ _ -> case era of
    C.ShelleyBasedEraShelley -> []
    C.ShelleyBasedEraAllegra -> []
    C.ShelleyBasedEraMary -> []
    C.ShelleyBasedEraAlonzo -> do
      (policyId, assetName, quantity, index', redeemer) <- getPolicyData txb $ LA.atbMint shelleyTx
      pure $ MintAsset policyId assetName quantity index' redeemer
    C.ShelleyBasedEraBabbage -> do
      (policyId, assetName, quantity, index', redeemer) <- getPolicyData txb $ LB.btbMint shelleyTx
      pure $ MintAsset policyId assetName quantity index' redeemer
    C.ShelleyBasedEraConway -> do
      (policyId, assetName, quantity, index', redeemer) <- getPolicyData txb $ LC.ctbMint shelleyTx
      pure $ MintAsset policyId assetName quantity index' redeemer
  _byronTxBody -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

-- * Helpers

getPolicyData
    :: forall era. Ledger.Era (C.ShelleyLedgerEra era)
    => C.TxBody era
    -> LM.MultiAsset OEra.StandardCrypto
    -> [(C.PolicyId, C.AssetName, C.Quantity, Word64, C.ScriptData)]
getPolicyData txb (LM.MultiAsset m) = do
  let
    policyIdList = Map.toList m
    getPolicyId index' = policyIdList !! fromIntegral index'
  ((maryPolicyID, assets), index'', (redeemer, _)) <- map (\(index', data_) -> (getPolicyId index', index', data_)) mintRedeemers
  (assetName, quantity) :: (LM.AssetName, Integer) <- Map.toList assets
  pure (fromMaryPolicyID maryPolicyID, fromMaryAssetName assetName, C.Quantity quantity, index'', fromAlonzoData redeemer)
  where
    mintRedeemers = txRedeemers txb
      & Map.toList
      & filter (\(LA.RdmrPtr tag _, _) -> tag == LA.Mint)
      & map (\(LA.RdmrPtr _ w, a) -> (w, a))

    txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
      C.TxBodyScriptData _proof _datum (LA.Redeemers redeemers) -> redeemers
      C.TxBodyNoScriptData                                      -> mempty
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
    { _txMintRowSlotNo          :: !C.SlotNo
    , _txMintRowBlockHeaderHash :: !(C.Hash C.BlockHeader)
    , _txMintRowTxId            :: !C.TxId
    , _txMintRowPolicyId        :: !C.PolicyId
    , _txMintRowAssetName       :: !C.AssetName
    , _txMintRowQuantity        :: !C.Quantity
    , _txMintRowRedeemerIdx     :: !Word64
    , _txMintRowRedeemerData    :: !C.ScriptData
    }
    deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

makeLenses 'TxMintRow

instance FromJSON TxMintRow where
    parseJSON (Object v) =
        TxMintRow
            <$> v .: "slotNo"
            <*> v .: "blockHeaderHash"
            <*> v .: "txId"
            <*> v .: "policyId"
            <*> v .: "assetName"
            <*> v .: "quantity"
            <*> v .: "redeemerIdx"
            <*> v .: "redeemerData"
    parseJSON _ = mempty

instance ToJSON TxMintRow where
  toJSON (TxMintRow slotNo bhh txId policyId assetName qty redIdx redData) = object
    [ "slotNo" .= slotNo
    , "blockHeaderHash" .= bhh
    , "txId" .= txId
    , "policyId" .= policyId
    , "assetName" .= assetName
    , "quantity" .= qty
    , "redeemerIdx" .= redIdx
    , "redeemerData" .= redData
    ]

sqliteInit :: SQL.Connection -> IO ()
sqliteInit c = liftIO $ do
  SQL.execute_ c
    " CREATE TABLE IF NOT EXISTS        \
    \   minting_policy_events           \
    \   ( slotNo          INT NOT NULL  \
    \   , blockHeaderHash INT NOT NULL  \
    \   , txId            BLOB NOT NULL \
    \   , policyId        BLOB NOT NULL \
    \   , assetName       TEXT NOT NULL \
    \   , quantity        INT NOT NULL  \
    \   , redeemerIx      INT NOT NULL  \
    \   , redeemerData    BLOB NOT NULL)"
  SQL.execute_ c
    " CREATE INDEX IF NOT EXISTS               \
    \    minting_policy_events__txId_policyId  \
    \ ON minting_policy_events (txId, policyId)"

sqliteInsert :: SQL.Connection -> [TxMintEvent] -> IO ()
sqliteInsert c es = SQL.executeMany c template $ toRows =<< toList es
  where
    template =
      "INSERT INTO minting_policy_events \
      \ ( slotNo, blockHeaderHash, txId  \
      \ , policyId, assetName, quantity  \
      \ , redeemerIx, redeemerData )     \
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)  "

toRows :: TxMintEvent -> [TxMintRow]
toRows e = do
  (txId, txMintAssets) <- NE.toList $ txMintEventTxAssets e
  mintAsset <- NE.toList txMintAssets
  pure $ TxMintRow
    (txMintEventSlotNo e)
    (txMintEventBlockHeaderHash e)
    txId
    (mintAssetPolicyId mintAsset)
    (mintAssetAssetName mintAsset)
    (mintAssetQuantity mintAsset)
    (mintAssetRedeemerIdx mintAsset)
    (mintAssetRedeemerData mintAsset)

-- | Input rows must be sorted by C.SlotNo.
fromRows :: [TxMintRow] -> [TxMintEvent]
fromRows rows =  do
  rs@(r :| _) <- NE.groupBy ((==) `on` slotNo) rows -- group by SlotNo
  pure $ TxMintEvent (slotNo r) (hash r) $ do
    rs' <- NE.groupBy1 ((==) `on` txId) rs -- group by TxId
    pure (txId r, rowToMintAsset <$> rs')
  where
    slotNo = view txMintRowSlotNo :: TxMintRow -> C.SlotNo
    hash = view txMintRowBlockHeaderHash :: TxMintRow -> C.Hash C.BlockHeader
    txId = view txMintRowTxId :: TxMintRow -> C.TxId
    rowToMintAsset :: TxMintRow -> MintAsset
    rowToMintAsset row =
        MintAsset
            (row ^. txMintRowPolicyId)
            (row ^. txMintRowAssetName)
            (row ^. txMintRowQuantity)
            (row ^. txMintRowRedeemerIdx)
            (row ^. txMintRowRedeemerData)

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
          " SELECT slotNo, blockHeaderHash, txId, policyId       \
          \      , assetName, quantity, redeemerIx, redeemerData \
          \   FROM minting_policy_events                         \
          \   " <> whereClause <> "                              \
          \  ORDER BY slotNo, txId                               "

groupBySlotAndHash :: [TxMintEvent] -> [TxMintEvent]
groupBySlotAndHash events = events
  & sort
  & groupBy (\e1 e2 -> txMintEventSlotNo e1 == txMintEventSlotNo e2 && txMintEventBlockHeaderHash e1 == txMintEventBlockHeaderHash e2)
  & concatMap (\case e : es -> [ TxMintEvent (txMintEventSlotNo e) (txMintEventBlockHeaderHash e) $ txMintEventTxAssets =<< (e :| es) ]
                     []     -> [])

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
  = QueryByAssetId C.PolicyId C.AssetName (Maybe C.SlotNo)
  -- ^ Query all transactions that minted a specific 'AssetId' until an upper bound slot in the
  -- blockchain. If the upper bound slot is 'Nothing', then we return everything.
  | QueryAllMintBurn (Maybe C.SlotNo)
  -- ^ Query all transactions that minted 'AssetId's until an upper bound slot in the blockchain. If
  -- the upper bound slot is 'Nothing', then we return everything.
  | QueryBurnByAssetId C.PolicyId C.AssetName (Maybe C.SlotNo)
  -- ^ Query all transactions that burned a specific 'AssetId' until an upper bound slot in the
  -- blockchain. If the upper bound slot is 'Nothing', then we return everything.
  | QueryAllBurn (Maybe C.SlotNo)
  -- ^ Query all transactions that burned 'AssetId's until an upper bound slot in the blockchain. If
  -- the upper bound slot is 'Nothing', then we return everything.
  deriving (Show)

newtype instance RI.StorableResult MintBurnHandle
  = MintBurnResult [TxMintRow]
  deriving (Show)

instance RI.Queryable MintBurnHandle where
  queryStorage memoryEvents (MintBurnHandle sqlCon _k) query = liftSQLError CantQueryIndexer $ do
    storedEvents <- queryStoredTxMintEvents sqlCon $ mkSqliteConditions query
    pure $ MintBurnResult $ do
      TxMintEvent slotNo blockHeaderHash txAssets <- filteredMemoryEvents <> storedEvents
      (txId, mintAssets) <- NE.toList txAssets
      MintAsset policyId assetName quantity redeemerIx redeemerData <- NE.toList mintAssets
      pure $ TxMintRow
        slotNo
        blockHeaderHash
        txId
        policyId
        assetName
        quantity
        redeemerIx
        redeemerData

    where
      filteredMemoryEvents :: [TxMintEvent]
      filteredMemoryEvents = coerce $ fromRows $ filter rowFilter $ toRows =<< (coerce $ toList memoryEvents)

      -- Applies every predicate to row, when all are True, then result is True.
      rowFilter :: TxMintRow -> Bool
      rowFilter row = let filters = mkRowPredicates query in all ($ row) filters

      -- * Filter in-memory events

      upToSlot :: Maybe C.SlotNo -> [TxMintRow -> Bool]
      upToSlot = \case
        Just slotNo -> [\row -> _txMintRowSlotNo row <= slotNo]
        Nothing     -> []

      matchesAssetId :: C.PolicyId -> C.AssetName -> TxMintRow -> Bool
      matchesAssetId policyId assetName row =
        _txMintRowPolicyId row == policyId && _txMintRowAssetName row == assetName

      isBurn :: TxMintRow -> Bool
      isBurn row = _txMintRowQuantity row < 0

      mkRowPredicates :: RI.StorableQuery MintBurnHandle -> [TxMintRow -> Bool]
      mkRowPredicates = \case
        QueryAllMintBurn maybeSlotNo -> upToSlot maybeSlotNo
        QueryAllBurn     maybeSlotNo -> upToSlot maybeSlotNo <> [isBurn]
        QueryByAssetId policyId assetName maybeSlotNo     -> upToSlot maybeSlotNo <> [matchesAssetId policyId assetName]
        QueryBurnByAssetId policyId assetName maybeSlotNo -> upToSlot maybeSlotNo <> [matchesAssetId policyId assetName, isBurn]

      -- * Filter sqlite events

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
        Just s  -> (["slotNo <= :slotNo"] , [":slotNo" := s])

instance RI.HasPoint (RI.StorableEvent MintBurnHandle) C.ChainPoint where
  getPoint (MintBurnEvent e) = C.ChainPoint (txMintEventSlotNo e) (txMintEventBlockHeaderHash e)

instance RI.Buffered MintBurnHandle where
  persistToStorage events h@(MintBurnHandle sqlCon _k)
    = liftSQLError CantInsertEvent
    $ do
    sqliteInsert sqlCon (map coerce $ toList events)
    pure h

  getStoredEvents (MintBurnHandle sqlCon k)
    = liftSQLError CantQueryIndexer
    $ do
    fmap MintBurnEvent . fromRows <$> SQL.query sqlCon query (SQL.Only k)
    where
      query =
        " SELECT slotNo, blockHeaderHash, txId, policyId, assetName, quantity, \
        \        redeemerIx, redeemerData                                      \
        \   FROM minting_policy_events                                         \
        \  WHERE slotNo >= (SELECT MAX(slotNo) - ? FROM minting_policy_events) \
        \  ORDER BY slotNo DESC, txId                                          "

instance RI.Resumable MintBurnHandle where
  resumeFromStorage (MintBurnHandle c _) = liftSQLError CantQueryIndexer $ fmap chainPointOrGenesis $
    SQL.query_ c "SELECT slotNo, blockHeaderHash FROM minting_policy_events ORDER BY slotNo DESC LIMIT 1"

instance RI.Rewindable MintBurnHandle where
  rewindStorage cp h@(MintBurnHandle sqlCon _k)
    = liftSQLError CantRollback $ doRewind >> pure h
    where
      doRewind = case cp of
        C.ChainPoint slotNo _ ->
          SQL.execute  sqlCon "DELETE FROM minting_policy_events WHERE slotNo > ?" (SQL.Only slotNo)
        C.ChainPointAtGenesis ->
          SQL.execute_ sqlCon "DELETE FROM minting_policy_events"

open :: FilePath -> SecurityParam -> StorableMonad MintBurnHandle MintBurnIndexer
open dbPath bufferSize = do
  c <- liftSQLError CantStartIndexer $ SQL.open dbPath
  lift $ SQL.execute_ c "PRAGMA journal_mode=WAL"
  lift $ sqliteInit c
  RI.emptyState (fromEnum bufferSize) (MintBurnHandle c bufferSize)
