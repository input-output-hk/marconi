{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- |

Mint/burn event indexer, the result of which is an sqlite database 'minttokenevent.db' which has a
table 'minting_policy_events' with the following fields:

@
( slotNo INT NOT NULL
, blockHeaderHash BLOB NOT NULL
, blockNo INT NOT NULL
, txId BLOB NOT NULL
, txIndexInBlock INT NOT NULL
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
  MintTokenBlockEvents (MintTokenBlockEvents),
  mintTokenEvents,
  MintTokenEvent (MintTokenEvent),
  mintTokenEventLocation,
  mintTokenEventAsset,
  MintTokenEventLocation (MintTokenEventLocation),
  mintTokenEventBlockNo,
  mintTokenEventIndexInBlock,
  mintTokenEventTxId,
  MintAsset (MintAsset),
  mintAssetAssetId,
  mintAssetPolicyId,
  mintAssetAssetName,
  mintAssetQuantity,
  mintAssetRedeemer,
  MintAssetRedeemer (MintAssetRedeemer),
  mintAssetRedeemerData,
  mintAssetRedeemerHash,

  -- * Indexer and worker
  MintTokenEventIndexer,
  StandardMintTokenEventIndexer,
  MintTokenEventConfig (..),
  configTrackedAssetIds,
  mkMintTokenIndexer,
  mkMintTokenEventWorker,
  filterByTargetAssetIds,

  -- * Extract events
  extractEventsFromTx,

  -- * Queries
  MintTokenEventsMatchingQuery (MintTokenEventsMatchingQuery),
  allEvents,
  allMintEvents,
  allBurnEvents,
  QueryByAssetId (..),
  EventType (..),
  toTimedMintEvents,
)
where

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
import Control.Lens (Lens', folded, lens, over, toListOf, view, (%~), (.~), (^.), (^?))
import Control.Lens qualified as Lens
import Control.Monad (unless)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Data.ByteString.Short qualified as Short
import Data.Function (on, (&))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig,
  mkStandardWorkerWithFilter,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (
  TxIndexInBlock,
 )
import Marconi.Core qualified as Core

-- | A raw SQLite indexer for 'MintTokenBlockEvents'
type MintTokenEventIndexer = Core.SQLiteIndexer MintTokenBlockEvents

-- | A SQLite 'MintTokenBlockEvents' indexer with Catchup
type StandardMintTokenEventIndexer m = StandardSQLiteIndexer m MintTokenBlockEvents

newtype MintTokenEventConfig = MintTokenEventConfig {_configTrackedAssetIds :: [C.AssetId]}

-- | Minting events given for each block.
newtype MintTokenBlockEvents = MintTokenBlockEvents
  { _mintTokenEvents :: NonEmpty MintTokenEvent
  }
  deriving (Show, Eq, Ord)

{- | Single minting event. This is the datatype was will be used to store the events in the database
(not 'MintTokenBlockEvents'). More specifically, we will store 'Core.Timed (Core.Point
MintTokenEvent) MintTokenEvent)'.
-}
data MintTokenEvent = MintTokenEvent
  { _mintTokenEventLocation :: !MintTokenEventLocation
  , _mintTokenEventAsset :: !MintAsset
  }
  deriving (Show, Eq, Ord, Generic)

data MintTokenEventLocation = MintTokenEventLocation
  { _mintTokenEventBlockNo :: !C.BlockNo
  , _mintTokenEventTxId :: !C.TxId
  , _mintTokenEventIndexInBlock :: !TxIndexInBlock
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

data QueryByAssetId event = QueryByAssetId
  { _queryByAssetIdPolicyId :: !C.PolicyId
  , _queryByAssetIdName :: !(Maybe C.AssetName)
  , _queryByAssetIdEventType :: !(Maybe EventType)
  , _queryByAssetIdUpperSlotNo :: !(Maybe C.SlotNo)
  , _queryByAssetIdLowerTxId :: !(Maybe C.TxId)
  }
  deriving (Show)

data EventType = MintEventType | BurnEventType
  deriving (Show)

Lens.makeLenses ''MintTokenBlockEvents
Lens.makeLenses ''MintTokenEvent
Lens.makeLenses ''MintTokenEventLocation
Lens.makeLenses ''MintAsset
Lens.makeLenses ''MintAssetRedeemer
Lens.makeLenses ''MintTokenEventConfig
Lens.makeLenses ''QueryByAssetId

mintAssetAssetId :: Lens' MintAsset C.AssetId
mintAssetAssetId = lens get set
  where
    get mintAsset = C.AssetId (mintAsset ^. mintAssetPolicyId) (mintAsset ^. mintAssetAssetName)
    set mintAsset (C.AssetId policyId assetName) =
      mintAsset
        & mintAssetPolicyId .~ policyId
        & mintAssetAssetName .~ assetName
    set mintAsset C.AdaAssetId =
      mintAsset
        & mintAssetPolicyId .~ ""
        & mintAssetAssetName .~ ""

instance SQL.ToRow (Core.Timed C.ChainPoint MintTokenEvent) where
  toRow te = SQL.toRow (te ^. Core.point) ++ SQL.toRow (te ^. Core.event)

instance SQL.FromRow (Core.Timed C.ChainPoint MintTokenEvent) where
  fromRow = Core.Timed <$> SQL.fromRow <*> SQL.fromRow

instance SQL.ToRow MintTokenEvent where
  toRow e =
    SQL.toRow (e ^. mintTokenEventLocation)
      ++ SQL.toRow (e ^. mintTokenEventAsset)

instance SQL.FromRow MintTokenEvent where
  fromRow = do
    MintTokenEvent
      <$> SQL.fromRow
      <*> SQL.fromRow

instance SQL.ToRow MintTokenEventLocation where
  toRow l =
    SQL.toRow
      [ SQL.toField $ l ^. mintTokenEventBlockNo
      , SQL.toField $ l ^. mintTokenEventTxId
      , SQL.toField $ l ^. mintTokenEventIndexInBlock
      ]

instance SQL.FromRow MintTokenEventLocation where
  fromRow =
    MintTokenEventLocation
      <$> SQL.field
      <*> SQL.field
      <*> SQL.field

instance SQL.ToRow MintAsset where
  toRow ma =
    SQL.toRow
      [ SQL.toField $ ma ^. mintAssetPolicyId
      , SQL.toField $ ma ^. mintAssetAssetName
      , SQL.toField $ ma ^. mintAssetQuantity
      , SQL.toField $ ma ^? mintAssetRedeemer . traverse . mintAssetRedeemerHash
      , SQL.toField $ ma ^? mintAssetRedeemer . traverse . mintAssetRedeemerData
      ]

instance SQL.FromRow MintAsset where
  fromRow = do
    policyId <- SQL.field
    assetName <- SQL.field
    quantity <- SQL.field
    redeemerHashM <- SQL.field
    redeemerDataM <- SQL.field
    let redeemer =
          do
            redeemerHash <- redeemerHashM
            redeemerData <- redeemerDataM
            pure $ MintAssetRedeemer redeemerData redeemerHash
    pure $ MintAsset policyId assetName quantity redeemer

type instance Core.Point MintTokenBlockEvents = C.ChainPoint

-- | Create a worker for the MintTokenEvent indexer
mkMintTokenEventWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => StandardWorkerConfig m input MintTokenBlockEvents
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> MintTokenEventConfig
  -- ^ Specific configuration of the indexer (mostly for logging purpose and filtering for target
  -- asset ids)
  -> FilePath
  -> n (StandardWorker m input MintTokenBlockEvents Core.SQLiteIndexer)
mkMintTokenEventWorker workerConfig (MintTokenEventConfig trackedAssetIds) dbPath = do
  sqliteIndexer <- mkMintTokenIndexer dbPath
  mkStandardWorkerWithFilter workerConfig (filterByTargetAssetIds trackedAssetIds) sqliteIndexer

-- | Only keep the MintTokenEvents at a block if they mint a target 'AssetId'.
filterByTargetAssetIds :: [C.AssetId] -> MintTokenBlockEvents -> Maybe MintTokenBlockEvents
filterByTargetAssetIds assetIds =
  fmap MintTokenBlockEvents
    . NonEmpty.nonEmpty
    . NonEmpty.filter ((`elem` assetIds) . view (mintTokenEventAsset . mintAssetAssetId))
    . view mintTokenEvents

-- Events extraction

extractEventsFromTx :: C.BlockNo -> TxIndexInBlock -> C.TxBody era -> [MintTokenEvent]
extractEventsFromTx blockNo txIndexInBlock txb =
  let mintEventLocation = MintTokenEventLocation blockNo (C.getTxId txb) txIndexInBlock

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
              , blockHeaderHash BLOB NOT NULL
              , blockNo INT NOT NULL
              , txId BLOB NOT NULL
              , txIndexInBlock INT NOT NULL
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

  let mintEventInsertQuery :: SQL.Query
      mintEventInsertQuery =
        [sql|INSERT
               INTO minting_policy_events (
                 slotNo,
                 blockHeaderHash,
                 blockNo,
                 txId,
                 txIndexInBlock,
                 policyId,
                 assetName,
                 quantity,
                 redeemerHash,
                 redeemerData
              ) VALUES
              (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|]
      createMintPolicyEventTables =
        [createMintPolicyEvent, createMintPolicyIdIndex]
      mintInsertPlans = [Core.SQLInsertPlan fromTimedMintEvents mintEventInsertQuery]
  Sync.mkSyncedSqliteIndexer
    dbPath
    createMintPolicyEventTables
    [mintInsertPlans]
    [Core.SQLRollbackPlan "minting_policy_events" "slotNo" C.chainPointToSlotNo]

fromTimedMintEvents
  :: Core.Timed point MintTokenBlockEvents
  -> [Core.Timed point MintTokenEvent]
fromTimedMintEvents =
  traverse $ toListOf $ mintTokenEvents . folded

toTimedMintEvents
  :: [Core.Timed C.ChainPoint MintTokenEvent]
  -> [Core.Timed C.ChainPoint MintTokenBlockEvents]
toTimedMintEvents =
  let groupSamePointEvents
        :: NonEmpty (Core.Timed point a)
        -> Core.Timed point (NonEmpty a)
      groupSamePointEvents xs@(x :| _) = Core.Timed (x ^. Core.point) (Lens.view Core.event <$> xs)
   in mapMaybe (traverse Just . fmap MintTokenBlockEvents . groupSamePointEvents)
        . NonEmpty.groupBy ((==) `on` Lens.view Core.point)

allEvents :: MintTokenEventsMatchingQuery MintTokenBlockEvents
allEvents = MintTokenEventsMatchingQuery Just

allMintEvents :: MintTokenEventsMatchingQuery MintTokenBlockEvents
allMintEvents = MintTokenEventsMatchingQuery $ \(MintTokenBlockEvents events) ->
  fmap MintTokenBlockEvents $
    NonEmpty.nonEmpty $
      NonEmpty.filter (\e -> e ^. mintTokenEventAsset . mintAssetQuantity > 0) events

allBurnEvents :: MintTokenEventsMatchingQuery MintTokenBlockEvents
allBurnEvents = MintTokenEventsMatchingQuery $ \(MintTokenBlockEvents events) ->
  fmap MintTokenBlockEvents $
    NonEmpty.nonEmpty $
      NonEmpty.filter (\e -> e ^. mintTokenEventAsset . mintAssetQuantity < 0) events

newtype MintTokenEventsMatchingQuery event = MintTokenEventsMatchingQuery (event -> Maybe event)

type instance
  Core.Result (MintTokenEventsMatchingQuery event) =
    [Core.Timed (Core.Point event) event]

instance
  (MonadIO m, MonadError (Core.QueryError (MintTokenEventsMatchingQuery MintTokenBlockEvents)) m)
  => Core.Queryable
      m
      MintTokenBlockEvents
      (MintTokenEventsMatchingQuery MintTokenBlockEvents)
      Core.SQLiteIndexer
  where
  query =
    let parseResult
          :: (MintTokenBlockEvents -> Maybe MintTokenBlockEvents)
          -> [Core.Timed C.ChainPoint MintTokenEvent]
          -> [Core.Timed C.ChainPoint MintTokenBlockEvents]
        parseResult eventsFilterFunc es =
          mapMaybe (sequence . over Core.event eventsFilterFunc) $ toTimedMintEvents es
     in Core.querySyncedOnlySQLiteIndexerWith
          (\cp -> pure [":slotNo" := C.chainPointToSlotNo cp])
          (const $ mkMintTokenEventQueryBy Nothing)
          (\(MintTokenEventsMatchingQuery p) r -> parseResult p r)

instance
  (MonadError (Core.QueryError (MintTokenEventsMatchingQuery MintTokenBlockEvents)) m)
  => Core.Queryable
      m
      MintTokenBlockEvents
      (MintTokenEventsMatchingQuery MintTokenBlockEvents)
      Core.ListIndexer
  where
  query p (MintTokenEventsMatchingQuery predicate) ix = do
    let convertError
          :: Core.QueryError (Core.EventsMatchingQuery MintTokenBlockEvents)
          -> Core.QueryError (MintTokenEventsMatchingQuery MintTokenBlockEvents)
        convertError = \case
          Core.NotStoredAnymore -> Core.NotStoredAnymore
          (Core.IndexerQueryError t) -> Core.IndexerQueryError t
          (Core.AheadOfLastSync r) -> Core.AheadOfLastSync r

    timedEventsE <- runExceptT $ Core.query p (Core.EventsMatchingQuery predicate) ix
    timedEvents <- either (throwError . convertError) pure timedEventsE
    pure $ sortEventsByOrderOfBlockchainAppearance timedEvents

type instance Core.Result (QueryByAssetId event) = [Core.Timed (Core.Point event) event]

{- | Utility for QueryByAssetId queries. Check whether the upper SlotNo bound is
 less than or equal to the provided @C.'ChainPoint'@. If so, return 'Just' that.
 Otherwise, return Nothing. If no upper SlotNo is provided, return the ChainPoint.
-}
upperSlotNoIfValid :: C.ChainPoint -> Maybe C.SlotNo -> Maybe C.SlotNo
upperSlotNoIfValid C.ChainPointAtGenesis _ = Nothing
upperSlotNoIfValid (C.ChainPoint sn _) Nothing = Just sn
upperSlotNoIfValid (C.ChainPoint sn _) (Just usn) = if usn <= sn then Nothing else Just usn

{- | Utility for QueryByAssetId queries. If upperSlotNo is provided,
 check that the point's SlotNo is late enough to have all information.
-}
pointSufficient :: C.ChainPoint -> Maybe C.SlotNo -> Bool
pointSufficient cp = isJust . upperSlotNoIfValid cp

instance
  forall m
   . ( MonadIO m
     , MonadError (Core.QueryError (QueryByAssetId MintTokenBlockEvents)) m
     )
  => Core.Queryable m MintTokenBlockEvents (QueryByAssetId MintTokenBlockEvents) Core.ListIndexer
  where
  query point (QueryByAssetId policyId assetNameM eventType upperSlotNo lowerTxId) ix = do
    -- Filter events based on 'QueryByAssetId' query
    let queryByAssetIdPredicate = Core.EventsMatchingQuery $ \(MintTokenBlockEvents events) ->
          let isEventType :: MintTokenEvent -> Bool
              isEventType e = case eventType of
                Nothing -> True
                Just MintEventType -> e ^. mintTokenEventAsset . mintAssetQuantity > 0
                Just BurnEventType -> e ^. mintTokenEventAsset . mintAssetQuantity < 0
              isAssetId :: MintTokenEvent -> Bool
              isAssetId e =
                case assetNameM of
                  Nothing ->
                    e ^. mintTokenEventAsset . mintAssetPolicyId == policyId
                  Just assetName ->
                    e ^. mintTokenEventAsset . mintAssetPolicyId == policyId
                      && e ^. mintTokenEventAsset . mintAssetAssetName == assetName
           in fmap MintTokenBlockEvents $
                NonEmpty.nonEmpty $
                  NonEmpty.filter (\e -> isAssetId e && isEventType e) events

    unless (pointSufficient point upperSlotNo) $
      throwError $
        Core.PointTooEarly ("Point " <> Text.pack (show point) <> " precedes query upper SlotNo")

    timedEventsE <- runExceptT $ Core.query point queryByAssetIdPredicate ix
    timedEvents <-
      either (throwError . convertEventsMatchingErrorToQueryByAssetIdError) pure timedEventsE
    let sortedTimedEvents = sortEventsByOrderOfBlockchainAppearance timedEvents
    pure sortedTimedEvents

{- | Sort the events based on their order of appearance in the blockchain. This means that events
first sorted by block number (or slot number). Then, for events in the same block number (or slot
number), we sort by their position in a block. As each event is associated with a transaction, we
use the index of a transaction in a block.
-}
sortEventsByOrderOfBlockchainAppearance
  :: [Core.Timed C.ChainPoint MintTokenBlockEvents]
  -> [Core.Timed C.ChainPoint MintTokenBlockEvents]
sortEventsByOrderOfBlockchainAppearance =
  fmap
    ( \te ->
        te
          & Core.event . mintTokenEvents
            %~ NonEmpty.sortBy
              ( comparing
                  ( \e ->
                      ( e ^. mintTokenEventLocation . mintTokenEventBlockNo
                      , e ^. mintTokenEventLocation . mintTokenEventIndexInBlock
                      )
                  )
              )
    )
    . List.sort

convertEventsMatchingErrorToQueryByAssetIdError
  :: Core.QueryError (Core.EventsMatchingQuery MintTokenBlockEvents)
  -> Core.QueryError (QueryByAssetId MintTokenBlockEvents)
convertEventsMatchingErrorToQueryByAssetIdError = \case
  Core.NotStoredAnymore -> Core.NotStoredAnymore
  (Core.IndexerQueryError t) -> Core.IndexerQueryError t
  (Core.AheadOfLastSync r) -> Core.AheadOfLastSync r
  (Core.PointTooEarly r) -> Core.PointTooEarly r

instance
  (MonadIO m, MonadError (Core.QueryError (QueryByAssetId MintTokenBlockEvents)) m)
  => Core.Queryable m MintTokenBlockEvents (QueryByAssetId MintTokenBlockEvents) Core.SQLiteIndexer
  where
  query point config ix = do
    let
      -- Check whether the upperSlotNo is valid. If not, e.g. point is genesis, throw an error
      -- before the query.
      validUpperSlotNo = upperSlotNoIfValid point (config ^. queryByAssetIdUpperSlotNo)

      -- NOTE: Though this is a function of any cp and config, querySyncedOnlySQLiteIndexerWith only
      -- calls mkNamedParams on 'point' and 'config' provided as arguments here. So upperSlotNo is
      -- valid below if and only if it is valid when checked in 'validUpperSlotNo', and that value
      -- is captured.
      mkNamedParams cp =
        \case
          QueryByAssetId policyId (Just assetName) _ _ lowerTxId ->
            [ ":slotNo" := validUpperSlotNo
            , ":policyId" := policyId
            , ":assetName" := assetName
            ]
          QueryByAssetId policyId Nothing _ _ lowerTxId ->
            [ ":slotNo" := validUpperSlotNo
            , ":policyId" := policyId
            ]
      mkQuery (QueryByAssetId _ assetNameM eventType upperSlotNo lowerTxId) =
        let policyIdWhereClause = Just "policyId = :policyId"
            assetNameWhereClause = fmap (const "assetName = :assetName") assetNameM
            eventTypeWhereClause =
              case eventType of
                Nothing -> Nothing
                Just MintEventType -> Just "quantity > 0"
                Just BurnEventType -> Just "quantity < 0"
            whereClause =
              fmap (\e -> Text.intercalate " AND " $ NonEmpty.toList e) $
                NonEmpty.nonEmpty $
                  catMaybes [policyIdWhereClause, assetNameWhereClause, eventTypeWhereClause]
         in mkMintTokenEventQueryBy whereClause

    unless (isJust validUpperSlotNo) $
      throwError $
        Core.PointTooEarly ("Point " <> Text.pack (show point) <> " precedes query upper SlotNo")

    Core.querySyncedOnlySQLiteIndexerWith
      mkNamedParams
      mkQuery
      (const toTimedMintEvents)
      point
      config
      ix

mkMintTokenEventQueryBy :: Maybe Text -> SQL.Query
mkMintTokenEventQueryBy whereClauseM =
  SQL.Query $
    Text.intercalate " " $
      catMaybes
        [ Just
            "SELECT slotNo, blockHeaderHash, blockNo, txId, txIndexInBlock, policyId, assetName, quantity, redeemerHash, redeemerData"
        , Just "FROM minting_policy_events"
        , Just "WHERE slotNo <= :slotNo"
        , fmap (\e -> Text.append " AND " e) whereClauseM
        , Just "ORDER BY slotNo ASC, blockNo ASC, txIndexInBlock ASC"
        ]
