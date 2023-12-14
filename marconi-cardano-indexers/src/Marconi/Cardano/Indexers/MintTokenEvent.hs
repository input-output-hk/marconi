{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
module Marconi.Cardano.Indexers.MintTokenEvent (
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
  catchupConfigEventHook,
  mkMintTokenIndexer,
  mintTokenEventWorker,
  mintTokenEventBuilder,
  filterByTargetAssetIds,

  -- * Extract events
  extractEventsFromTx,

  -- * Queries
  MintTokenEventsMatchingQuery (AllEvents, AllMintEvents, AllBurnEvents, ByTargetAssetIds),
  ByTargetAssetIdsArgs (ByTargetAssetIdsArgs),
  evalMintTokenEventsMatchingQuery,
  QueryByAssetId (..),
  queryByAssetIdPolicyId,
  queryByAssetIdAssetName,
  queryByAssetIdEventType,
  queryByAssetIdUpperSlotNo,
  queryByAssetIdLowerTxId,
  EventType (..),
  toTimedMintEvents,
)
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Trace (Trace)
import Cardano.BM.Tracing qualified as BM
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
import Control.Lens (
  Lens',
  folded,
  lens,
  over,
  toListOf,
  view,
  (%~),
  (.~),
  (?~),
  (^.),
  (^?),
 )
import Control.Lens qualified as Lens
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Short qualified as Short
import Data.Foldable (foldlM)
import Data.Function (on, (&))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardSQLiteIndexer,
  StandardWorker,
  StandardWorkerConfig (StandardWorkerConfig),
  mkStandardWorker,
  mkStandardWorkerWithFilter,
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (
  AnyTxBody (AnyTxBody),
  SecurityParam,
  TxIndexInBlock,
 )
import Marconi.Cardano.Indexers.SyncHelper qualified as Sync
import Marconi.Core qualified as Core
import System.FilePath ((</>))

-- | A raw SQLite indexer for 'MintTokenBlockEvents'
type MintTokenEventIndexer = Core.SQLiteIndexer MintTokenBlockEvents

-- | A SQLite 'MintTokenBlockEvents' indexer with Catchup
type StandardMintTokenEventIndexer m = StandardSQLiteIndexer m MintTokenBlockEvents

{- | 'MintTokenEventConfig' allows for specifying a list of @C.'PolicyId'@s and
possibly @C.'AssetName'@s by which to filter a query. 'Nothing' represents the case
in which no filtering should occur.
-}
newtype MintTokenEventConfig = MintTokenEventConfig
  { _configTrackedAssetIds :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  }
  deriving (Show)

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
  , _queryByAssetIdAssetName :: !(Maybe C.AssetName)
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
mintTokenEventWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => StandardWorkerConfig m input MintTokenBlockEvents
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> MintTokenEventConfig
  -- ^ Specific configuration of the indexer (mostly for logging purpose and filtering for target
  -- asset ids)
  -> FilePath
  -> n (StandardWorker m input MintTokenBlockEvents Core.SQLiteIndexer)
mintTokenEventWorker workerConfig (MintTokenEventConfig mTrackedAssetIds) dbPath = do
  sqliteIndexer <- mkMintTokenIndexer dbPath
  case mTrackedAssetIds of
    Nothing -> mkStandardWorker workerConfig sqliteIndexer
    Just trackedAssetIds -> mkStandardWorkerWithFilter workerConfig (filterByTargetAssetIds trackedAssetIds) sqliteIndexer

{- | Convenience wrapper around 'mkMintTokenEventWorker' with some defaults for
creating 'StandardWorkerConfig', including a preprocessor.
-}
mintTokenEventBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> MintTokenEventConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n (StandardWorker IO [AnyTxBody] MintTokenBlockEvents Core.SQLiteIndexer)
mintTokenEventBuilder securityParam catchupConfig mintEventConfig textLogger path =
  let indexerName = "MintTokenEvent"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      mintDbPath = path </> "mint.db"
      catchupConfigWithTracer =
        catchupConfig
          & Core.configCatchupEventHook
            ?~ catchupConfigEventHook indexerName textLogger mintDbPath
      extractMint :: AnyTxBody -> [MintTokenEvent]
      extractMint (AnyTxBody bn ix txb) = extractEventsFromTx bn ix txb
      mintTokenWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfigWithTracer
          (pure . fmap MintTokenBlockEvents . NonEmpty.nonEmpty . (>>= extractMint))
          (BM.appendName indexerName indexerEventLogger)
   in mintTokenEventWorker mintTokenWorkerConfig mintEventConfig mintDbPath

-- | Only keep the MintTokenEvents at a block if they mint a target 'AssetId'.
filterByTargetAssetIds
  :: NonEmpty (C.PolicyId, Maybe C.AssetName) -> MintTokenBlockEvents -> Maybe MintTokenBlockEvents
filterByTargetAssetIds assetIds =
  fmap MintTokenBlockEvents
    . NonEmpty.nonEmpty
    . NonEmpty.filter ((`hasPolicyAndMaybeName` assetIds) . view (mintTokenEventAsset . mintAssetAssetId))
    . view mintTokenEvents
  where
    hasPolicyAndMaybeName :: C.AssetId -> NonEmpty (C.PolicyId, Maybe C.AssetName) -> Bool
    hasPolicyAndMaybeName C.AdaAssetId _ = False
    hasPolicyAndMaybeName (C.AssetId pid name) pidLookup = any (isPolicyAndMaybeName pid name) pidLookup
    isPolicyAndMaybeName :: C.PolicyId -> C.AssetName -> (C.PolicyId, Maybe C.AssetName) -> Bool
    isPolicyAndMaybeName pid _ (pid', Nothing) = pid == pid'
    isPolicyAndMaybeName pid name (pid', Just name') = pid == pid' && name == name'

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
      createMintPolicyEventTables = [createMintPolicyEvent]
      mintInsertPlans = [Core.SQLInsertPlan fromTimedMintEvents mintEventInsertQuery]
  Sync.mkSyncedSqliteIndexer
    dbPath
    createMintPolicyEventTables
    [mintInsertPlans]
    [Core.SQLRollbackPlan "minting_policy_events" "slotNo" C.chainPointToSlotNo]

catchupConfigEventHook :: Text -> Trace IO Text -> FilePath -> Core.CatchupEvent -> IO ()
catchupConfigEventHook indexerName stdoutTrace dbPath Core.Synced = do
  SQL.withConnection dbPath $ \c -> do
    let txIdPolicyIdIndexName = "minting_policy_events__txId_policyId"
        createMintPolicyIdIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString txIdPolicyIdIndexName
            <> " ON minting_policy_events (txId, policyId)"
    Core.createIndexTable
      indexerName
      stdoutTrace
      c
      txIdPolicyIdIndexName
      createMintPolicyIdIndexStatement

    let slotNoIndexName = "minting_policy_events__slotNo"
        createSlotNoIndexStatement =
          "CREATE INDEX IF NOT EXISTS "
            <> fromString slotNoIndexName
            <> " ON minting_policy_events (slotNo)"
    Core.createIndexTable indexerName stdoutTrace c slotNoIndexName createSlotNoIndexStatement

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

allEvents :: MintTokenBlockEvents -> Maybe MintTokenBlockEvents
allEvents = Just

allMintEvents :: MintTokenBlockEvents -> Maybe MintTokenBlockEvents
allMintEvents (MintTokenBlockEvents events) =
  fmap MintTokenBlockEvents $
    NonEmpty.nonEmpty $
      NonEmpty.filter (\e -> e ^. mintTokenEventAsset . mintAssetQuantity > 0) events

allBurnEvents :: MintTokenBlockEvents -> Maybe MintTokenBlockEvents
allBurnEvents (MintTokenBlockEvents events) =
  fmap MintTokenBlockEvents $
    NonEmpty.nonEmpty $
      NonEmpty.filter (\e -> e ^. mintTokenEventAsset . mintAssetQuantity < 0) events

newtype ByTargetAssetIdsArgs = ByTargetAssetIdsArgs (NonEmpty (C.PolicyId, Maybe C.AssetName))
  deriving (Show, Eq, Ord)

-- | Defunctionalisation of queries
data MintTokenEventsMatchingQuery event
  = AllEvents
  | AllMintEvents
  | AllBurnEvents
  | ByTargetAssetIds ByTargetAssetIdsArgs
  deriving (Show, Eq, Ord)

-- | Evaluator for 'MintTokenEventsMatchingQuery'
evalMintTokenEventsMatchingQuery
  :: MintTokenEventsMatchingQuery MintTokenBlockEvents
  -> MintTokenBlockEvents
  -> Maybe MintTokenBlockEvents
evalMintTokenEventsMatchingQuery AllEvents = allEvents
evalMintTokenEventsMatchingQuery AllMintEvents = allMintEvents
evalMintTokenEventsMatchingQuery AllBurnEvents = allBurnEvents
evalMintTokenEventsMatchingQuery (ByTargetAssetIds (ByTargetAssetIdsArgs assetIds)) =
  filterByTargetAssetIds assetIds

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
          (\p r -> parseResult (evalMintTokenEventsMatchingQuery p) r)

instance
  (MonadError (Core.QueryError (MintTokenEventsMatchingQuery MintTokenBlockEvents)) m)
  => Core.Queryable
      m
      MintTokenBlockEvents
      (MintTokenEventsMatchingQuery MintTokenBlockEvents)
      Core.ListIndexer
  where
  query p (evalMintTokenEventsMatchingQuery -> predicate) ix = do
    let convertError
          :: Core.QueryError (Core.EventsMatchingQuery MintTokenBlockEvents)
          -> Core.QueryError (MintTokenEventsMatchingQuery MintTokenBlockEvents)
        convertError = \case
          Core.NotStoredAnymore -> Core.NotStoredAnymore
          (Core.IndexerQueryError t) -> Core.IndexerQueryError t
          (Core.AheadOfLastSync r) -> Core.AheadOfLastSync r
          (Core.SlotNoBoundsInvalid r) -> Core.SlotNoBoundsInvalid r

    timedEventsE <- runExceptT $ Core.query p (Core.EventsMatchingQuery predicate) ix
    timedEvents <- either (throwError . convertError) pure timedEventsE
    pure $ sortEventsByOrderOfBlockchainAppearance timedEvents

type instance Core.Result (QueryByAssetId event) = [Core.Timed (Core.Point event) event]
type instance
  Core.Result (Core.WithStability (QueryByAssetId event)) =
    [Core.Stability (Core.Timed (Core.Point event) event)]

{- | Utility for QueryByAssetId queries. Check whether the upper SlotNo bound is
 less than or equal to the provided @C.'ChainPoint'@. If the upperSlotNo is is provided
 but is after the point, return Nothing. Otherwise, return the valid upper SlotNo.
 If no upper SlotNo bound is provided, return the @C.'ChainPoint'@.
-}
upperSlotNoIfValid :: C.ChainPoint -> Maybe C.SlotNo -> Maybe C.SlotNo
upperSlotNoIfValid C.ChainPointAtGenesis _ = Nothing
upperSlotNoIfValid (C.ChainPoint sn _) Nothing = Just sn
upperSlotNoIfValid (C.ChainPoint sn _) (Just usn) = if usn <= sn then Just usn else Nothing

{- | Utility for QueryByAssetId queries. Wraps 'upperSlotNoIfValid', throwing an error
 if that functions returns Nothing. Otherwise, returns the validated upper bound if provided or
 returns the slot number of the input chain point if no upper bound is provided.
-}
validatedUpperBound
  :: (MonadError (Core.QueryError (QueryByAssetId MintTokenBlockEvents)) m)
  => C.ChainPoint
  -> Maybe C.SlotNo
  -> m C.SlotNo
validatedUpperBound cp sn =
  let err =
        Core.SlotNoBoundsInvalid $
          "Point "
            <> Text.pack (show cp)
            <> " precedes query upper SlotNo "
            <> Text.pack (show sn)
   in maybe (throwError err) pure $ upperSlotNoIfValid cp sn

instance
  forall m
   . ( MonadIO m
     , MonadError (Core.QueryError (QueryByAssetId MintTokenBlockEvents)) m
     )
  => Core.Queryable m MintTokenBlockEvents (QueryByAssetId MintTokenBlockEvents) Core.ListIndexer
  where
  query point (QueryByAssetId policyId assetNameM eventType upperSlotNo lowerTxId) ix = do
    -- Check whether the upperSlotNo is valid, if provided.
    -- If not, e.g. point is genesis, throw an error before any queries.
    -- If upperSlotNo is Nothing, return slot number of point.
    validUpperSlotNo <- validatedUpperBound point upperSlotNo

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

    timedEventsE <- runExceptT $ Core.query point queryByAssetIdPredicate ix
    timedEvents <-
      either (throwError . convertEventsMatchingErrorToQueryByAssetIdError) pure timedEventsE
    let sortedTimedEvents = sortEventsByOrderOfBlockchainAppearance timedEvents

    -- Filter timedEvents to within the upper/lower slot bounds. Throws an error only if
    -- lowerTxId slotNo is found and is not less than or equal to the upper bound. Note sorting
    -- ascending by block number in sortEventsByOrderOfBlockchainAppearance guarantees ascending
    -- order by slot number.
    either throwError pure $ filterBySlotNoBounds lowerTxId validUpperSlotNo sortedTimedEvents

instance
  ( MonadIO m
  , MonadError (Core.QueryError (Core.WithStability (QueryByAssetId MintTokenBlockEvents))) m
  )
  => Core.Queryable
      m
      MintTokenBlockEvents
      (Core.WithStability (QueryByAssetId MintTokenBlockEvents))
      Core.ListIndexer
  where
  query p (Core.WithStability q) idx = do
    lsp <- Core.lastStablePoint idx
    Core.withStabilityM (Core.calcStability (view Core.point) lsp) $ Core.query p q idx

{- | Helper for ListIndexer query.
 Filter timed events to within the upper/lower slot bounds, returning 'Left' only if
 lowerTxId slot number is found and is not <= the upper bound. This function assumes the
 events are sorted in ascending order by ChainPoint SlotNo. It's purpose is to avoid a double
 pass in first finding the slot number associated with the lower bound.

 It requires there to be a matching transaction for 'lowerTxId' and will not distinguish between
 cases where there are no such transactions and ones where no events are below the upper bound.
-}
filterBySlotNoBounds
  :: Maybe C.TxId
  -> C.SlotNo
  -> [Core.Timed C.ChainPoint MintTokenBlockEvents]
  -> Either
      (Core.QueryError (QueryByAssetId MintTokenBlockEvents))
      [Core.Timed C.ChainPoint MintTokenBlockEvents]
filterBySlotNoBounds txIdM validUpperSlotNo =
  let
    getSlotNo e = C.chainPointToSlotNo (e ^. Core.point)
    isBeforeUpperSlotNo sn e = getSlotNo e <= Just sn
    findLowerAndFilter txId (es, False) e
      -- This tx is the one setting the lower bound. Check for validity, then accumulate if <=
      -- the upper bound.
      | matchingTxIdFromTimedEvent txId e =
          if isBeforeUpperSlotNo validUpperSlotNo e
            then Right (e : es, True)
            else
              Left $
                Core.SlotNoBoundsInvalid "SlotNo associated with query lowerTxId not found within upperSlotNo bound"
      | otherwise = Right (es, False)
    -- The lower bound has already been found and validated. Accumulate according to upper bound.
    findLowerAndFilter _ (es, True) e = Right (accumulateIfWithinUpper validUpperSlotNo es e, True)
    accumulateIfWithinUpper upper es e = if isBeforeUpperSlotNo upper e then e : es else es
    matchingTxIdFromTimedEvent txId =
      Lens.anyOf
        (Core.event . mintTokenEvents . Lens.folded . mintTokenEventLocation . mintTokenEventTxId)
        (== txId)
   in
    case txIdM of
      Nothing -> Right . filter (isBeforeUpperSlotNo validUpperSlotNo)
      Just txId -> fmap fst . foldlM (findLowerAndFilter txId) ([], False)

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
  (Core.SlotNoBoundsInvalid r) -> Core.SlotNoBoundsInvalid r

instance
  (MonadIO m, MonadError (Core.QueryError (QueryByAssetId MintTokenBlockEvents)) m)
  => Core.Queryable m MintTokenBlockEvents (QueryByAssetId MintTokenBlockEvents) Core.SQLiteIndexer
  where
  query point config ix = do
    validUpperSlotNo <- validatedUpperBound point (config ^. queryByAssetIdUpperSlotNo)
    lowerSlotNo <- queryLowerSlotNoByTxId validUpperSlotNo point config ix

    -- Build the main query
    let
      -- These parameter values and clauses can never be null.
      namedParamsBase =
        [ ":slotNo" := validUpperSlotNo
        , ":policyId" := config ^. queryByAssetIdPolicyId
        ]
      policyIdWhereClause = Just "policyId = :policyId"

      -- These cases handle nulls
      lowerSlotNoParam = maybe [] (\x -> [":lowerSlotNo" := x]) lowerSlotNo
      lowerSlotNoWhereClause = fmap (const "slotNo >= :lowerSlotNo") lowerSlotNo
      assetNameParam = maybe [] (\x -> [":assetName" := x]) (config ^. queryByAssetIdAssetName)
      assetNameWhereClause = fmap (const "assetName = :assetName") (config ^. queryByAssetIdAssetName)
      eventTypeWhereClause =
        case config ^. queryByAssetIdEventType of
          Nothing -> Nothing
          Just MintEventType -> Just "quantity > 0"
          Just BurnEventType -> Just "quantity < 0"

      namedParams = concat [namedParamsBase, assetNameParam, lowerSlotNoParam]
      queryByAssetId =
        let
          whereClause =
            fmap (\e -> Text.intercalate " AND " $ NonEmpty.toList e) $
              NonEmpty.nonEmpty $
                catMaybes [policyIdWhereClause, assetNameWhereClause, eventTypeWhereClause, lowerSlotNoWhereClause]
         in
          mkMintTokenEventQueryBy whereClause

    Core.querySyncedOnlySQLiteIndexerWith
      (const . const namedParams)
      (const queryByAssetId)
      (const toTimedMintEvents)
      point
      config
      ix

{- | Helper for MintTokenEventIndexer in the case where a 'lowerTxId' is provided.
Query to look up the earliest SlotNo matching a TxId. This is implemented as a separate query so
as to be able to check that the 'lowerSlotNo' found is <= 'upperSlotNo'. If not, it will
throw an error.
-}
queryLowerSlotNoByTxId
  :: (MonadIO m, MonadError (Core.QueryError (QueryByAssetId MintTokenBlockEvents)) m)
  => C.SlotNo
  -> C.ChainPoint
  -> QueryByAssetId MintTokenBlockEvents
  -> Core.SQLiteIndexer MintTokenBlockEvents
  -> m (Maybe C.SlotNo)
queryLowerSlotNoByTxId upperSlotNo point config ix =
  let
    lowerTxIdQuery :: SQL.Query
    lowerTxIdQuery = mkMintTokenEventQueryBy (Just "txId = :lowerTxId")

    lowerTxIdQueryNamedParams :: C.TxId -> [SQL.NamedParam]
    lowerTxIdQueryNamedParams lowerTxId = [":lowerTxId" := lowerTxId, ":slotNo" := upperSlotNo]

    slotNoOfLowerTxIdQuery rs = listToMaybe rs >>= \r -> C.chainPointToSlotNo (r ^. Core.point)

    handleSlotNoLowerTxIdQuery Nothing =
      Left $
        Core.SlotNoBoundsInvalid "SlotNo associated with query lowerTxId not found within upperSlotNo bound"
    handleSlotNoLowerTxIdQuery (Just sn) = Right sn
   in
    case config ^. queryByAssetIdLowerTxId of
      Nothing -> pure Nothing
      Just lower -> do
        lowerSlotNoE <-
          runExceptT $
            Core.querySyncedOnlySQLiteIndexerWith
              (const . const (lowerTxIdQueryNamedParams lower))
              (const lowerTxIdQuery)
              (const toTimedMintEvents)
              point
              config
              ix
        either
          throwError
          (pure . Just)
          (lowerSlotNoE >>= handleSlotNoLowerTxIdQuery . slotNoOfLowerTxIdQuery)

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
