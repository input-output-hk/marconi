{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Experimental.Indexers.Utxo where

import Control.Lens (folded, imap, makeLenses, view, (&), (.~), (^.), (^..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (fromRight)
import Data.Foldable (fold, foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord ()
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import Text.RawString.QQ (r)

import Cardano.Api ()
import Cardano.Api qualified as C
import Cardano.Api qualified as Core
import Cardano.Api.Shelley qualified as C
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam), TargetAddresses, TxOut, pattern CurrentEra)
import Marconi.Core.Experiment qualified as Core
import Marconi.Core.Experiment.Indexer.ListIndexer (listIndexer)
import Marconi.Core.Experiment.Indexer.MixedIndexer (newMixedIndexer)

data Utxo = Utxo
  { _utxoAddress          :: !C.AddressAny
  , _utxoTxIn             :: !C.TxIn
  , _utxoDatum            :: !(Maybe C.ScriptData)
  , _utxoDatumHash        :: !(Maybe (C.Hash C.ScriptData))
  , _utxoValue            :: !C.Value
  , _utxoInlineScript     :: !(Maybe C.ScriptInAnyLang)
  , _utxoInlineScriptHash :: !(Maybe C.ScriptHash)
  } deriving (Show, Eq, Generic)

$(makeLenses ''Utxo)

instance Ord Utxo where
  compare (Utxo _ txin _ _ _ _ _) (Utxo _ txin' _ _ _ _ _) =
    compare txin txin'

newtype Spent = Spent
  { unSpent ::  C.TxIn
  } deriving (Show, Eq, Ord)

data UtxoEvent = UtxoEvent
    { _ueUtxos  :: !(Set Utxo)
    , _ueInputs :: !(Set Spent)
    } deriving (Eq, Ord, Show, Generic)

$(makeLenses ''UtxoEvent)

type instance Core.Point UtxoEvent = C.ChainPoint
type instance Core.Point Utxo = C.ChainPoint  -- required for the unspent_transaction table SQL mappings
type instance Core.Point Spent = C.ChainPoint -- required for the spent table SQL mappings

instance Core.HasGenesis C.ChainPoint where
  genesis = C.ChainPointAtGenesis

newtype QueryUtxoByAddress = QueryUtxoByAddress (C.AddressAny, Maybe C.SlotNo)
  deriving (Eq, Ord, Show)

type instance Core.Result QueryUtxoByAddress = [Core.TimedEvent Utxo] -- use this instead of the utxorow

-- | Take the query results from two indexers, merge the results and re-compute Unspent
instance MonadIO m => Core.ResumableResult m UtxoEvent QueryUtxoByAddress Core.ListIndexer where
  resumeResult
    :: Ord (Core.Point UtxoEvent)
    => Core.Point UtxoEvent -- ^ give me all you know to this point
    -> QueryUtxoByAddress  -- ^ utxoEvent query
    -> Core.ListIndexer UtxoEvent -- ^ the indexer
    -> m (Core.Result QueryUtxoByAddress) -- ^ any result set from other indexers
    -> m (Core.Result QueryUtxoByAddress)
  resumeResult C.ChainPointAtGenesis _ _ _ = pure []
  resumeResult cp@(C.ChainPoint _ _) q ix queryResult =
    let

      inputsTxIns :: UtxoEvent -> Set C.TxIn
      inputsTxIns = Set.map unSpent . _ueInputs

      txins ::  Set C.TxIn
      txins = foldl' (\a c -> inputsTxIns c `Set.union` a) Set.empty utxoEvents

      utxoEvents :: [UtxoEvent]
      utxoEvents = ix ^. Core.events ^.. folded . Core.event

      computeUnspent:: Core.Result QueryUtxoByAddress -> Core.Result QueryUtxoByAddress
      computeUnspent= filter (\(Core.TimedEvent _ utxo)
                            -> utxo ^. utxoTxIn `notElem` txins)
    in do
      mem :: (Core.Result QueryUtxoByAddress) <- Core.query cp q ix
      disk <- queryResult
      pure . computeUnspent $  disk <> mem -- merging and reduce ondisk & in-memory results

-- | Query the in-memory indexer
instance MonadIO m => Core.Queryable m UtxoEvent QueryUtxoByAddress Core.ListIndexer  where
  query
    :: Ord (Core.Point UtxoEvent)
    =>  Core.Point UtxoEvent -- ^ give me what you know up to this point, potentially a point in future
    -> QueryUtxoByAddress
    -> Core.ListIndexer UtxoEvent -- ^ get the point for ListIndexer
    -> m (Core.Result QueryUtxoByAddress)
  query C.ChainPointAtGenesis _ _ = pure []
   -- TODO we're ignoring future spent query for this version
  query _ q listindexer =
    let

      queryTimedUtxoEvent :: QueryUtxoByAddress -> Core.TimedEvent UtxoEvent -> Core.TimedEvent UtxoEvent
      queryTimedUtxoEvent (QueryUtxoByAddress (addr, maybeSno)) timedutxoevent =
        let

          isBeforeSlot :: C.SlotNo -> Core.TimedEvent UtxoEvent  -> Bool
          isBeforeSlot s te' = case te' ^. Core.point of
            C.ChainPointAtGenesis -> True
            C.ChainPoint s' _     -> s' <= s

          pointFilter :: Maybe C.SlotNo -> (Core.TimedEvent UtxoEvent -> Bool)
          pointFilter = maybe (const True) (\s -> isBeforeSlot s)

          splitEventAtAddress :: UtxoEvent -> [UtxoEvent]
          splitEventAtAddress event =
            let

              utxosAtAddress :: Set Utxo
              utxosAtAddress = Set.filter (\u -> (u ^. utxoAddress) == addr) $ _ueUtxos event

            in [ event {_ueUtxos = utxosAtAddress} | not (null utxosAtAddress) ]

        in fold
           . filter (pointFilter maybeSno) -- filter for query slotNo
           . fmap (Core.TimedEvent (timedutxoevent ^. Core.point)) -- filter for address
           $ splitEventAtAddress (timedutxoevent ^. Core.event)

    in pure
       . concatMap timedUtxosFromTimedUtxoEvent
       . fmap (queryTimedUtxoEvent q)
       . view Core.events
       $ listindexer

-- | Get Timed Utxo from Timed UtxoEvent
timedUtxosFromTimedUtxoEvent :: Core.TimedEvent UtxoEvent -> [Core.TimedEvent Utxo]
timedUtxosFromTimedUtxoEvent timedUtxoEvent =
  [
    Core.TimedEvent (timedUtxoEvent ^. Core.point) utxo
    | utxo <- Set.toList (timedUtxoEvent ^. Core.event . ueUtxos)
  ]

-- | Get Timed Spent from Timed UtxoEvent
timedSpentsFromTimedUtxoEvent :: Core.TimedEvent UtxoEvent -> [Core.TimedEvent Spent]
timedSpentsFromTimedUtxoEvent timedUtxoEvent =
  [
    Core.TimedEvent (timedUtxoEvent ^. Core.point) spent
    | spent <- Set.toList (timedUtxoEvent ^. Core.event . ueInputs)
  ]

type instance Core.InsertRecord UtxoEvent =
  ([Core.TimedEvent Utxo], [Core.TimedEvent Spent])

-- | Make a SQLiteIndexer to store indexer in SQLite
sqliteIndexer
  :: SQL.Connection -- ^ SQL connection to database
  -> Core.SQLiteIndexer UtxoEvent
sqliteIndexer conn =
  let
    utxoInsertQuery:: SQL.Query -- Utxo table SQL statement
    utxoInsertQuery =
            [r|INSERT
               INTO unspent_transactions (
                 address,
                 txId,
                 txIx,
                 datum,
                 datumHash,
                 value,
                 inlineScript,
                 inlineScriptHash,
                 slotNo,
                 blockHash
              ) VALUES
              (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|]

    spentInsertQuery :: SQL.Query -- Spent table SQL statements
    spentInsertQuery =
            [r|INSERT
              INTO spent (
                txId,
                txIx,
                slotNo,
                blockHash
              ) VALUES
              (?, ?, ?, ?)|]

    prepareInsert'
      :: Core.TimedEvent UtxoEvent -- UtxoEvent
      -> Core.InsertRecord UtxoEvent  -- SQL mapping of Utxo and Spent parameters
    prepareInsert' utxoEv =
      let

        tUtxos = timedUtxosFromTimedUtxoEvent utxoEv -- timed Utxo
        tSpents = timedSpentsFromTimedUtxoEvent utxoEv -- timed Spent

      in (tUtxos, tSpents)

    buildInsert' :: Core.InsertRecord UtxoEvent -> [[Core.IndexQuery]]
    buildInsert' (us, ss) = pure
      [Core.IndexQuery utxoInsertQuery us, Core.IndexQuery spentInsertQuery ss]

  in Core.SQLiteIndexer
      conn
      prepareInsert'
      buildInsert'
      Core.genesis

-- | combine UtxoEvents and balance
instance Semigroup UtxoEvent where
  (UtxoEvent outs ins) <> (UtxoEvent outs' ins') =
    let

      insTxins :: Set C.TxIn
      insTxins = Set.union (Set.map unSpent ins) (Set.map unSpent ins')

      utxos' :: Set Utxo
      utxos' = Set.union outs outs'

      outsTxins :: Set C.TxIn
      outsTxins = (Set.fromList $ utxos' ^.. folded . utxoTxIn)

      utxos :: Set Utxo
      utxos = foldl' (\us u ->
                    if (u ^. utxoTxIn) `Set.notMember` insTxins then
                      Set.insert u us
                    else us
                 ) Set.empty utxos'

      spents :: Set Spent
      spents = Set.map Spent $  foldl'(\txins txin ->
                         if txin `Set.notMember` outsTxins then
                           Set.insert txin txins
                          else txins
                                      ) Set.empty insTxins

    in UtxoEvent utxos spents

instance Monoid UtxoEvent where
  mempty = UtxoEvent Set.empty Set.empty

instance Semigroup (Core.TimedEvent UtxoEvent) where
  te <> te' =
    let

      point = max (te ^. Core.point) (te' ^. Core.point)
      event = (te ^. Core.event) <> (te' ^. Core.event)

    in Core.TimedEvent point event

instance Monoid (Core.TimedEvent UtxoEvent) where
  mempty = Core.TimedEvent Core.ChainPointAtGenesis mempty
-- | createla SQLite tables
--
initSQLite :: FilePath -> IO SQL.Connection
initSQLite dbPath = do
  c <- SQL.open dbPath

  -- allow for concurrent insert/query.
  -- see SQLite WAL, https://www.sqlite.org/wal.html
  SQL.execute_ c "PRAGMA journal_mode=WAL"

  SQL.execute_ c [r|CREATE TABLE IF NOT EXISTS unspent_transactions
                      ( address BLOB NOT NULL
                      , txId TEXT NOT NULL
                      , txIx INT NOT NULL
                      , datum BLOB
                      , datumHash BLOB
                      , value BLOB
                      , inlineScript BLOB
                      , inlineScriptHash BLOB
                      , slotNo INT
                      , blockHash BLOB)|]

  SQL.execute_ c [r|CREATE TABLE IF NOT EXISTS spent
                      ( txId TEXT NOT NULL
                      , txIx INT NOT NULL
                      , slotNo INT
                      , blockHash BLOB)|]
  -- TODO
  -- Consider adding indices per user request through CLI or other type of configuration
 --  Well create indices on these tables once we have completed all the required quries

  pure c

-- | Make a SQLiteIndexer
mkMixedIndexer
  :: SQL.Connection
  -> SecurityParam -- ^ We use securityParam to set the Indexers flush sise to database
  -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer UtxoEvent
mkMixedIndexer conn (SecurityParam w64) =
  let

    keepInMemory::Word = fromIntegral w64  -- how much to keep in memory to minimize disk rollbacks
    flushsize::Word = keepInMemory`div` 6  -- how often to flush to disk

  in mkMixedIndexer' conn keepInMemory flushsize

-- | Make a SQLiteIndexer
mkMixedIndexer'
  :: SQL.Connection
  -> Word -- ^  events keept in memory post flush
  -> Word -- ^ flush size
  -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer UtxoEvent
mkMixedIndexer' conn keep flush =
    newMixedIndexer keep flush (sqliteIndexer conn) listIndexer

-----------------------------------------------------------------------
-- SQL table mappings
-----------------------------------------------------------------------
-- | SQL Row mapping for Utxo to unspent_transactions
instance ToRow (Core.TimedEvent Utxo) where
  toRow u =
    let

      (C.TxIn txid txix) = u ^. Core.event . utxoTxIn
      (snoField, bhhField) = case u ^. Core.point of
        C.ChainPointAtGenesis  -> (SQL.SQLNull,SQL.SQLNull)
        (C.ChainPoint sno bhh) -> (toField sno, toField bhh)

    in toRow
    ( toField (u ^. Core.event . utxoAddress)
    , toField txid
    , toField txix
    , toField (u ^. Core.event . utxoDatum)
    , toField (u ^. Core.event . utxoDatumHash)
    , toField (u ^. Core.event . utxoValue)
    , toField (u ^. Core.event . utxoInlineScript)
    , toField (u ^. Core.event . utxoInlineScriptHash)
    , snoField
    , bhhField
    )

instance FromRow (Core.TimedEvent Utxo) where
  fromRow
    = flip Core.TimedEvent
    <$> ( Utxo
        <$> field
        <*> (C.TxIn <$> field <*> field)
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field )
    <*> ( C.ChainPoint <$> field <*> field)

instance ToRow (Core.TimedEvent Spent) where
  toRow s =
    let

      (C.TxIn txid txix) = unSpent . view Core.event $ s
      (snoField, bhhField) = case s ^. Core.point of
        C.ChainPointAtGenesis  -> (SQL.SQLNull,SQL.SQLNull)
        (C.ChainPoint sno bhh) -> (toField sno, toField bhh)

    in toRow
            ( toField txid
            , toField txix
            , snoField
            , bhhField)

instance FromRow (Core.TimedEvent Spent) where
  fromRow
    = flip Core.TimedEvent
    <$> ( Spent
        <$> (C.TxIn <$> field <*> field))
    <*> ( C.ChainPoint <$> field <*> field)

-----------------------------------------------------------------------
-- Utxo Address Query
-----------------------------------------------------------------------

-- | Queryable SQLiteIndexer instance
--  We connect the query actions to SQLiteIndexer in this instance
-- instance (MonadIO m, MonadError (Core.QueryError QueryUtxoByAddress) m)
instance (MonadIO m)
    => Core.Queryable m UtxoEvent QueryUtxoByAddress Core.SQLiteIndexer where
  -- query
  --   :: (MonadError (Core.QueryError QueryUtxoByAddress) m, Ord (Core.Point UtxoEvent))
  --   => Core.Point UtxoEvent
  --   -> QueryUtxoByAddress
  --   -> Core.SQLiteIndexer UtxoEvent
  --   -> m (Core.Result QueryUtxoByAddress)
  query cp q (Core.SQLiteIndexer conn _ _ _) =
    let

      action :: SQL.Connection -> IO (Core.Result QueryUtxoByAddress)
      action = mkUtxoAddressQueryAction cp q

    in liftIO $ action conn

-- | Here we define our QueryAction functions
mkUtxoAddressQueryAction
  :: MonadIO m
  => Core.Point UtxoEvent -- ^ chain point to compute/query future spent
  -> QueryUtxoByAddress -- ^ Query
  -> (SQL.Connection -> m (Core.Result QueryUtxoByAddress)) -- we rerturn a query action funtion
mkUtxoAddressQueryAction C.ChainPointAtGenesis _ = \_ -> pure []
mkUtxoAddressQueryAction (C.ChainPoint futureSpentSlotNo _) (QueryUtxoByAddress (addr, slotNo)) =
  let

    _ = futureSpentSlotNo  -- TODO this is place holder to build the `Future Spent` as required by SideChain
    filterPairs :: ([SQL.Query], [NamedParam])
    filterPairs = (["u.address = :address"], [":address" := addr])
      <> maybe mempty (\sno -> (["u.slotNo <= :slotNo"] , [":slotNo" := sno])) slotNo
      -- TODO build the future spent in the above filer

    mkUtxoAddressQueryAction'
      :: MonadIO m
      => [SQL.Query] -- ^ the filter part of the query
      -> [NamedParam]
      -> (SQL.Connection -> m (Core.Result QueryUtxoByAddress) )
    mkUtxoAddressQueryAction' filters params=
      let builtQuery =
                [r|SELECT
                      u.address,
                      u.txId,
                      u.txIx,
                      u.datum,
                      u.datumHash,
                      u.value,
                      u.inlineScript,
                      u.inlineScriptHash,
                      u.slotNo,
                      u.blockHash
                  FROM
                      unspent_transactions u
                  LEFT JOIN spent s ON u.txId = s.txId
                  AND u.txIx = s.txIx
                  WHERE
                      s.txId IS NULL
                      AND s.txIx IS NULL
                  AND |] <> SQL.Query (Text.intercalate " AND " $ SQL.fromQuery <$> filters) <>
                [r| ORDER BY
                    u.slotNo ASC |]

      in \conn -> liftIO $ SQL.queryNamed conn builtQuery params

  in uncurry mkUtxoAddressQueryAction' filterPairs

instance MonadIO m => Core.Rollbackable m UtxoEvent Core.SQLiteIndexer where

    rollback C.ChainPointAtGenesis ix = do
      let c = ix ^. Core.handle
      liftIO $ SQL.execute_ c "DELETE FROM TABLE unspent_transactions"
      liftIO $ SQL.execute_ c "DELETE FROM TABLE spent"

      pure $ ix & Core.dbLastSync .~ C.ChainPointAtGenesis

    rollback p@(C.ChainPoint sno _) ix = do
      let c = ix ^. Core.handle
      liftIO $ SQL.execute c "DELETE FROM unspent_transactions WHERE slotNo > ?" (SQL.Only sno)
      liftIO $ SQL.execute c "DELETE FROM spent WHERE slotNo > ?" (SQL.Only sno)
      pure $ ix & Core.dbLastSync .~ p


-----------------------------------------------------------------------
-- copy paste from Marconi.ChainIndex.Indexers.Utxo
-----------------------------------------------------------------------

-- | Extract UtxoEvents from Cardano Block
getUtxoEventsFromBlock
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> C.Block era
  -> Core.TimedEvent UtxoEvent -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEventsFromBlock maybeTargetAddresses (C.Block (C.BlockHeader slotNo hsh _) txs) =
  getUtxoEvents maybeTargetAddresses txs (C.ChainPoint slotNo hsh)

-- | Extract UtxoEvents from Cardano Transactions
getUtxoEvents
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> [C.Tx era]
  -> C.ChainPoint
  -> Core.TimedEvent UtxoEvent -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents _ _ C.ChainPointAtGenesis =
   Core.TimedEvent  C.ChainPointAtGenesis (UtxoEvent Set.empty Set.empty)
getUtxoEvents maybeTargetAddresses txs cp =
  let
    (TxOutBalance utxos spentTxOuts) =
      foldMap (balanceUtxoFromTx maybeTargetAddresses) txs

    resolvedUtxos :: Set Utxo
    resolvedUtxos = Set.fromList $ Map.elems utxos

    spents :: Set Spent
    spents = Set.map Spent spentTxOuts

    event :: UtxoEvent
    event = UtxoEvent resolvedUtxos spents
  in
   Core.TimedEvent cp event

-- | Extract TxOut from Cardano TxBodyContent
getTxOutFromTxBodyContent :: C.TxBodyContent build era -> [C.TxOut C.CtxTx era]
getTxOutFromTxBodyContent C.TxBodyContent {C.txOuts, C.txReturnCollateral, C.txScriptValidity} = case txScriptValidityToScriptValidity txScriptValidity of
  C.ScriptValid   -> txOuts -- When transaction is valid, only transaction fee is collected
  C.ScriptInvalid -> collateral txReturnCollateral -- failed Tx, we collect from collateral and return excess collateral
  where
    collateral C.TxReturnCollateralNone       = []
    collateral (C.TxReturnCollateral _ txout) = [txout]

-- | Extract Utxos from Cardano TxBody
getUtxosFromTxBody
  :: (C.IsCardanoEra era)
  => Maybe TargetAddresses
  -> C.TxBody era
  -> Map C.TxIn Utxo
getUtxosFromTxBody maybeTargetAddresses txBody@(C.TxBody txBodyContent@C.TxBodyContent{} )=
  fromRight Map.empty (getUtxos $ getTxOutFromTxBodyContent txBodyContent)
  where
    getUtxos :: C.IsCardanoEra era => [C.TxOut C.CtxTx era] -> Either C.EraCastError (Map C.TxIn Utxo)
    getUtxos
      = fmap (Map.fromList . concatMap Map.toList . imap txoutToUtxo)
      . traverse (C.eraCast CurrentEra)

    txid = C.getTxId txBody
    txoutToUtxo :: Int -> TxOut -> Map C.TxIn Utxo
    txoutToUtxo ix txout =
      let

        txin = C.TxIn txid (C.TxIx (fromIntegral ix))

      in case getUtxoFromTxOut maybeTargetAddresses txin txout of
          Nothing   -> Map.empty
          Just utxo -> Map.singleton txin utxo

-- | Extract Utxos from Cardano TxOut
getUtxoFromTxOut
  :: Maybe TargetAddresses -- ^ Target addresses to filter for
  -> C.TxIn -- ^ unique id and position of this transaction
  -> C.TxOut C.CtxTx era -- ^ Cardano TxOut
  -> Maybe Utxo -- ^ Utxo
getUtxoFromTxOut maybeTargetAddresses txin  (C.TxOut addr val dtum refScript) =
  if isAddressInTarget maybeTargetAddresses addrAny
  then Just $ Utxo
    { _utxoTxIn = txin
    , _utxoAddress = addrAny
    , _utxoValue = C.txOutValueToValue val
    , _utxoDatum = datum'
    , _utxoDatumHash = datumHash'
    , _utxoInlineScript = inlineScript'
    , _utxoInlineScriptHash = inlineScriptHash'
    }
  else Nothing
  where
    addrAny = toAddr addr
    (datum', datumHash') = getScriptDataAndHash dtum
    (inlineScript', inlineScriptHash') = getRefScriptAndHash refScript

-- | get the inlineScript and inlineScriptHash
getRefScriptAndHash
  :: C.ReferenceScript era
  -> (Maybe C.ScriptInAnyLang, Maybe C.ScriptHash)
getRefScriptAndHash refScript = case refScript of
  C.ReferenceScriptNone -> (Nothing, Nothing)
  C.ReferenceScript _ s@(C.ScriptInAnyLang C.SimpleScriptLanguage script) ->
      ( Just  s
      , Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script)->
    ( Just s
    , Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script)->
    ( Just s
    , Just . C.hashScript $ script)

-- | Get the datum hash and datum or a transaction output.
getScriptDataAndHash
  :: C.TxOutDatum C.CtxTx era
  -> (Maybe C.ScriptData, Maybe (C.Hash C.ScriptData))
getScriptDataAndHash C.TxOutDatumNone         = (Nothing, Nothing)
getScriptDataAndHash (C.TxOutDatumHash _ h)   = (Nothing, Just h)
getScriptDataAndHash (C.TxOutDatumInTx _ d)   = (Just $ C.getScriptData d, (Just . C.hashScriptDataBytes) d)
getScriptDataAndHash (C.TxOutDatumInline _ d) = (Just $ C.getScriptData d, (Just . C.hashScriptDataBytes) d)

getInputsFromTx :: C.Tx era -> Set C.TxIn
getInputsFromTx (C.Tx txbody _) = getInputs txbody

-- | Compute TxIn
--  If phase-2 validation fails, we only process TxIns associated with collateral
getInputs :: C.TxBody era -> Set C.TxIn
getInputs (C.TxBody C.TxBodyContent
                 {C.txIns, C.txInsCollateral, C.txScriptValidity }) =
  let

    inputs = case txScriptValidityToScriptValidity txScriptValidity of
      C.ScriptValid -> fst <$> txIns
      C.ScriptInvalid -> case txInsCollateral of
        C.TxInsCollateralNone     -> []
        C.TxInsCollateral _ txins -> txins

  in Set.fromList inputs


-- | Duplicated from cardano-api (not exposed in cardano-api)
-- This function should be removed when marconi will depend on a cardano-api version that has accepted this PR:
-- https://github.com/input-output-hk/cardano-node/pull/4569
txScriptValidityToScriptValidity :: C.TxScriptValidity era -> C.ScriptValidity
txScriptValidityToScriptValidity C.TxScriptValidityNone                = C.ScriptValid
txScriptValidityToScriptValidity (C.TxScriptValidity _ scriptValidity) = scriptValidity

-- | does the transaction contain a targetAddress
isAddressInTarget' :: TargetAddresses -> Utxo -> Bool
isAddressInTarget' targetAddresses utxo =
    case utxo ^. utxoAddress  of
      C.AddressByron _       -> False
      C.AddressShelley addr' -> addr' `elem` targetAddresses

balanceUtxoFromTx
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> C.Tx era
  -> TxOutBalance
balanceUtxoFromTx addrs (C.Tx txBody _) =
    let
        txInputs = getInputs txBody -- adjusted txInput after phase-2 validation
        utxoRefs :: Map C.TxIn Utxo
        utxoRefs = getUtxosFromTxBody addrs txBody
    in TxOutBalance utxoRefs txInputs

-- A container to allow balance Utxos in the presensy of Spents
data TxOutBalance =
  TxOutBalance
    { _tbUnspent :: !(Map C.TxIn Utxo)
    -- ^ Outputs newly added by the transaction(s)
    , _tbSpent   :: !(Set C.TxIn)
    -- ^ Outputs spent by the transaction(s)
    }
    deriving stock (Eq, Show, Generic)

instance Semigroup TxOutBalance where
    bUtxoL <> bUtxoR =
      let
        bUnspentKeys :: Set C.TxIn
        bUnspentKeys
          = Map.keysSet (_tbUnspent bUtxoR)
          <> (Map.keysSet (_tbUnspent bUtxoL) `Set.difference` _tbSpent bUtxoR)
        utxoMap :: Map C.TxIn Utxo
        utxoMap = _tbUnspent bUtxoL `Map.union` _tbUnspent bUtxoR
        bSpentKeys :: Set C.TxIn
        bSpentKeys
          = _tbSpent bUtxoL
          <> ( _tbSpent bUtxoR `Set.difference` Map.keysSet (_tbUnspent bUtxoL))
      in
        TxOutBalance
            { _tbUnspent = Map.restrictKeys utxoMap bUnspentKeys
            , _tbSpent = bSpentKeys
            }

instance Monoid TxOutBalance where
    mappend = (<>)
    mempty = TxOutBalance mempty mempty

-- | does the transaction contain a targetAddress
isAddressInTarget :: Maybe TargetAddresses -> C.AddressAny -> Bool
isAddressInTarget Nothing _ = True -- all addresses are target addresses
isAddressInTarget (Just targetAddresses) addr =
    case addr  of
      C.AddressByron _       -> False
      C.AddressShelley addr' -> addr' `elem` targetAddresses

-- | Convert from 'AddressInEra' of the 'CurrentEra' to 'AddressAny'.
toAddr :: C.AddressInEra era -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr)    = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr
