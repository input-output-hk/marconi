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

import Control.Lens (view)
import Control.Lens.Combinators (imap)
import Control.Lens.Fold (folded)
import Control.Lens.Operators ((^.), (^..))
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord ()
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import Text.RawString.QQ (r)

import Cardano.Api ()
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam), TargetAddresses, TxOut, pattern CurrentEra)
import Marconi.Core.Experiment qualified as Core

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
  compare (Utxo addr txin _ _ _ _ _) (Utxo addr' txin' _ _ _ _ _) =
     compare (addr, txin) (addr', txin')

newtype Spent = Spent { unSpent ::  C.TxIn } deriving (Show, Eq)

instance Ord Spent where
    compare (Spent (C.TxIn txid txix)) (Spent (C.TxIn txid' txix')) =
      compare (txid, txix) (txid', txix')

data UtxoEvent = UtxoEvent
    { _ueUtxos  :: !(Set Utxo)
    , _ueInputs :: !(Set Spent)
    } deriving (Eq, Ord, Show, Generic)

$(makeLenses ''UtxoEvent)

type instance Core.Point UtxoEvent = C.ChainPoint
type instance Core.Point Utxo = C.ChainPoint
type instance Core.Point Spent = C.ChainPoint
instance Core.HasGenesis C.ChainPoint where
  genesis = C.ChainPointAtGenesis

newtype QueryUtxoByAddress = QueryUtxoByAddress (C.AddressAny, Maybe C.SlotNo)

type instance Core.Result QueryUtxoByAddress = [Core.TimedEvent Utxo] -- use this instead of the utxorow

instance MonadIO m => Core.ResumableResult m UtxoEvent QueryUtxoByAddress Core.ListIndexer where
  resumeResult
    :: Ord (Core.Point UtxoEvent)
    => Core.Point UtxoEvent
    -> QueryUtxoByAddress
    -> Core.ListIndexer UtxoEvent
    -> m (Core.Result QueryUtxoByAddress)
    -> m (Core.Result QueryUtxoByAddress)
  resumeResult C.ChainPointAtGenesis _ _ _ = pure []
  resumeResult cp@(C.ChainPoint _ _) q events onDiskResult =
    let
      utxoEvents :: Core.ListIndexer UtxoEvent -> [UtxoEvent]
      utxoEvents lx = lx ^. Core.events ^.. folded . Core.event

      txInsFromEvent :: UtxoEvent -> Set C.TxIn
      txInsFromEvent = Set.map unSpent . _ueInputs

      txins :: Core.ListIndexer UtxoEvent -> Set C.TxIn
      txins = foldl' (\a c -> txInsFromEvent c  `Set.union` a) Set.empty . utxoEvents

      reduceUtxos :: Core.Result QueryUtxoByAddress -> Core.Result QueryUtxoByAddress
      reduceUtxos =
        filter (\(Core.TimedEvent _ utxo) -> utxo ^. utxoTxIn `notElem` txins events)
    in
      do
        mem :: (Core.Result QueryUtxoByAddress) <- Core.query cp q events
        disk <- onDiskResult
        pure $ reduceUtxos (disk <> mem)

  {- merging the ondisk and in-memory event. -}

-- TODO filter the in memmory for the address, then merge with onDisk

-- take a look at EvetnAtQuery line 1022, implemenation
-- used in the resume result,
instance MonadIO m => Core.Queryable m UtxoEvent QueryUtxoByAddress Core.ListIndexer  where
  query
    :: Ord (Core.Point UtxoEvent)
    =>  Core.Point UtxoEvent -- ^ give me what you know up to this point, potentially a point in future
    -> QueryUtxoByAddress
    -> Core.ListIndexer UtxoEvent -- ^ get the point for ListIndexer
    -> m (Core.Result QueryUtxoByAddress)
  query C.ChainPointAtGenesis _ _ = pure []
  query _ (QueryUtxoByAddress (addr, maybeSno)) listXer = pure timedEventsAtAddress
  -- we're ignoring future spent query for this version
    -- pure $ fmap (Core.TimedEvent pnt) eventsAtAddress
    where
      utxoEvents :: [UtxoEvent]
      utxoEvents = filter (pointFilter maybeSno) (listXer ^. Core.events) ^.. folded . Core.event
      pnt :: Core.Point UtxoEvent
      pnt = listXer ^. Core.latest


      pointFilter :: Maybe C.SlotNo -> Core.TimedEvent UtxoEvent -> Bool
      pointFilter ms = maybe (const True) (\s -> isBeforeSlot s . view Core.point )  ms

      isBeforeSlot :: C.SlotNo -> C.ChainPoint -> Bool
      isBeforeSlot s = \case
            C.ChainPointAtGenesis -> True
            C.ChainPoint s' _     -> s' <= s

      addressFilter :: Utxo -> Bool
      addressFilter u = u ^. utxoAddress == addr

      timedEventAtAddress :: [Core.TimedEvent UtxoEvent]
      timedEventAtAddress =
        let
            utxosAtAddress :: UtxoEvent -> UtxoEvent
            utxosAtAddress (UtxoEvent outs ins)  = UtxoEvent (Set.filter addressFilter outs) ins
            eventsAtAddress :: [UtxoEvent]
            eventsAtAddress = fmap utxosAtAddress utxoEvents

        in fmap (Core.TimedEvent pnt)  eventsAtAddress

      timedEventsAtAddress :: [Core.TimedEvent Utxo]
      timedEventsAtAddress = concatMap timedUtxosFromTimedUtxoEvent timedEventAtAddress

timedUtxosFromTimedUtxoEvent :: Core.TimedEvent UtxoEvent -> [Core.TimedEvent Utxo]
timedUtxosFromTimedUtxoEvent timedEvent =
  let
    getevent :: UtxoEvent -> [Utxo]
    getevent = Set.toList . _ueUtxos
  in
    fmap (Core.TimedEvent (timedEvent ^. Core.point)) (getevent (timedEvent ^. Core.event))

timedSpentsFromTimedUtxoEvent :: Core.TimedEvent UtxoEvent -> [Core.TimedEvent Spent]
timedSpentsFromTimedUtxoEvent timedEvent =
  let
    getevent :: UtxoEvent -> [Spent]
    getevent = Set.toList . _ueInputs
  in
    fmap (Core.TimedEvent (timedEvent ^. Core.point)) (getevent (timedEvent ^. Core.event))

type instance Core.InsertRecord UtxoEvent =
  ([Core.TimedEvent Utxo], [Core.TimedEvent Spent])

sqliteIndexer
  :: SQL.Connection
  -> Core.SQLiteIndexer UtxoEvent
sqliteIndexer conn =
  let
    utxoInsertQuery:: SQL.Query
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

    spentInsertQuery :: SQL.Query
    spentInsertQuery =
            [r|INSERT
              INTO spent (
                txId,
                txIx, slotNo, blockHash
              ) VALUES
              (?, ?, ?, ?)|]
    prepareInsert' :: Core.TimedEvent UtxoEvent -> ([Core.TimedEvent Utxo], [Core.TimedEvent Spent])
    prepareInsert' utxoEv =
      let
        tUtxos = timedUtxosFromTimedUtxoEvent utxoEv
        tSpents = timedSpentsFromTimedUtxoEvent utxoEv
      in (tUtxos, tSpents)

    buildInsert' :: Core.InsertRecord UtxoEvent -> [Core.IndexQuery]
    buildInsert' (u, s) = [ Core.IndexQuery utxoInsertQuery u
                          , Core.IndexQuery spentInsertQuery s]
  in
    Core.SQLiteIndexer
      conn
      prepareInsert'
      buildInsert'
      Core.genesis

-- | combine events in the context of time
instance Semigroup UtxoEvent where
  (UtxoEvent us is) <> (UtxoEvent us' is') =
    UtxoEvent utxos spents
    where
      getTxIn :: Utxo -> C.TxIn
      getTxIn u = u ^. utxoTxIn
      txins :: Set C.TxIn = Set.union (Set.map unSpent is) (Set.map unSpent is')
      utxos
        = foldl' (\a c -> if getTxIn c `Set.notMember` txins then Set.insert c a; else a) Set.empty
        $ Set.union us us'
      spents = Set.map Spent txins

instance Monoid UtxoEvent where
  mempty = UtxoEvent Set.empty Set.empty


-- | Create tables and indexes
--
initSQLite :: FilePath -> IO SQL.Connection
initSQLite dbPath = do
  c <- SQL.open dbPath

  SQL.execute_ c "PRAGMA journal_mode=WAL"

  SQL.execute_ c [r|CREATE TABLE IF NOT EXISTS unspent_transactions
                      ( address TEXT NOT NULL
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

mkMixedIndexer
  :: SQL.Connection
  -> SecurityParam
  -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer UtxoEvent
mkMixedIndexer conn (SecurityParam w64) =
  let
    flushsize::Word = fromIntegral w64
    keepinmem::Word = flushsize `div` 6  -- TODO need to discuss and replace
  in
    mkMixedIndexer' conn keepinmem flushsize

mkMixedIndexer'
  :: SQL.Connection
  -> Word -- ^  events keept in memory post flush
  -> Word -- ^ flush size
  -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer UtxoEvent
mkMixedIndexer' conn keep flush =
    Core.mixedIndexer keep flush (sqliteIndexer conn) Core.listIndexer

-----------------------------------------------------------------------
-- SQL table mappings
-----------------------------------------------------------------------
instance ToRow (Core.TimedEvent Utxo) where
  toRow u =
    let
      (C.TxIn txid txix) = u ^. Core.event . utxoTxIn
      (snoField, bhhField) = case u ^. Core.point of
        C.ChainPointAtGenesis  -> (SQL.SQLNull,SQL.SQLNull)
        (C.ChainPoint sno bhh) -> (toField sno, toField bhh)
    in
    toRow
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
    = Core.TimedEvent
    <$> ( C.ChainPoint <$> field <*> field)
    <*> ( Utxo
        <$> field
        <*> (C.TxIn <$> field <*> field)
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field )

instance ToRow (Core.TimedEvent Spent) where
  toRow s =
    let
      (C.TxIn txid txix) = unSpent . view Core.event $ s
      (snoField, bhhField) = case s ^. Core.point of
        C.ChainPointAtGenesis  -> (SQL.SQLNull,SQL.SQLNull)
        (C.ChainPoint sno bhh) -> (toField sno, toField bhh)
    in
          toRow
            ( toField txid
            , toField txix
            , snoField
            , bhhField)

instance FromRow (Core.TimedEvent Spent) where
  fromRow
    = Core.TimedEvent
    <$> ( C.ChainPoint <$> field <*> field)
    <*> ( Spent
        <$> (C.TxIn <$> field <*> field))

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

getTxOutFromTxBodyContent :: C.TxBodyContent build era -> [C.TxOut C.CtxTx era]
getTxOutFromTxBodyContent C.TxBodyContent {C.txOuts, C.txReturnCollateral, C.txScriptValidity} = case txScriptValidityToScriptValidity txScriptValidity of
  C.ScriptValid   -> txOuts -- When transaction is valid, only transaction fee is collected
  C.ScriptInvalid -> collateral txReturnCollateral -- failed Tx, we collect from collateral and return excess collateral
  where
    collateral C.TxReturnCollateralNone       = []
    collateral (C.TxReturnCollateral _ txout) = [txout]

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
      in
        case getUtxoFromTxOut maybeTargetAddresses txin txout of
          Nothing   -> Map.empty
          Just utxo -> Map.singleton txin utxo

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
--
getRefScriptAndHash
  :: C.ReferenceScript era
  -> (Maybe C.ScriptInAnyLang, Maybe C.ScriptHash)
getRefScriptAndHash refScript = case refScript of
  C.ReferenceScriptNone -> (Nothing, Nothing)
  C.ReferenceScript _ s@(C.ScriptInAnyLang(C.SimpleScriptLanguage C.SimpleScriptV1) script) ->
      ( Just  s
      , Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.SimpleScriptLanguage C.SimpleScriptV2) script)->
    ( Just s
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
getScriptDataAndHash (C.TxOutDatumInTx _ d)   = (Just d, (Just . C.hashScriptData) d)
getScriptDataAndHash (C.TxOutDatumInline _ d) = (Just d, (Just . C.hashScriptData) d)

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
  in
    Set.fromList inputs


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
