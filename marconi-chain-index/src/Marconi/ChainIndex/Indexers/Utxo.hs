{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Module for indexing the Utxos in the Cardano blockchain

-- + This module will create the SQL tables:
--
-- + table: transactions_output
--
-- @
--      |---------+------+-------+-----------+-------+-------+------------------+--------------+---|
--      | Address | TxId | TxIx  | DatumHash | Datum | Value | InlineScriptHash | InlineScript |...|
--      |---------+------+-------+-----------+-------+-------+------------------+--------------|---|
--
--      |---+------+-----------+----------------+-----------+------------+-----------|
--      |...| Slot | BlockHash | TxIndexInBlock | SpentSlot | SpentBlock | SpentTxId |
--      |---+------+-----------+----------------+-----------+------------|-----------|
-- @
--
-- Design choice: the single table approach is based on two rationales:
--
--    * In a scenario where we only follow a given set of addresses,
--      having a separate table for spent utxo requires to either to resolve the TxIn
--      to decide whether or not a spent must be stored before resolving it,
--      or to store TxIn that are not needed.
--    * On query, a single table save us a join.
--
-- To create this table, we extract all transactions inputs and outputs
-- from each transactions fetched with the chain-sync protocol of the local node.

module Marconi.ChainIndex.Indexers.Utxo where

import Control.Exception (bracket_)
import Control.Lens (Lens', Traversal', _Just, imap, makeLenses, (.~), (^.), (^?))
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.:?), (.=))
import Data.Either (fromRight)
import Data.Foldable (fold, foldl', toList)
import Data.List (groupBy, sort, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text qualified as Text
import Data.Word (Word64)
import Database.SQLite.Simple (NamedParam ((:=)), ResultError (UnexpectedNull))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (returnError)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field, fieldWith)
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import System.Random.MWC (createSystemRandom, uniformR)
import Text.RawString.QQ (r)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Function ((&))
import Data.Ord (Down (Down, getDown))
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (TargetAddresses, TxIndexInBlock, TxOut, pattern CurrentEra)
import Marconi.ChainIndex.Utils (chainPointOrGenesis)

import Marconi.ChainIndex.Error (IndexerError (CantInsertEvent, CantQueryIndexer, CantRollback, CantStartIndexer, InvalidQueryInterval),
                                 liftSQLError)
import Marconi.Core.Storable (Buffered (getStoredEvents, persistToStorage), HasPoint, Queryable (queryStorage),
                              Resumable (resumeFromStorage), Rewindable (rewindStorage), StorableEvent, StorableMonad,
                              StorablePoint, StorableQuery, StorableResult, emptyState)
import Marconi.Core.Storable qualified as Storable

{- Note [Last sync chainpoint]
 -
 - The 'LastSyncPoint' query doesn't return the last indexed chainpoint, but the one before.
 - The reason is that we want to use this query to find a sync point that is common to all the indexers
 - that are under the same coordinator.
 - Unfortunately, while the coordinator ensures that all the indexer move at the same speed, it can't
 - monitor if the last submitted block was indexed by all the indexers or not.
 -
 - As a consequence, if the last chainpoint of the utxo indexer can, at most, be ahead of one block compared to other
 - indexers. Taking the chainpoint before ensure that we have consistent information across all the indexers.
 -}

-- | Not comprehensive, only supports ChainPoint interval as outlines in
--   <https://github.com/input-output-hk/marconi/blob/main/marconi-sidechain/doc/API.adoc#getutxosfromaddress>
data Interval r
  = LessThanOrEqual !r
  | InRange !r !r
  deriving (Eq, Show)

lowerBound :: Interval r -> Maybe r
lowerBound = \case
    LessThanOrEqual _ -> Nothing
    InRange x _       -> Just x

upperBound :: Interval r -> Maybe r
upperBound = \case
    LessThanOrEqual x -> Just x
    InRange _ x       -> Just x


interval
  :: (Ord r, Show r)
  => Maybe r -- ^ lower bound
  -> r  -- ^ upper bound
  -> Either IndexerError (Interval r)
interval Nothing p = Right $ LessThanOrEqual p
interval (Just p) p' =
  let
--  Enforce the internal invariant
-- 'InRange'.
    wrap
      :: (Ord r, Show r)
      => (r -> r -> Interval r)
      -> r -> r -> Either IndexerError (Interval r)
    wrap f x y
      | x <= y = Right $ f x y
      | otherwise = Left . InvalidQueryInterval . pack
          $ "Invalid Interval. LowerBound, "
          <> show x
          <> " is not less than or equal to upperBound "
          <> show y

  in wrap InRange p p'

isInInterval :: Interval C.SlotNo -> C.ChainPoint -> Bool
isInInterval slotNoInterval = \case
    C.ChainPointAtGenesis -> case slotNoInterval of
        LessThanOrEqual _ -> True
        InRange _ _       -> False

    C.ChainPoint slotNo _  -> let
        lowerBoundCheck = maybe (const True) (<=) $ lowerBound slotNoInterval
        upperBoundCheck = maybe (const False) (>=) $ upperBound slotNoInterval
        in upperBoundCheck slotNo && lowerBoundCheck slotNo

type UtxoIndexer = Storable.State UtxoHandle

data UtxoHandle = UtxoHandle
  { hdlConnection :: !SQL.Connection  -- ^ SQLite connection
  , hdlDpeth      :: !Int             -- ^ depth before flushing to disk storage
  , toVacuume     :: !Bool            -- ^ weather to perform SQLite vacuum to release space
  }

data QueryUtxoByAddress = QueryUtxoByAddress !C.AddressAny !(Interval C.SlotNo)
  deriving (Show, Eq)

data instance StorableQuery UtxoHandle
    = QueryUtxoByAddressWrapper QueryUtxoByAddress
    | LastSyncPoint
    deriving (Show, Eq)

type QueryableAddresses = NonEmpty (StorableQuery UtxoHandle)

type instance StorableMonad UtxoHandle = ExceptT IndexerError IO

type instance StorablePoint UtxoHandle = C.ChainPoint

newtype Depth = Depth Int

data Utxo = Utxo
  { _address          :: !C.AddressAny
  , _txIn             :: !C.TxIn
  , _datum            :: !(Maybe C.ScriptData)
  , _datumHash        :: !(Maybe (C.Hash C.ScriptData))
  , _value            :: !C.Value
  , _inlineScript     :: !(Maybe C.ScriptInAnyLang)
  , _inlineScriptHash :: !(Maybe C.ScriptHash)
  , _txIndexInBlock   :: !TxIndexInBlock
  } deriving (Show, Eq, Generic)

$(makeLenses ''Utxo)

instance Ord Utxo where
  compare u u' = compare (u ^. address, u ^. txIn) (u' ^. address, u' ^. txIn)

instance FromJSON Utxo where
    parseJSON (Object v) =
        Utxo
            <$> v .: "address"
            <*> (C.TxIn <$> v .: "txId" <*> v .: "txIx")
            <*> v .: "datum"
            <*> v .: "datumHash"
            <*> v .: "value"
            <*> v .: "inlineScript"
            <*> v .: "inlineScriptHash"
            <*> v .: "txIndexInBlock"
    parseJSON _ = mempty

instance ToJSON Utxo where
  toJSON u = let
        C.TxIn txid txix = u ^. txIn
        in object
            [ "address"           .= (u ^. address)
            , "txId"              .= txid
            , "txIx"              .= txix
            , "txIndexInBlock"    .= (u ^. txIndexInBlock)
            , "datum"             .= (u ^. datum)
            , "datumHash"         .= (u ^. datumHash)
            , "value"             .= (u ^. value)
            -- Uses ToJSON instance of cardano-api which serialises using the 'C.HasTextEnvelope' typeclass.
            , "inlineScript"      .= (u ^. inlineScript)
            , "inlineScriptHash"  .= (u ^. inlineScriptHash)
            ]

instance FromRow Utxo where
  fromRow = Utxo
      <$> field
      <*> fromRow
      <*> field <*> field <*> field <*> field <*> field <*> field

data ChainPointRow
    = ChainPointRow { _cpSlotNo :: C.SlotNo, _cpBlockHash :: C.Hash C.BlockHeader }
    deriving (Show, Eq, Ord, Generic)

$(makeLenses ''ChainPointRow)

toChainPointRow :: C.ChainPoint -> Maybe ChainPointRow
toChainPointRow cp = ChainPointRow <$> C.chainPointToSlotNo cp <*> C.chainPointToHeaderHash cp

instance FromRow ChainPointRow where
  fromRow = ChainPointRow <$> field <*> field

getChainPoint :: ChainPointRow -> C.ChainPoint
getChainPoint cp = C.ChainPoint (cp ^. cpSlotNo) (cp ^. cpBlockHash)

data SpentInfo
    = SpentInfo
    { _spSpentPoint :: ChainPointRow
    , _spSpentTxId  :: C.TxId
    } deriving (Show, Eq, Ord, Generic)

$(makeLenses ''SpentInfo)

data UtxoRow = UtxoRow
  { _urUtxo          :: !Utxo
  , _urCreationPoint :: !ChainPointRow
  , _urSpentInfo     :: !(Maybe SpentInfo)
  } deriving (Show, Eq, Ord, Generic)


$(makeLenses ''UtxoRow)

urCreationSlotNo :: Lens' UtxoRow C.SlotNo
urCreationSlotNo = urCreationPoint . cpSlotNo

urCreationBlockHash :: Lens' UtxoRow (C.Hash C.BlockHeader)
urCreationBlockHash = urCreationPoint . cpBlockHash

urSpentSlotNo :: Traversal' UtxoRow C.SlotNo
urSpentSlotNo = urSpentInfo . _Just . spSpentPoint . cpSlotNo

urSpentBlockHash :: Traversal' UtxoRow (C.Hash C.BlockHeader)
urSpentBlockHash = urSpentInfo . _Just . spSpentPoint . cpBlockHash

urSpentTxId :: Traversal' UtxoRow C.TxId
urSpentTxId = urSpentInfo . _Just . spSpentTxId

instance FromJSON UtxoRow where
    parseJSON (Object v) = let

        parseSpentInfo = do
            s <- v .:? "spentSlotNo"
            bh <- v .:? "spentBlockHeaderHash"
            tId <- v .:? "spentTxId"
            pure $ case (s, bh, tId) of
                (Nothing, Nothing, Nothing)     -> Nothing
                (Just s', Just bh', Just txId') -> Just $ SpentInfo (ChainPointRow s' bh') txId'
                _error                          -> fail "Inconsistent spent info"

        in UtxoRow
            <$> v .: "utxo"
            <*> (ChainPointRow <$> v .: "slotNo" <*> v .: "blockHeaderHash")
            <*> parseSpentInfo
    parseJSON _ = mempty

instance ToJSON UtxoRow where
  toJSON ur = object
    [ "utxo" .= (ur ^. urUtxo)
    , "slotNo" .= (ur ^. urCreationPoint . cpSlotNo)
    , "blockHeaderHash" .= (ur ^. urCreationPoint . cpBlockHash)
    , "spentSlotNo" .= (ur ^? urSpentSlotNo)
    , "spentBlockHeaderHash" .= (ur ^? urSpentBlockHash)
    , "spentTxId" .= (ur ^? urSpentTxId)
    ]

instance FromJSON ChainPointRow where
    parseJSON (Object v)
        = ChainPointRow
            <$> v .: "slotNo"
            <*> v .: "blockHeaderHash"
    parseJSON _ = mempty

instance ToJSON ChainPointRow where
    toJSON (ChainPointRow s h)
        = object
        [ "slotNo" .= s
        , "blockHeaderHash" .= h
        ]

data instance StorableResult UtxoHandle
    = UtxoResult { getUtxoResult :: ![UtxoRow] }
    | LastSyncPointResult { getLastSyncPoint :: !C.ChainPoint }
    deriving (Eq, Show, Ord)

data Spent = Spent
    { _sTxIn       :: !C.TxIn
    , _sChainPoint :: !ChainPointRow
    , _sSpentTxId  :: !C.TxId
    } deriving (Show, Eq)

$(makeLenses ''Spent)

instance ToRow Spent where
    toRow s = let
        C.TxIn txid txix = s ^. sTxIn
        in [ toField $ s ^. sChainPoint . cpSlotNo
           , toField $ s ^. sChainPoint . cpBlockHash
           , toField $ s ^. sSpentTxId
           , toField txid
           , toField txix
           ]

instance Ord Spent where
    compare s s' = compare (s ^. sTxIn) (s' ^. sTxIn)

data instance StorableEvent UtxoHandle
    = UtxoEvent
    { ueUtxos       :: !(Set Utxo)
    , ueInputs      :: !(Map C.TxIn C.TxId)
    , ueChainPoint  :: !C.ChainPoint
    } deriving (Eq, Ord, Show, Generic)

-- | mappend, combine and balance Utxos
instance Semigroup (StorableEvent UtxoHandle) where
  (UtxoEvent us is cp) <> (UtxoEvent us' is' cp') = let

      txins = Map.union is is'

      insertUnspent :: Set Utxo -> Utxo -> Set Utxo
      insertUnspent acc u = if (u ^. txIn) `Map.notMember` txins
          then Set.insert u acc
          else acc

      utxos
        = foldl' insertUnspent Set.empty
        $ Set.union us us'

    in UtxoEvent utxos txins (max cp cp')

instance Monoid (StorableEvent UtxoHandle) where
  mempty = UtxoEvent mempty mempty C.ChainPointAtGenesis

-- | The effect of a transaction (or a number of them) on the tx output map.
data TxOutBalance =
  TxOutBalance
    { _tbUnspent :: !(Map C.TxIn Utxo)
    -- ^ Outputs newly added by the transaction(s)
    , _tbSpent   :: !(Map C.TxIn C.TxId)
    -- ^ Outputs spent by the transaction(s)
    }
    deriving stock (Eq, Show, Generic)

makeLenses ''TxOutBalance

instance Semigroup TxOutBalance where
    bUtxoL <> bUtxoR =
      let
        bUnspentKeys :: Set C.TxIn
        bUnspentKeys
            = Map.keysSet
            $ (bUtxoR ^. tbUnspent)
            <> ((bUtxoL ^. tbUnspent) `Map.difference` (bUtxoR ^. tbSpent))
        utxoMap :: Map C.TxIn Utxo
        utxoMap = _tbUnspent bUtxoL `Map.union` _tbUnspent bUtxoR
        bSpent
          =  bUtxoL ^. tbSpent
          <> ((bUtxoR ^. tbSpent) `Map.difference` (bUtxoL ^. tbUnspent))
      in
        TxOutBalance
            { _tbUnspent = Map.restrictKeys utxoMap bUnspentKeys
            , _tbSpent = bSpent
            }

instance Monoid TxOutBalance where
    mappend = (<>)
    mempty = TxOutBalance mempty mempty

instance HasPoint (StorableEvent UtxoHandle) C.ChainPoint where
  getPoint (UtxoEvent _ _ cp) = cp

------------------
-- sql mappings --
------------------

instance ToRow UtxoRow where
  toRow u =
      let C.TxIn txId txIx = u ^. urUtxo . txIn
      in toRow
          [ toField (u ^. urUtxo . address)
          , toField txId
          , toField txIx
          , toField (u ^. urUtxo . datum)
          , toField (u ^. urUtxo . datumHash)
          , toField (u ^. urUtxo . value)
          , toField (u ^. urUtxo . inlineScript)
          , toField (u ^. urUtxo . inlineScriptHash)
          , toField (u ^. urCreationPoint . cpSlotNo)
          , toField (u ^. urCreationPoint . cpBlockHash)
          , toField (u ^? urSpentSlotNo)
          , toField (u ^? urSpentBlockHash)
          , toField (u ^? urSpentTxId)
          , toField (u ^. urUtxo . txIndexInBlock)
          ]

data SpentInfoRow
    = SpentInfoRow !(Maybe C.SlotNo) !(Maybe (C.Hash C.BlockHeader)) !(Maybe C.TxId)

instance FromRow SpentInfoRow where

    fromRow = SpentInfoRow <$> field <*> field <*> field

instance FromRow UtxoRow where
  fromRow = let

      spentInfo Nothing Nothing Nothing       = pure Nothing
      spentInfo (Just s) (Just bh) (Just tid) = pure $ Just $ SpentInfo (ChainPointRow s bh) tid
      spentInfo _ _ _
          = fieldWith $ \field' -> returnError
              UnexpectedNull
              field'
              "Invalid spent values: Some fields are null, other aren't"

      in do
      utxo <- fromRow
      created <- fromRow
      (SpentInfoRow spentSlot spentBH spentTxId) <- fromRow
      info <- spentInfo spentSlot spentBH spentTxId
      pure $ UtxoRow utxo created info

-- | Open a connection to DB, and create resources
-- The parameter ((k + 1) * 2) specifies the amount of events that are buffered.
-- The larger the number, the more RAM the indexer uses. However, we get improved SQL
-- queries due to batching more events together.
open
  :: FilePath   -- ^ SQLite file path
  -> Depth      -- ^ The Depth parameter k, the larger K, the more RAM the indexer uses
  -> Bool       -- ^ whether to perform vacuum
  -> StorableMonad UtxoHandle UtxoIndexer
open dbPath (Depth k) isToVacuume = do
    c <- liftSQLError CantStartIndexer (SQL.open dbPath)

    lift $ SQL.execute_ c "PRAGMA journal_mode=WAL"
    lift $ SQL.execute_ c
        [r|CREATE TABLE IF NOT EXISTS transactions_outputs
           ( address TEXT NOT NULL
           , txId TEXT NOT NULL
           , txIx INT NOT NULL
           , datum BLOB
           , datumHash BLOB
           , value BLOB
           , inlineScript BLOB
           , inlineScriptHash BLOB
           , slotNo INT NOT NULL
           , blockHash BLOB NOT NULL
           , spentSlotNo INT
           , spentBlockHash BLOB
           , spentTxId TEXT
           , txIndexInBlock INT NOT NULL
           )|]

    lift $ SQL.execute_ c
        [r|CREATE INDEX IF NOT EXISTS transaction_address
           ON transactions_outputs (address)|]

    lift $ SQL.execute_ c
        [r|CREATE INDEX IF NOT EXISTS transaction_id
           ON transactions_outputs (txId, txIx)|]

    lift $ SQL.execute_ c
        [r|CREATE INDEX IF NOT EXISTS transaction_spent_slot
           ON transactions_outputs (spentSlotNo)|]

    emptyState k (UtxoHandle c k isToVacuume)

getSpentFrom :: StorableEvent UtxoHandle -> [Spent]
getSpentFrom (UtxoEvent _ txIns cp) = case toChainPointRow cp of
    Nothing  -> [] -- There are no Spent in the Genesis block
    Just cpr -> do
      (txin, spentTxId) <-  Map.toList txIns
      pure $ Spent txin cpr spentTxId

-- | Store UtxoEvents
-- Events are stored in memory and flushed to SQL, disk, when memory buffer has reached capacity
instance Buffered UtxoHandle where
  persistToStorage
    :: Foldable f
    => f (StorableEvent UtxoHandle) -- ^ events to store
    -> UtxoHandle -- ^ handler for storing events
    -> StorableMonad UtxoHandle UtxoHandle
  persistToStorage events h
    = liftSQLError CantInsertEvent $ do
    let rows = concatMap eventToRows events
        spents = concatMap getSpentFrom events
        c = hdlConnection h
    bracket_
        (SQL.execute_ c "BEGIN")
        (SQL.execute_ c "COMMIT")
        (do
         unless (null rows)
             $ SQL.executeMany c
                 [r|INSERT
                    INTO transactions_outputs (
                      address,
                      txId,
                      txIx,
                      datum,
                      datumHash,
                      value,
                      inlineScript,
                      inlineScriptHash,
                      slotNo,
                      blockHash,
                      spentSlotNo,
                      spentBlockHash,
                      spentTxId,
                      txIndexInBlock
                    ) VALUES
                    (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|] rows
         unless (null spents)
             $ SQL.executeMany c
                   [r|UPDATE transactions_outputs
                      SET spentSlotNo = ?,
                          spentBlockHash = ?,
                          spentTxId = ?
                      WHERE txId = ? AND txIx = ?|] spents
         )
    -- We want to perform vacuum about once every 100
    when (toVacuume h) $ do
      rndCheck <- createSystemRandom >>= uniformR (1 :: Int, 100)
      when (rndCheck == 42) $ do
        SQL.execute_ c [r|DELETE FROM transactions_outputs
                          WHERE transactions_outputs.spentSlotNo IS NOT NULL|]
        -- remove Spent and release space, see https://www.sqlite.org/lang_vacuum.html
        SQL.execute_ c "VACUUM"
    pure h

  getStoredEvents :: UtxoHandle -> StorableMonad UtxoHandle [StorableEvent UtxoHandle]
  getStoredEvents (UtxoHandle c sz _) = liftSQLError CantQueryIndexer $ do
    sns <- SQL.query c
        [r|SELECT slotNo
           FROM transactions_outputs
           GROUP BY slotNo
           ORDER BY slotNo DESC
           LIMIT ?|] (SQL.Only sz) :: IO [[Word64]]

    -- Take the slot number of the sz'th slot
    let sn = if null sns
                then 0
                else head . last $ take sz sns

    rows :: [UtxoRow] <- SQL.query c
        [r|SELECT
              u.address,
              u.txId,
              u.txIx,
              u.datum,
              u.datumHash,
              u.value,
              u.inlineScript,
              u.inlineScriptHash,
              u.txIndexInBlock,
              u.slotNo,
              u.blockHash,
              u.spentSlotNo,
              u.spentBlockHash,
              u.spentTxId
           FROM
              transactions_outputs u
           WHERE
              u.slotNo >= ?
           GROUP by
              u.slotNo
           ORDER BY
              u.slotNo ASC|] (SQL.Only (sn :: Word64))

    rowsToEvents (getTxIns c) rows

-- | Retrieve TxIns at a slotNo
-- This function is used to reconstruct the original UtxoEvent
getTxIns :: SQL.Connection -> C.SlotNo -> IO (Map C.TxIn C.TxId)
getTxIns c sn = do
    xs <- SQL.query c
        "SELECT txId, txIx, spentTxId FROM transactions_outputs WHERE spentSlotNo =?"
        (SQL.Only (Just sn))
    pure $ Map.fromList $ do
        (txid, txix, spentTx) <- xs
        pure (C.TxIn txix txid, spentTx)

-- | Convert UtxoRows to UtxoEvents
rowsToEvents
  :: (C.SlotNo -> IO (Map C.TxIn C.TxId))  -- ^ function to fetch TxIn
  -> [UtxoRow]                      -- ^ rows to convert back to event
  -> IO [StorableEvent UtxoHandle]  -- ^ utxo events
rowsToEvents _ [] = pure []
rowsToEvents  fetchTxIn rows
  =  sortOn ueChainPoint <$> traverse reduce eventsMap
  where
    mkEvent :: UtxoRow -> StorableEvent UtxoHandle
    mkEvent row = UtxoEvent
       (Set.singleton $ row ^. urUtxo)
       Map.empty
       (getChainPoint (row ^. urCreationPoint))

    newEventWithSpentOnly :: Map C.TxIn C.TxId -> C.ChainPoint -> StorableEvent UtxoHandle
    newEventWithSpentOnly = UtxoEvent Set.empty

    reduce :: (C.ChainPoint, [StorableEvent UtxoHandle]) -> IO (StorableEvent UtxoHandle)
    reduce ( C.ChainPointAtGenesis, _) = pure $ UtxoEvent Set.empty Map.empty C.ChainPointAtGenesis
    reduce (cp@(C.ChainPoint sn _), es) = do
                     tins <- fetchTxIn sn
                     let newE = newEventWithSpentOnly tins cp
                     pure. fold $ newE:es

    eventsMap :: [(C.ChainPoint, [StorableEvent UtxoHandle])]
    eventsMap
            = fmap  (\x -> (ueChainPoint . head $ x, x) )
            . groupBy (\el er -> ueChainPoint el == ueChainPoint er)
            . fmap mkEvent
            $ rows

-- | convert utxoEvent to utxoRow
-- Note: No `unspent` computation is performed
eventToRows :: StorableEvent UtxoHandle -> [UtxoRow]
eventToRows (UtxoEvent _ _ C.ChainPointAtGenesis) = []  -- we don't save anyting at genesis.
eventToRows (UtxoEvent utxos _ (C.ChainPoint sn bhsh)) =
  fmap (\u -> UtxoRow
           { _urUtxo = u
           , _urCreationPoint = ChainPointRow sn bhsh
           , _urSpentInfo = Nothing
           }
       ) . Set.toList $ utxos


-- | Used to gather the information required
-- to update the in-database result
-- from the UtoxByAddress query
-- with the in-memory events
data UtxoByAddressBufferEvents
    = UtxoByAddressBufferEvents
    { _bufferUtxos       :: ![StorableEvent UtxoHandle]
    -- ^ Utxos at the requested address
    , _bufferSpent       :: !(Set C.TxIn)
    -- ^ All the spent TxIn stored in memory that occured before the query upper bound
    , _bufferFutureSpent :: !(Map C.TxIn SpentInfo)
    -- ^ All the spent TxIn stored in memory that occured after the query upper bound
    } deriving (Eq, Show)

makeLenses ''UtxoByAddressBufferEvents

instance Semigroup UtxoByAddressBufferEvents where
    u1 <> u2 =
        UtxoByAddressBufferEvents
            (u1 ^. bufferUtxos <> u2 ^. bufferUtxos)
            (u1 ^. bufferSpent <> u2 ^. bufferSpent)
            (u1 ^. bufferFutureSpent <> u2 ^. bufferFutureSpent)

instance Monoid UtxoByAddressBufferEvents where
    mempty = UtxoByAddressBufferEvents mempty mempty mempty

-- | merge in-memory events with SQL retrieved UtxoRows
-- Notes, a property of this merge is to remove all spent utxos from the resulting [UtxoRow]
mergeInMemoryAndSql
  :: UtxoByAddressBufferEvents
  -> [UtxoRow]
  -> [UtxoRow]
mergeInMemoryAndSql bufferEvents = let

    asTxIn :: UtxoRow -> C.TxIn
    asTxIn u = u ^. urUtxo . txIn

    excludeBufferSpent :: [UtxoRow] -> [UtxoRow]
    excludeBufferSpent = filter (flip Set.notMember (bufferEvents ^. bufferSpent) . asTxIn)

    updateFutureSpent :: UtxoRow -> UtxoRow
    updateFutureSpent u = let

        spent = Map.lookup (asTxIn u) $ bufferEvents ^. bufferFutureSpent

        in case u ^. urSpentInfo of
             -- if it's already spent, no need to check if it's spent in the buffer events
             Just _  -> u
             Nothing -> u & urSpentInfo .~ spent

    appendBufferUtxos :: [UtxoRow] -> [UtxoRow]
    appendBufferUtxos = (<> (bufferEvents ^. bufferUtxos >>= eventToRows))

    in fmap updateFutureSpent
        . excludeBufferSpent
        . appendBufferUtxos


-- | Filter for events at the given address
eventsAtAddress
  :: Foldable f
  => QueryUtxoByAddress -- ^ Address SlotInterval query
  -> f (StorableEvent UtxoHandle) -- ^ Utxo event
  -> UtxoByAddressBufferEvents
eventsAtAddress (QueryUtxoByAddress addr snoInterval) =
  let

    pointFilter :: StorableEvent UtxoHandle -> Bool
    pointFilter = isInInterval snoInterval . ueChainPoint

    afterBoundCheck :: Maybe C.SlotNo -> Bool
    afterBoundCheck = case upperBound snoInterval of
        Nothing -> const False
        Just s  ->  maybe False (s <)

    afterUpperBound :: StorableEvent UtxoHandle -> Bool
    afterUpperBound
        = afterBoundCheck
        . C.chainPointToSlotNo . ueChainPoint

    addressFilter :: Utxo -> Bool
    addressFilter u = (u ^. address) == addr

    utxosAtAddress :: StorableEvent UtxoHandle -> Set Utxo
    utxosAtAddress = Set.filter addressFilter . ueUtxos

    splitEventAtAddress :: StorableEvent UtxoHandle -> [StorableEvent UtxoHandle]
    splitEventAtAddress event =
      [event {ueUtxos = utxosAtAddress event}
      | not (null $ utxosAtAddress event)
        && pointFilter event]

    generateSpentInfo :: StorableEvent UtxoHandle -> C.TxId -> Maybe SpentInfo
    generateSpentInfo event txid = flip SpentInfo txid
        <$> toChainPointRow (ueChainPoint event)

    getBufferSpent :: StorableEvent UtxoHandle -> Set C.TxIn
    getBufferSpent event = if afterUpperBound event
        then mempty
        else Map.keysSet $ ueInputs event

    getBufferFutureSpent :: StorableEvent UtxoHandle -> Map C.TxIn SpentInfo
    getBufferFutureSpent event = if afterUpperBound event
        then Map.mapMaybe (generateSpentInfo event) $ ueInputs event
        else mempty

    go :: StorableEvent UtxoHandle -> UtxoByAddressBufferEvents
    go event =
        UtxoByAddressBufferEvents
            (splitEventAtAddress event)
            (getBufferSpent event)
            (getBufferFutureSpent event)

  in foldMap go

utxoAtAddressQuery
    :: SQL.Connection
    -> UtxoByAddressBufferEvents
    -> [SQL.Query] -- ^ the filter part of the query
    -> [NamedParam]
    -> IO (StorableResult UtxoHandle)
utxoAtAddressQuery c bufferResult filters params
    = do
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
                  u.txIndexInBlock,
                  u.slotNo,
                  u.blockHash,
                  u.spentSlotNo,
                  u.spentBlockHash,
                  u.spentTxId
               FROM
                  transactions_outputs u
               WHERE |] <> SQL.Query (Text.intercalate " AND " $ SQL.fromQuery <$> filters) <>
            [r| ORDER BY u.slotNo ASC |]
    persistedUtxoRows :: [UtxoRow] <- SQL.queryNamed c builtQuery params
    pure
      $ UtxoResult
      $ mergeInMemoryAndSql
          bufferResult
          persistedUtxoRows

-- | Query the data stored in the indexer
-- Quries SQL + buffered data, where buffered data is the data that will be batched to SQL
instance Queryable UtxoHandle where
  queryStorage
    :: Foldable f
    => f (StorableEvent UtxoHandle)
    -> UtxoHandle
    -> StorableQuery UtxoHandle
    -> StorableMonad UtxoHandle (StorableResult UtxoHandle)

  queryStorage es (UtxoHandle c _ _) (QueryUtxoByAddressWrapper q@(QueryUtxoByAddress addr slotInterval)) =
    let
      eventAtQuery :: UtxoByAddressBufferEvents
      eventAtQuery = eventsAtAddress q es
      addressFilter = (["u.address = :address"], [":address" := addr])
      lowerBoundFilter = case lowerBound slotInterval of
          Nothing -> mempty
          Just x  -> (["u.slotNo >= :slotNoLow"] , [":slotNoLow" := x])
      upperBoundFilter = case upperBound slotInterval of
          Nothing -> mempty
          Just x  -> (["u.slotNo <= :slotNo", "(u.spentSlotNo IS NULL OR u.spentSlotNo > :slotNo)"] , [":slotNo" := x])
      filters = addressFilter <> lowerBoundFilter <> upperBoundFilter

    in liftSQLError CantQueryIndexer $ uncurry (utxoAtAddressQuery c eventAtQuery) filters

  queryStorage es (UtxoHandle c _ _) LastSyncPoint =
    let
      queryLastSlot = [r|SELECT u.slotNo, u.blockHash
                         FROM transactions_outputs u
                         GROUP BY u.slotNo
                         ORDER BY u.slotNo DESC
                         LIMIT ? |]
      -- We don't send the last event but the one before, to ensure that every indexers reached this point
      -- It's a hack, which should be removed once we have a proper handling of synchronization events.
      --
      -- See Note [Last sync chainpoint]
      in case toList es of
        -- 2+ elements in memory
        (_:_:_) -> pure . LastSyncPointResult $
            case fmap getDown $ sort $ Down . ueChainPoint <$> toList es of
                _:p:_xs -> p
                _other  -> C.ChainPointAtGenesis
        -- 1 element in memory
        (_:_) -> liftSQLError CantQueryIndexer $ do
                persisted <- SQL.query c queryLastSlot (SQL.Only (1 :: Word64))
                pure . LastSyncPointResult $ case persisted of
                    p:_    -> getChainPoint p
                    _other -> C.ChainPointAtGenesis
        -- 0 element in memory
        [] -> liftSQLError CantQueryIndexer $ do
                persisted <- SQL.query c queryLastSlot (SQL.Only (2 :: Word64))
                pure . LastSyncPointResult $ case persisted of
                    _:p:_xs -> getChainPoint p
                    _other  -> C.ChainPointAtGenesis

instance Rewindable UtxoHandle where
  rewindStorage :: C.ChainPoint -> UtxoHandle -> StorableMonad UtxoHandle UtxoHandle
  rewindStorage (C.ChainPoint sn _) h@(UtxoHandle c _ _) =  liftSQLError CantRollback $ do
    SQL.execute c "DELETE FROM transactions_outputs WHERE slotNo > ?" (SQL.Only sn)
    SQL.execute c
        [r| UPDATE transactions_outputs
            SET spentSlotNo = NULL,
                spentBlockHash = NULL,
                spentTxId = NULL
            WHERE (spentSlotNo IS NOT NULL AND spentSlotNo > ?) |] (SQL.Only sn)
    pure h
  rewindStorage C.ChainPointAtGenesis h@(UtxoHandle c _ _) = liftSQLError CantRollback $ do
    SQL.execute_ c "DELETE FROM transactions_outputs"
    pure h

-- For resuming we need to provide a list of points where we can resume from.
instance Resumable UtxoHandle where
  resumeFromStorage (UtxoHandle c _ _) = liftSQLError CantQueryIndexer $ fmap chainPointOrGenesis $
    SQL.query_ c "SELECT slotNo, blockHash FROM transactions_outputs ORDER BY slotNo DESC LIMIT 1"

-- Add pagination to resume
-- Main reason for adding this is to protect against OOM
-- TODO use withAsync to spread the load. as resume, as is implemented here, could take several minutes depending on the amount of data stored.
resumeHelper :: SQL.Connection -> IO [C.ChainPoint]
resumeHelper c =
  let
    limit :: Int = 216000
    helper
      :: Int -- ^ page
      -> [C.ChainPoint]
      -> IO [C.ChainPoint] -- ^ accumulated chainpoints
    helper page tally =  do
      let offset = page * limit
      cps <- fmap (uncurry C.ChainPoint) <$>
        SQL.query c [r|SELECT DISTINCT slotNo, blockHash
                   FROM transactions_outputs
                   ORDER BY slotNo DESC
                   LIMIT ? OFFSET ? |] (limit, offset)
      case cps of
        []   -> pure tally
        cps' -> helper (page + 1) (tally <> cps')
  in helper 0 []

-- | Convert from 'AddressInEra' of the 'CurrentEra' to 'AddressAny'.
toAddr :: C.AddressInEra era -> C.AddressAny
toAddr (C.AddressInEra C.ByronAddressInAnyEra addr)    = C.AddressByron addr
toAddr (C.AddressInEra (C.ShelleyAddressInEra _) addr) = C.AddressShelley addr

-- | Extract UtxoEvents from Cardano Block
getUtxoEventsFromBlock
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> C.Block era
  -> StorableEvent UtxoHandle -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEventsFromBlock maybeTargetAddresses (C.Block (C.BlockHeader slotNo hsh _) txs) =
  getUtxoEvents maybeTargetAddresses txs (C.ChainPoint slotNo hsh)

-- | Extract UtxoEvents from Cardano Transactions
getUtxoEvents
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> [C.Tx era]
  -> C.ChainPoint
  -> StorableEvent UtxoHandle -- ^ UtxoEvents are stored in storage after conversion to UtxoRow
getUtxoEvents maybeTargetAddresses txs cp =
  let (TxOutBalance utxos spentTxOuts) = foldMap (balanceUtxoFromTx maybeTargetAddresses) $ zip txs [0..]
      resolvedUtxos :: Set Utxo
      resolvedUtxos = Set.fromList $ Map.elems utxos
  in
    UtxoEvent resolvedUtxos spentTxOuts cp

-- | does the transaction contain a targetAddress
isAddressInTarget :: Maybe TargetAddresses -> C.AddressAny -> Bool
isAddressInTarget Nothing _ = True -- all addresses are target addresses
isAddressInTarget (Just targetAddresses) addr =
    case addr  of
      C.AddressByron _       -> False
      C.AddressShelley addr' -> addr' `elem` targetAddresses

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
  -> TxIndexInBlock
  -> Map C.TxIn Utxo
getUtxosFromTxBody maybeTargetAddresses txBody@(C.TxBody txBodyContent@C.TxBodyContent{}) txIndexInBlock' =
  fromRight Map.empty (getUtxos $ getTxOutFromTxBodyContent txBodyContent)
  where
    getUtxos :: C.IsCardanoEra era => [C.TxOut C.CtxTx era] -> Either C.EraCastError (Map C.TxIn Utxo)
    getUtxos
      = fmap (mconcat . imap txoutToUtxo) . traverse (C.eraCast CurrentEra)

    txid = C.getTxId txBody
    txoutToUtxo :: Int -> TxOut -> Map C.TxIn Utxo
    txoutToUtxo ix txout =
        let txin = C.TxIn txid (C.TxIx (fromIntegral ix))
        in case getUtxoFromTxOut maybeTargetAddresses txin txout txIndexInBlock' of
            Nothing   -> Map.empty
            Just utxo -> Map.singleton txin utxo

getUtxoFromTxOut
  :: Maybe TargetAddresses -- ^ Target addresses to filter for
  -> C.TxIn -- ^ unique id and position of this transaction
  -> C.TxOut C.CtxTx era -- ^ Cardano TxOut
  -> TxIndexInBlock
  -> Maybe Utxo -- ^ Utxo
getUtxoFromTxOut maybeTargetAddresses txIn' (C.TxOut addr val dtum refScript) txIndexInBlock' = do
  guard $ isAddressInTarget maybeTargetAddresses addrAny
  pure $ Utxo
    { _txIn = txIn'
    , _address = addrAny
    , _value = C.txOutValueToValue val
    , _datum = datum'
    , _datumHash = datumHash'
    , _inlineScript = inlineScript'
    , _inlineScriptHash = inlineScriptHash'
    , _txIndexInBlock = txIndexInBlock'
    }
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
  C.ReferenceScript _ s@(C.ScriptInAnyLang C.SimpleScriptLanguage script) ->
      (Just s, Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script)->
      (Just s, Just . C.hashScript $ script)
  C.ReferenceScript _ s@(C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script)->
      (Just s, Just . C.hashScript $ script)

-- | Get the datum hash and datum or a transaction output.
getScriptDataAndHash
  :: C.TxOutDatum C.CtxTx era
  -> (Maybe C.ScriptData, Maybe (C.Hash C.ScriptData))
getScriptDataAndHash C.TxOutDatumNone         = (Nothing, Nothing)
getScriptDataAndHash (C.TxOutDatumHash _ h)   = (Nothing, Just h)
getScriptDataAndHash (C.TxOutDatumInTx _ d)   = (Just $ C.getScriptData d, (Just . C.hashScriptDataBytes) d)
getScriptDataAndHash (C.TxOutDatumInline _ d) = (Just $ C.getScriptData d, (Just . C.hashScriptDataBytes) d)

getInputsFromTx :: C.Tx era -> Map C.TxIn C.TxId
getInputsFromTx (C.Tx txbody _) = getInputs txbody

-- | Compute TxIn
--  If phase-2 validation fails, we only process TxIns associated with collateral
getInputs :: C.TxBody era -> Map C.TxIn C.TxId
getInputs b@(C.TxBody C.TxBodyContent
                 {C.txIns, C.txInsCollateral, C.txScriptValidity }) =
  let
    inputs = case txScriptValidityToScriptValidity txScriptValidity of
      C.ScriptValid -> fst <$> txIns
      C.ScriptInvalid -> case txInsCollateral of
        C.TxInsCollateralNone     -> []
        C.TxInsCollateral _ txins -> txins
  in
    Map.fromList $ (, C.getTxId b) <$> inputs


-- | Duplicated from cardano-api (not exposed in cardano-api)
-- This function should be removed when marconi will depend on a cardano-api version that has accepted this PR:
-- https://github.com/input-output-hk/cardano-node/pull/4569
txScriptValidityToScriptValidity :: C.TxScriptValidity era -> C.ScriptValidity
txScriptValidityToScriptValidity C.TxScriptValidityNone                = C.ScriptValid
txScriptValidityToScriptValidity (C.TxScriptValidity _ scriptValidity) = scriptValidity

-- | does the transaction contain a targetAddress
isAddressInTarget' :: TargetAddresses -> Utxo -> Bool
isAddressInTarget' targetAddresses utxo =
    case utxo ^. address  of
      C.AddressByron _       -> False
      C.AddressShelley addr' -> addr' `elem` targetAddresses

balanceUtxoFromTx
  :: C.IsCardanoEra era
  => Maybe TargetAddresses    -- ^ target addresses to filter for
  -> (C.Tx era, TxIndexInBlock)
  -> TxOutBalance
balanceUtxoFromTx addrs (C.Tx txBody _, txIndexInBlock') =
    let
        txInputs = getInputs txBody -- adjusted txInput after phase-2 validation
        utxoRefs :: Map C.TxIn Utxo
        utxoRefs = getUtxosFromTxBody addrs txBody txIndexInBlock'
    in TxOutBalance utxoRefs txInputs
