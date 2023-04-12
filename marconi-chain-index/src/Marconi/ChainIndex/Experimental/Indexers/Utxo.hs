{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Marconi.ChainIndex.Experimental.Indexers.Utxo where

import Control.Lens.Fold (folded)
import Control.Lens.Operators ((^.), (^..))
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (foldl')
import Data.Ord ()
import Data.Set (Set)
import Data.Set qualified as Set


import Cardano.Api ()
import Cardano.Api qualified as C
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()

import Marconi.Core.Experiment qualified as Core

data Utxo = Utxo
  { _address          :: !C.AddressAny
  , _txIn             :: !C.TxIn
  , _datum            :: !(Maybe C.ScriptData)
  , _datumHash        :: !(Maybe (C.Hash C.ScriptData))
  , _value            :: !C.Value
  , _inlineScript     :: !(Maybe C.ScriptInAnyLang)
  , _inlineScriptHash :: !(Maybe C.ScriptHash)
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
    { ueUtxos  :: !(Set Utxo)
    , ueInputs :: !(Set Spent)
    } deriving (Eq, Ord, Show, Generic)

$(makeLenses ''UtxoEvent)

type instance Core.Point UtxoEvent = C.ChainPoint

newtype QueryUtxoByAddress = QueryUtxoByAddress (C.AddressAny, Maybe C.SlotNo)

type instance Core.Result QueryUtxoByAddress = [Core.TimedEvent Utxo]

data UtxoRow = UtxoRow
  { _urUtxo       :: !Utxo,
    _urChainPoint :: !C.ChainPoint
  }
  deriving (Show, Eq, Ord, Generic)

data SpentRow = SpentRow
  { _srSpent      :: !Spent,
    _srChainPoint :: !C.ChainPoint
  }
  deriving (Show, Eq, Ord, Generic)

type instance Core.InsertRecord UtxoEvent = [(Maybe UtxoRow, Maybe SpentRow)] -- TODO not sure about this

instance Core.ResumableResult m UtxoEvent QueryUtxoByAddress Core.ListIndexer where
  resumeResult ::
    Core.Point UtxoEvent ->
    QueryUtxoByAddress ->
    Core.ListIndexer UtxoEvent ->
    m (Core.Result QueryUtxoByAddress) ->
    m (Core.Result QueryUtxoByAddress)
  resumeResult (C.ChainPoint sno _) (QueryUtxoByAddress (addr, maybeSno)) events onDiskResult = undefined

-- TODO filter the in memmory for the address, then merge with onDisk

-- take a look at EvetnAtQuery line 1022, implemenation
-- used in the resume result,
instance MonadIO m => Core.Queryable m UtxoEvent QueryUtxoByAddress Core.ListIndexer  where
  query
    :: Core.Point UtxoEvent -- ^ give me what you know up to this point, potentially a point in future
    -> QueryUtxoByAddress
    -> Core.ListIndexer UtxoEvent -- ^ get the point for ListIndexer
    -> m (Core.Result QueryUtxoByAddress)
  query (C.ChainPoint sno _) (QueryUtxoByAddress (addr, maybeSno)) listXer = pure timedEventAtAddress'
    -- pure $ fmap (Core.TimedEvent pnt) eventsAtAddress
    where
      utxoEvents :: [UtxoEvent] = listXer ^. Core.events ^.. folded . Core.event
      utxosAtAddress :: UtxoEvent -> UtxoEvent
      pnt :: Core.Point UtxoEvent = listXer ^. Core.latest
      utxosAtAddress (UtxoEvent outs ins)  = UtxoEvent (Set.filter addressFilter outs) ins
      eventsAtAddress :: [UtxoEvent] = fmap utxosAtAddress utxoEvents
      timedEventAtAddress :: [Core.TimedEvent UtxoEvent] = fmap (Core.TimedEvent pnt) eventsAtAddress
      getUtxoTimedEvent :: Core.TimedEvent UtxoEvent -> [Core.TimedEvent Utxo]
      getUtxoTimedEvent = undefined
      timedEventAtAddress' :: [Core.TimedEvent Utxo] = concatMap ( getUtxoTimedEvent . Core.TimedEvent pnt) eventsAtAddress

      pointFilter :: Maybe C.SlotNo -> Bool -- TODO need the case where utxo becomes spent in future
      pointFilter ms = maybe True (sno <=) ms

      addressFilter :: Utxo -> Bool
      addressFilter u = (u ^. address) == addr


-- | combine events in the context of time
instance Semigroup UtxoEvent where
  (UtxoEvent us is) <> (UtxoEvent us' is') =
    UtxoEvent utxos spents
    where
      getTxIn :: Utxo -> C.TxIn
      getTxIn u = u ^. txIn
      txins :: Set C.TxIn = Set.union (Set.map unSpent is) (Set.map unSpent is')
      utxos
        = foldl' (\a c -> if getTxIn c `Set.notMember` txins then Set.insert c a; else a) Set.empty
        $ Set.union us us'
      spents = Set.map Spent txins

instance Monoid UtxoEvent where
  mempty = UtxoEvent Set.empty Set.empty

