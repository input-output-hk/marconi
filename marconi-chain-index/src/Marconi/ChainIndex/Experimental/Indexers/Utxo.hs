{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Marconi.ChainIndex.Experimental.Indexers.Utxo where

import Control.Lens (view)
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
        filter (\(Core.TimedEvent _ utxo) -> (utxo ^. utxoTxIn) `notElem` txins events)
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
      addressFilter u = (u ^. utxoAddress) == addr

      timedEventAtAddress :: [Core.TimedEvent UtxoEvent]
      timedEventAtAddress =
        let
            utxosAtAddress :: UtxoEvent -> UtxoEvent
            utxosAtAddress (UtxoEvent outs ins)  = UtxoEvent (Set.filter addressFilter outs) ins
            eventsAtAddress :: [UtxoEvent]
            eventsAtAddress = fmap utxosAtAddress utxoEvents

        in fmap (Core.TimedEvent pnt)  eventsAtAddress

      timedEventsAtAddress :: [Core.TimedEvent Utxo]
      timedEventsAtAddress = concatMap timedEventToTimedUtxos timedEventAtAddress


timedEventToTimedUtxos :: Core.TimedEvent UtxoEvent -> [Core.TimedEvent Utxo]
timedEventToTimedUtxos te =
  let
    getutxos :: UtxoEvent -> [Utxo]
    getutxos = Set.toList . _ueUtxos
  in
    fmap (\u -> Core.TimedEvent (te ^. Core.point) u) (getutxos (te ^. Core.event))

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

