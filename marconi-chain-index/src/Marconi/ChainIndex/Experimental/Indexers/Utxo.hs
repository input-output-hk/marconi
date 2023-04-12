{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Marconi.ChainIndex.Experimental.Indexers.Utxo where

import Control.Lens.Operators ((^.))
import Control.Lens.TH (makeLenses)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord ()
import Data.Set (Set)
import Data.Set qualified as Set


import Cardano.Api ()
import Cardano.Api qualified as C
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Experiment (IsIndex, ListIndexer, Point, Result, ResumableResult (resumeResult), TimedEvent)

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

type instance Point UtxoEvent = C.ChainPoint

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

type instance Point Spent = C.ChainPoint

-- | The effect of a transaction (or a number of them) on the tx output map.
data BalanceUtxo =
  BalanceUtxo
    { _tobUnspent :: !(Map C.TxIn Utxo)
    -- ^ Outputs newly added by the transaction(s)
    , _tobSpent   :: !(Set C.TxIn)
    -- ^ Outputs spent by the transaction(s)
    }
    deriving (Eq, Show, Generic)

instance Semigroup BalanceUtxo where
    tobL <> tobR =
      let
        tobUnspentKeys :: Set C.TxIn
        tobUnspentKeys
          = (Map.keysSet $ _tobUnspent tobR)
          <> ((Map.keysSet $ _tobUnspent tobL) `Set.difference` _tobSpent tobR)
        utxoMap :: Map C.TxIn Utxo
        utxoMap = _tobUnspent tobL `Map.union` _tobUnspent tobR
        tobSpentKeys :: Set C.TxIn
        tobSpentKeys
          = _tobSpent tobL
          <> ( _tobSpent tobR `Set.difference` (Map.keysSet $ _tobUnspent tobL))
      in
        BalanceUtxo
            { _tobUnspent = Map.restrictKeys utxoMap tobUnspentKeys
            , _tobSpent = tobSpentKeys
            }

instance Monoid BalanceUtxo where
    mappend = (<>)
    mempty = BalanceUtxo mempty mempty

newtype QueryUtxoByAddress = QueryUtxoByAddress C.AddressAny

type instance Result QueryUtxoByAddress = [TimedEvent Utxo]


instance ResumableResult m UtxoEvent QueryUtxoByAddress ListIndexer where
  resumeResult
    :: Ord (Point UtxoEvent) => Point UtxoEvent
    -> QueryUtxoByAddress
    -> ListIndexer UtxoEvent
    -> m (Result QueryUtxoByAddress)
    -> m (Result QueryUtxoByAddress)
  resumeResult point query indexer onDiskResult = undefined


  -- TODO filter the in memmory for the address, then merge with onDisk

-- take a look at EvetnAtQuery line 1022, implemenation

eventsAtAddress
  :: Foldable f
  => C.AddressAny -- ^ Address query
  -> Maybe C.SlotNo -- ^ Latest included chainpoint
  -> f UtxoEvent  -- ^ Utxo event
  -> f UtxoEvent -- ^ Utxo event at thegiven address
eventsAtAddress addr p =
  let
    splitEventAtAddress :: UtxoEvent -> [UtxoEvent]
    splitEventAtAddress event =
      let

        isBeforeSlot :: C.SlotNo -> C.ChainPoint -> Bool
        isBeforeSlot s = \case
            C.ChainPointAtGenesis -> True
            C.ChainPoint s' _     -> s' <= s

        pointFilter :: Maybe C.SlotNo -> UtxoEvent -> Bool
        pointFilter ms
            = maybe
                (const True)
                (\s -> isBeforeSlot s . ueChainPoint)
                ms

        addressFilter :: Utxo -> Bool
        addressFilter u = (u ^. address) == addr

        utxosAtAddress :: Set Utxo
        utxosAtAddress = Set.filter addressFilter $ ueUtxos event

      in [event {ueUtxos = utxosAtAddress} | not (null utxosAtAddress) && pointFilter p event]

   in concatMap splitEventAtAddress
