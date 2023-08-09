{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Marconi.Sidechain.Error where

import Cardano.Api qualified as C
import Control.Exception (Exception)
import Data.Text (Text)
import Marconi.ChainIndex.Indexers.Utxo (UtxoHandle)
import Marconi.Core.Storable (StorableQuery)

data QueryExceptions
  = QueryError !Text
  | UntrackedPolicy C.PolicyId (Maybe C.AssetName)
  | UnexpectedQueryResult !(StorableQuery UtxoHandle)
  | IndexerInternalError !Text
  deriving stock (Show)
  deriving anyclass (Exception)
