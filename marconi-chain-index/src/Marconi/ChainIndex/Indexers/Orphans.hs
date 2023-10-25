{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Indexers.Orphans where

import Cardano.Api qualified as C
import Marconi.Core qualified as Core

instance Core.HasGenesis C.ChainPoint where
  genesis = C.ChainPointAtGenesis
