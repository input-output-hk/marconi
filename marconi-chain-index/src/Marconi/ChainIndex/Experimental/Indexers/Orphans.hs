{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Experimental.Indexers.Orphans where

import Cardano.Api qualified as C
import Marconi.Core.Experiment qualified as Core

instance Core.HasGenesis C.ChainPoint where
  genesis = C.ChainPointAtGenesis

instance Core.OrdPoint C.ChainPoint where
  comparePoint C.ChainPointAtGenesis C.ChainPointAtGenesis = Core.Same
  comparePoint C.ChainPoint{} C.ChainPointAtGenesis = Core.After
  comparePoint C.ChainPointAtGenesis C.ChainPoint{} = Core.Before
  comparePoint (C.ChainPoint p1 h1) (C.ChainPoint p2 h2)
    | p1 < p2 = Core.Before
    | p1 > p2 = Core.After
    | p1 == p2 && h1 == h2 = Core.Same
    | otherwise = Core.Fork
