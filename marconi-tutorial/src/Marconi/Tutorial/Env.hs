{-# LANGUAGE TemplateHaskell #-}

module Marconi.Tutorial.Env where

import Cardano.BM.Trace (Trace)
import Control.Lens (Lens', makeLenses)
import Data.Text (Text)
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.Indexers.AddressCount (AddressCountStandardWorker)

data Env = Env
  { _envIndexers :: IndexersEnv
  -- ^ Access the Tutorial indexers for querying purposes.
  , _envCliArgs :: !CLI.Options
  , _envStdoutTrace :: !(Trace IO Text)
  , _securityParam :: !SecurityParam
  }

-- | Should contain all the indexers required by the tutorial.
newtype IndexersEnv = IndexersEnv
  { _addressCountIndexerEnv :: AddressCountIndexerEnv
  }

newtype AddressCountIndexerEnv = AddressCountIndexerEnv
  { _addressCountIndexerVar :: AddressCountStandardWorker
  }

makeLenses ''Env
makeLenses ''IndexersEnv
makeLenses ''AddressCountIndexerEnv

addressCountIndexerWorker :: Lens' Env AddressCountStandardWorker
addressCountIndexerWorker = envIndexers . addressCountIndexerEnv . addressCountIndexerVar
