{-# LANGUAGE TemplateHaskell #-}

module Marconi.Tutorial.Env where

import Control.Lens (Lens', makeLenses)
import Marconi.ChainIndex.Types (MarconiTrace, SecurityParam)
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.Indexers.AddressCount (AddressCountStandardWorker)

data Env ann = Env
  { _envIndexers :: IndexersEnv
  -- ^ Access the Tutorial indexers for querying purposes.
  , _envCliArgs :: !CLI.Options
  , _envStdoutTrace :: !(MarconiTrace IO ann)
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

addressCountIndexerWorker :: Lens' (Env ann) AddressCountStandardWorker
addressCountIndexerWorker = envIndexers . addressCountIndexerEnv . addressCountIndexerVar
