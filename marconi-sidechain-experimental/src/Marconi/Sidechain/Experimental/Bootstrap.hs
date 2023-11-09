module Marconi.Sidechain.Experimental.Bootstrap where

import Control.Monad.Reader (ReaderT)
import Marconi.Sidechain.Experimental.Env (SidechainEnv)

-- | TODO: PLT-8076
runSidechainIndexers :: ReaderT SidechainEnv IO ()
runSidechainIndexers = undefined
