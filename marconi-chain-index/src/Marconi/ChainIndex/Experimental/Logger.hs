-- | Common loggers for marconi
module Marconi.ChainIndex.Experimental.Logger (
  BM.nullTracer,
  defaultStdOutLogger,
) where

import Cardano.BM.Configuration qualified as BM
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Tracing qualified as BM

import Data.Text (Text)

-- | StdOut logger, only log stuff above the Info level
defaultStdOutLogger :: Text -> IO (Trace IO Text)
defaultStdOutLogger appName = do
  cfg <- BM.defaultConfigStdout
  BM.setMinSeverity cfg BM.Info
  BM.setupTrace (Right cfg) appName
