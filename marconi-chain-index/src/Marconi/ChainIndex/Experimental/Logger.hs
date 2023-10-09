-- | Common loggers for marconi
module Marconi.ChainIndex.Experimental.Logger (
  BM.nullTracer,
  defaultStdOutLogger,
) where

import Cardano.BM.Backend.Switchboard qualified as BM
import Cardano.BM.Configuration qualified as BM
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Tracing qualified as BM
import Data.Text (Text)

-- | StdOut logger, only log stuff above the Info level
defaultStdOutLogger :: Text -> IO (Trace IO Text, BM.Switchboard Text)
defaultStdOutLogger appName = do
  cfg <- BM.defaultConfigStdout
  BM.setMinSeverity cfg BM.Info
  BM.setupTrace_ cfg appName
