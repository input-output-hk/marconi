{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Common loggers for marconi
module Marconi.Cardano.Core.Logger (
  BM.nullTracer,
  defaultStdOutLogger,
  mkMarconiTrace,
  marconiFormatting,
  MarconiTrace,
) where

import Cardano.BM.Backend.Switchboard qualified as BM
import Cardano.BM.Configuration qualified as BM
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Tracing (contramap)
import Cardano.BM.Tracing qualified as BM
import Data.Text (Text)
import Marconi.Cardano.Core.Orphans ()
import Prettyprinter (Doc)
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty

-- | Alias for a 'Doc' tracer, it is the 'Trace' used throughout the Marconi application
type MarconiTrace m = Trace m (Doc ())

-- | StdOut logger, only log stuff above the Info level
defaultStdOutLogger :: Text -> BM.Severity -> IO (Trace IO Text, BM.Switchboard Text)
defaultStdOutLogger appName logLevel = do
  cfg <- BM.defaultConfigStdout
  BM.setMinSeverity cfg logLevel
  BM.setupTrace_ cfg appName

-- | Builds a 'MarconiTrace' from a base tracer.
mkMarconiTrace :: Trace m Text -> MarconiTrace m
mkMarconiTrace =
  contramap . fmap . fmap $ marconiFormatting

marconiFormatting :: Pretty.Doc ann -> Text
marconiFormatting =
  Pretty.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions
