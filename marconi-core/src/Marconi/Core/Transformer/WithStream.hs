module Marconi.Core.Transformer.WithStream where

import Marconi.Core.Transformer.IndexTransformer (IndexTransformer (IndexTransformer))
import Marconi.Core.Transformer.WithAction (
  WithAction (WithAction),
  WithActionConfig (WithActionConfig),
 )
import Marconi.Core.Transformer.WithStream.Streamable (
  Streamable (streamTo),
 )
import Marconi.Core.Type (Point, Timed)

{- | A smart constructor for @WithAction@, using any streamable.

  Be aware: depending on the implementation you choose, this can lead to the indexer blocking, given
  a slow (or non-existent) consumer!
-}
withStream
  :: (Streamable a r)
  => (Timed (Point event) event -> r)
  -> a
  -> indexer event
  -> WithAction indexer event
withStream mapping container =
  WithAction . IndexTransformer (WithActionConfig $ streamTo mapping container)
