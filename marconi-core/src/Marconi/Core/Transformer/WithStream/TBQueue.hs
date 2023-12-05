module Marconi.Core.Transformer.WithStream.TBQueue where

import Control.Concurrent.STM (TBQueue, atomically, readTBQueue, writeTBQueue)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
 )
import Marconi.Core.Transformer.WithAction (
  WithAction (WithAction),
  WithActionConfig (WithActionConfig),
 )
import Marconi.Core.Type (Point, Timed)
import Streaming (Of, Stream)
import Streaming.Prelude (repeatM)

-- | Stream from a given @TBQueue@
streamFromTBQueue
  :: forall m r
   . (MonadIO m)
  => TBQueue r
  -> Stream (Of r) m ()
streamFromTBQueue q = repeatM . liftIO . atomically $ readTBQueue q

-- | A smart constructor for @WithAction@, using @TBQueue@
withStream
  :: (Timed (Point event) event -> r)
  -> TBQueue r
  -> indexer event
  -> WithAction indexer event
withStream mapping q idx =
  let pushEvent x = atomically $ writeTBQueue q (mapping x)
   in WithAction $ IndexTransformer (WithActionConfig pushEvent) idx
