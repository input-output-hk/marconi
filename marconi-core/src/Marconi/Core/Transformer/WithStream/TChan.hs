module Marconi.Core.Transformer.WithStream.TChan where

import Control.Concurrent.STM (atomically, newBroadcastTChan, readTChan)
import Control.Concurrent.STM.TChan (TChan, dupTChan, writeTChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Marconi.Core (Point, Timed)
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
 )
import Marconi.Core.Transformer.WithAction (
  WithAction (WithAction),
  WithActionConfig (WithActionConfig),
 )
import Streaming (Of, Stream)
import Streaming.Prelude (repeatM)

-- | Stream from a given @TChan@
streamFromTChan
  :: forall m r
   . (MonadIO m)
  => TChan r
  -> Stream (Of r) m ()
streamFromTChan chan = repeatM . liftIO . atomically $ readTChan chan

-- | A smart constructor for @WithAction@
withStream
  :: (Show event, Show (Point event))
  => (Timed (Point event) event -> r)
  -> TChan r
  -> indexer event
  -> IO (WithAction indexer event)
withStream mapping chan idx = do
  let pushEvent x = do
        liftIO $ print ("pushing" ++ show x)
        liftIO $ atomically $ writeTChan chan (mapping x)
        liftIO $ print "pushed"
  pure $ WithAction $ IndexTransformer (WithActionConfig pushEvent) idx
