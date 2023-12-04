module Marconi.Core.Transformer.WithStream.Socket where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy qualified as BL
import Marconi.Core (Point, Timed)
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
 )
import Marconi.Core.Transformer.WithAction (
  WithAction (WithAction),
  WithActionConfig (WithActionConfig),
 )
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)
import Network.Socket.ByteString qualified as SBS
import Streaming.Prelude (Of, Stream, repeatM)

-- | Stream from a given @Socket@
streamFromSocket
  :: (MonadIO m, Binary r)
  => Socket
  -> Stream (Of r) m ()
streamFromSocket sock = repeatM $ liftIO $ fmap (decode . BL.fromStrict) (SBS.recv sock 4096)

-- | A smart constructor for @WithStream@, using @Socket@
withStream
  :: (Binary r)
  => (Timed (Point event) event -> r)
  -> Socket
  -> indexer event
  -> IO (WithAction indexer event)
withStream mapping sock idx = do
  let overSocket event = send (mapping event) sock
  pure $ WithAction $ IndexTransformer (WithActionConfig overSocket) idx
  where
    send :: (Binary r) => r -> Socket -> IO ()
    send r s = sendAll s $ BL.toStrict $ encode r
