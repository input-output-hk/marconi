module Marconi.Core.Transformer.WithStream.Socket where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary, encode)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Marconi.Core (Point, Timed)
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
 )
import Marconi.Core.Transformer.WithAction (
  WithAction (WithAction),
  WithActionConfig (WithActionConfig),
 )
import Network.Run.TCP (runTCPServer)
import Network.Socket (HostName, ServiceName, Socket)
import Network.Socket.ByteString (sendAll)
import Network.Socket.ByteString qualified as SBS
import Streaming.Prelude (Of, Stream, repeatM)

-- | Stream from a given @Socket@
streamFromSocket :: (MonadIO m) => Socket -> Stream (Of String) m ()
streamFromSocket sock = repeatM $ liftIO $ fmap B.unpack (SBS.recv sock 4096)

-- | A smart constructor for @WithStream@
withStream
  :: (Binary r)
  => (Timed (Point event) event -> r)
  -> Maybe HostName
  -> ServiceName
  -> indexer event
  -> IO (WithAction indexer event)
withStream mapping hostName serviceName idx = do
  let overSocket event = runTCPServer hostName serviceName (send $ mapping event)
  pure $ WithAction $ IndexTransformer (WithActionConfig overSocket) idx
  where
    send :: (Binary r) => r -> Socket -> IO ()
    send r sock = sendAll sock $ BL.toStrict $ encode r
