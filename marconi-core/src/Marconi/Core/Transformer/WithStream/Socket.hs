module Marconi.Core.Transformer.WithStream.Socket where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString qualified as BS
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
import Streaming.Prelude (Of, Stream, yield)

-- | Stream from a given @Socket@
streamFromSocket
  :: (MonadIO m, Binary r)
  => Socket
  -> Stream (Of r) m ()
streamFromSocket sock = loop BL.empty
  where
    loop buffer = do
      chunk <- liftIO $ SBS.recv sock 4096
      unless (BS.null chunk) $ do
        let newBuffer = buffer <> BS.fromStrict chunk
        processBuffer newBuffer

    processBuffer buffer =
      case decodeOrFail buffer of
        Left _ -> loop buffer
        Right (rest, _, res) -> do
          yield res
          if BL.null rest
            then loop BL.empty
            else processBuffer rest

-- | A smart constructor for @WithStream@, using @Socket@
withStream
  :: (Binary r)
  => (Timed (Point event) event -> r)
  -> Socket
  -> indexer event
  -> WithAction indexer event
withStream mapping sock idx =
  let overSocket event = send (mapping event) sock
      send :: (Binary r) => r -> Socket -> IO ()
      send r s = sendAll s $ BL.toStrict $ encode r
   in WithAction $ IndexTransformer (WithActionConfig overSocket) idx
