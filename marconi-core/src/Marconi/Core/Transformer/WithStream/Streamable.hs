module Marconi.Core.Transformer.WithStream.Streamable where

import Control.Concurrent.STM (TBQueue, atomically, readTBQueue, writeTBQueue)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Marconi.Core.Type (Point, Timed)
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as SBS
import Network.Socket.ByteString.Lazy (sendAll)
import Streaming (Of, Stream)
import Streaming.Prelude (repeatM, yield)

-- | Something that can have mapped events streamed to or from it
class Streamable a r where
  streamFrom :: a -> Stream (Of r) IO ()
  streamTo :: (Timed (Point event) event -> r) -> a -> Timed (Point event) event -> IO ()

{- | A streaming implementation using @Socket@. For an example of how
  to use it, please see: test/Marconi.CoreSpec.hs/propWithStreamSocket
-}
instance (Binary r) => Streamable Socket r where
  streamFrom sock = loop BL.empty
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
  streamTo mapping sock = sendAll sock . encode . mapping

{- | An experimental streaming implementation using @TBQueue@.

    IMPORTANT: Note that this will block when used with @withStream@ if you have slow or
               non-existent consumers!

    For an example of how to use it, please see: test/Marconi.CoreSpec.hs/propWithStreamTBQueue
-}
instance Streamable (TBQueue r) r where
  streamFrom = repeatM . liftIO . atomically . readTBQueue
  streamTo mapping q = atomically . writeTBQueue q . mapping
