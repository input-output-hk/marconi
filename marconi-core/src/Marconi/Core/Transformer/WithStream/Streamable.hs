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

{- | A streaming implementation using @Socket@. Here's an example of how
  to use it, in the form of a property test:

    propWithStreamSocket :: Property
    propWithStreamSocket = monadicExceptTIO @() $ GenM.forAllM genChainWithInstability $ \args -> do
      let chainSubset = take (chainSizeSubset args) (eventGenerator args)

      -- A semaphore to let the client know it can connect
      serverStarted <- liftIO $ newQSem 1

      -- Run the server and client concurrently. The server runs forever, so the client will finish
      -- first
      Right (actual, expected) <-
        liftIO $ race (server chainSubset serverStarted) (client chainSubset serverStarted)

      GenM.stop (actual == expected)
      where
        server chainSubset serverStarted = do
          runTCPServer Nothing "3005" serve
          where
            serve :: Socket -> IO ()
            serve s = do
              -- Signal that the server has started so the client knows it can connect
              signalQSem serverStarted

              _ <- runExceptT $ do
                -- Mapping function, applied to the event before running the action
                let toInt = view (Core.event . _TestEvent)

                -- Create the streaming indexer, passing the server socket
                    indexer = withStream toInt s Core.mkListIndexer

                -- Run the indexer
                foldM_ (flip process) indexer chainSubset

              -- Send an empty bytestring to mark message completion
              sendAll s BS.empty
        client chainSubset serverStarted = do

          -- Wait until the server's started before trying to connect
          waitQSem serverStarted
          runTCPClient "127.0.0.1" "3005" $ \s -> do
            let testEvents :: [TestEvent] = chainSubset ^.. traversed . _Insert . _2 . _Just

                -- Initialise the stream using the client socket
                stream :: Stream (Of Int) IO () = streamFrom s

            -- Use the stream in some way
            str <- S.toList_ stream
            pure (str, fmap (view _TestEvent) testEvents)
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

    Here's an example of how to use it, in the form of a property test:

    propWithStreamTBQueue :: Property
    propWithStreamTBQueue = monadicExceptTIO @() $ GenM.forAllM genChainWithInstability $ \args -> do
        let chainSubset = take (chainSizeSubset args) (eventGenerator args)

        -- Initialise a TBQueue
        q :: TBQueue Int <- liftIO $ newTBQueueIO 10
        _ <- liftIO $
          forkIO $
            void $
              runExceptT $ do

                -- Mapping function, applied to the event before running the action
                let toInt = view (Core.event . _TestEvent)

                -- Create the streaming indexer, passing the @TBQueue@
                let indexer = withStream toInt q Core.mkListIndexer

                -- Run the indexer
                foldM_ (flip process) indexer chainSubset

        let testEvents :: [TestEvent] = chainSubset ^.. traversed . _Insert . _2 . _Just

            -- We're just getting the @Int@s from the @TestEvents@: @data TestEvent = TestEvent Int@
            expected = fmap (view _TestEvent) testEvents

            -- Initialise the stream using the @TBQueue@
            stream = S.take (length testEvents) $ streamFrom q

        -- Use the stream in some way
        actual <- liftIO $ S.toList_ stream

        GenM.stop (actual == expected)
-}
instance Streamable (TBQueue r) r where
  streamFrom = repeatM . liftIO . atomically . readTBQueue
  streamTo mapping q = atomically . writeTBQueue q . mapping
