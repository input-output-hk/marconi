{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Api.Extended.Streaming (
  withChainSyncEventStream,
  withChainSyncBlockEventStream,
  ChainSyncEvent (..),
  ChainSyncEventException (..),

  -- * Issue types
  BlockEvent (..),

  -- * Stream blocks
  blocks,
  blocksPipelined,
  ignoreRollbacks,
) where

import Cardano.Api qualified as C
import Cardano.Api.ChainSync.Client (
  ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
  ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
  ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward),
 )
import Cardano.Api.Extended.IPC qualified as C
import Cardano.Api.Extended.Streaming.Callback (blocksCallback, blocksCallbackPipelined)
import Cardano.Api.Extended.Streaming.ChainSyncEvent (
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
 )
import Cardano.Slotting.Time qualified as C
import Control.Concurrent qualified as IO
import Control.Concurrent.Async (
  ExceptionInLinkedThread (ExceptionInLinkedThread),
  async,
  link,
  withAsync,
 )
import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  putMVar,
  takeMVar,
 )
import Control.Exception (
  SomeException (SomeException),
  catch,
  throw,
 )
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as Time
import Data.Word (Word32)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

{- | `withChainSyncEventStream` uses the chain-sync mini-protocol to
 connect to a locally running node and fetch blocks from the given
 starting point.
-}
withChainSyncEventStream
  :: FilePath
  -- ^ Path to the node socket
  -> C.NetworkId
  -> [C.ChainPoint]
  -- ^ The point on the chain to start streaming from
  -> (Stream (Of (ChainSyncEvent (C.BlockInMode C.CardanoMode))) IO r -> IO b)
  -- ^ The stream consumer
  -> IO b
withChainSyncEventStream socketPath networkId points consumer =
  do
    -- The chain-sync client runs in a different thread passing the blocks it
    -- receives to the stream consumer through a MVar. The chain-sync client
    -- thread and the stream consumer will each block on each other and stay
    -- in lockstep.
    --
    -- NOTE: choosing a MVar is a tradeoff towards simplicity. In this case a
    -- (bounded) queue could perform better. Indeed a properly-sized buffer
    -- can reduce the time the two threads are blocked waiting for each
    -- other. The problem here is "properly-sized". A bounded queue like
    -- Control.Concurrent.STM.TBQueue allows us to specify a max queue length
    -- but block size can vary a lot (TODO quantify this) depending on the
    -- era. We have an alternative implementation with customizable queue
    -- size (TBMQueue) but it needs to be extracted from the
    -- plutus-chain-index-core package. Using a simple MVar doesn't seem to
    -- slow down marconi's indexing, likely because the difference is
    -- negligeable compared to existing network and IO latencies.  Therefore,
    -- let's stick with a MVar now and revisit later.
    nextChainSyncEventVar <- newEmptyMVar

    let client = chainSyncStreamingClient points nextChainSyncEventVar

        localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
        localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

    withAsync (connectToLocalNodeWithChainSyncClient localNodeConnectInfo client) $ \a -> do
      -- Make sure all exceptions in the client thread are passed to the consumer thread
      link a
      -- Run the consumer
      consumer $ S.repeatM $ takeMVar nextChainSyncEventVar
    -- Let's rethrow exceptions from the client thread unwrapped, so that the
    -- consumer does not have to know anything about async
    `catch` \(ExceptionInLinkedThread _ (SomeException e)) -> throw e

-- | A block, along with some contextual information
data BlockEvent = BlockEvent
  { blockInMode :: C.BlockInMode C.CardanoMode
  , epochNo :: C.EpochNo
  , blockTime :: POSIXTime
  }
  deriving (Show)

{- | Uses the chain-sync mini-protocol to connect to a locally running node and fetch blocks from the
    given starting point, along with their @EpochNo@ and creation time.
-}
withChainSyncBlockEventStream
  :: MonadIO m
  => FilePath
  -- ^ Path to the node socket
  -> C.NetworkId
  -> [C.ChainPoint]
  -- ^ The point on the chain to start streaming from
  -> (Stream (Of (ChainSyncEvent BlockEvent)) m r -> m b)
  -- ^ The stream consumer
  -> m b
withChainSyncBlockEventStream socketPath networkId points consumer =
  liftIO $
    do
      -- The chain-sync client runs in a different thread passing the blocks it
      -- receives to the stream consumer through a MVar. The chain-sync client
      -- thread and the stream consumer will each block on each other and stay
      -- in lockstep.
      --
      -- NOTE: choosing a MVar is a tradeoff towards simplicity. In this case a
      -- (bounded) queue could perform better. Indeed a properly-sized buffer
      -- can reduce the time the two threads are blocked waiting for each
      -- other. The problem here is "properly-sized". A bounded queue like
      -- Control.Concurrent.STM.TBQueue allows us to specify a max queue length
      -- but block size can vary a lot (TODO quantify this) depending on the
      -- era. We have an alternative implementation with customizable queue
      -- size (TBMQueue) but it needs to be extracted from the
      -- plutus-chain-index-core package. Using a simple MVar doesn't seem to
      -- slow down marconi's indexing, likely because the difference is
      -- negligeable compared to existing network and IO latencies.  Therefore,
      -- let's stick with a MVar now and revisit later.
      nextChainSyncEventVar <- newEmptyMVar

      let localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
          localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

      systemStart <-
        C.queryNodeLocalState localNodeConnectInfo Nothing C.QuerySystemStart
          >>= \case
            Left err -> fail $ show err
            Right systemStart -> pure systemStart

      let queryHistoryInMode :: C.QueryInMode C.CardanoMode (C.EraHistory C.CardanoMode)
          queryHistoryInMode = C.QueryEraHistory C.CardanoModeIsMultiEra

          askHistory :: IO (C.EraHistory C.CardanoMode)
          askHistory = do
            res <- C.queryNodeLocalState localNodeConnectInfo Nothing queryHistoryInMode
            case res of
              Left err -> fail $ show err
              Right h -> pure h

          attachEpochAndTime
            :: C.EraHistory C.CardanoMode
            -> ChainSyncEvent (C.BlockInMode C.CardanoMode)
            -> IO (ChainSyncEvent BlockEvent, C.EraHistory C.CardanoMode)
          attachEpochAndTime h (RollBackward cp ct) = pure (RollBackward cp ct, h)
          attachEpochAndTime h evt@(RollForward (C.BlockInMode block _) _) =
            let C.BlockHeader sn _ _ = C.getBlockHeader block
                toEpochTime = Time.utcTimeToPOSIXSeconds . C.fromRelativeTime systemStart
                epochAndTime history = do
                  (epoch, _, _) <- C.slotToEpoch sn history
                  (relativeTime, _) <- C.getProgress sn history
                  pure (epoch, toEpochTime relativeTime)
                buildEpochAndTime
                  :: C.EraHistory C.CardanoMode
                  -> IO
                      ( ChainSyncEvent BlockEvent
                      , C.EraHistory C.CardanoMode
                      )
                buildEpochAndTime history = case epochAndTime history of
                  Left _ -> askHistory >>= buildEpochAndTime
                  Right (epoch, time) ->
                    pure ((\b -> BlockEvent b epoch time) <$> evt, h)
             in buildEpochAndTime h
          client = chainSyncStreamingClient points nextChainSyncEventVar

          -- Compute the next event and upgrade history if needed
          eventLoop
            :: C.EraHistory C.CardanoMode
            -> IO (Either r (ChainSyncEvent BlockEvent, C.EraHistory C.CardanoMode))
          eventLoop history = takeMVar nextChainSyncEventVar >>= fmap Right . attachEpochAndTime history

      history <- askHistory
      withAsync (connectToLocalNodeWithChainSyncClient localNodeConnectInfo client) $ \a -> do
        -- Make sure all exceptions in the client thread are passed to the consumer thread
        link a
        -- Run the consumer
        consumer $ S.unfoldr eventLoop history
      -- Let's rethrow exceptions from the client thread unwrapped, so that the
      -- consumer does not have to know anything about async
      `catch` \(ExceptionInLinkedThread _ (SomeException e)) -> throw e

connectToLocalNodeWithChainSyncClient
  :: C.LocalNodeConnectInfo C.CardanoMode
  -> C.ChainSyncClient (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
  -> IO ()
connectToLocalNodeWithChainSyncClient connectInfo client =
  let
    localNodeSyncClientProtocols =
      C.LocalNodeClientProtocols
        { C.localChainSyncClient = C.LocalChainSyncClient client
        , C.localStateQueryClient = Nothing
        , C.localTxMonitoringClient = Nothing
        , C.localTxSubmissionClient = Nothing
        }
   in
    C.connectToLocalNode connectInfo localNodeSyncClientProtocols

{- | `chainSyncStreamingClient` is the client that connects to a local node
 and runs the chain-sync mini-protocol. This client is fire-and-forget
 and does not require any control.

 If the starting point is such that an intersection cannot be found, this
 client will throw a NoIntersectionFound exception.
-}
chainSyncStreamingClient
  :: [C.ChainPoint]
  -> MVar (ChainSyncEvent block)
  -> C.ChainSyncClient block C.ChainPoint C.ChainTip IO ()
chainSyncStreamingClient points nextChainEventVar =
  C.ChainSyncClient $ pure $ SendMsgFindIntersect points onIntersect
  where
    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \_ _ ->
            C.ChainSyncClient sendRequestNext
        , recvMsgIntersectNotFound =
            -- There is nothing we can do here
            throw NoIntersectionFound
        }

    sendRequestNext =
      pure $ SendMsgRequestNext onNext (pure onNext)
      where
        onNext =
          ClientStNext
            { recvMsgRollForward = \bim ct ->
                C.ChainSyncClient $ do
                  putMVar nextChainEventVar (RollForward bim ct)
                  sendRequestNext
            , recvMsgRollBackward = \cp ct ->
                C.ChainSyncClient $ do
                  putMVar nextChainEventVar (RollBackward cp ct)
                  sendRequestNext
            }

{- | Create stream of @ChainSyncEvent (BlockInMode CardanoMode)@ from
 a node at @socketPath@ with @networkId@ starting at @point@.
-}
blocks
  :: C.LocalNodeConnectInfo C.CardanoMode
  -> C.ChainPoint
  -> S.Stream (S.Of (ChainSyncEvent (C.BlockInMode C.CardanoMode))) IO r
blocks con chainPoint = do
  chan <- liftIO IO.newChan
  void $ liftIO $ linkedAsync $ blocksCallback con chainPoint $ IO.writeChan chan
  S.repeatM $ IO.readChan chan

blocksPipelined
  :: Word32
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.ChainPoint
  -> S.Stream (S.Of (ChainSyncEvent (C.BlockInMode C.CardanoMode))) IO r
blocksPipelined pipelineSize con chainPoint = do
  chan <- liftIO IO.newChan
  void $
    liftIO $
      linkedAsync $
        blocksCallbackPipelined pipelineSize con chainPoint $
          IO.writeChan chan
  S.repeatM $ IO.readChan chan

{- | Ignore rollback events in the chainsync event stream. Useful for
 monitor which blocks has been seen by the node, regardless whether
 they are permanent.
-}
ignoreRollbacks :: (Monad m) => S.Stream (S.Of (ChainSyncEvent a)) m r -> S.Stream (S.Of a) m r
ignoreRollbacks = S.mapMaybe (\case RollForward e _ -> Just e; _ -> Nothing)

linkedAsync :: IO a -> IO ()
linkedAsync action = link =<< async action
