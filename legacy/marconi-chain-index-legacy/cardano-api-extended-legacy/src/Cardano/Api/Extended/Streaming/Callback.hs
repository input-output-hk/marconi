{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.Api.Extended.Streaming.Callback where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming.ChainSyncEvent (
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
 )
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Exception (throw)
import Data.Word (Word32)
import Network.TypedProtocol.Pipelined (N (Z), Nat (Succ, Zero))
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (
  PipelineDecision (Collect),
  pipelineDecisionMax,
 )

-- * Raw chain-sync clients using callback

blocksCallbackPipelined
  :: Word32
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.ChainPoint
  -> (ChainSyncEvent (C.BlockInMode C.CardanoMode) -> IO ())
  -> IO ()
blocksCallbackPipelined n con point callback =
  C.connectToLocalNode con $
    C.LocalNodeClientProtocols
      { C.localChainSyncClient =
          C.LocalChainSyncClientPipelined $
            CSP.ChainSyncClientPipelined $
              pure $
                CSP.SendMsgFindIntersect [point] onIntersect
      , C.localTxSubmissionClient = Nothing
      , C.localStateQueryClient = Nothing
      , C.localTxMonitoringClient = Nothing
      }
  where
    onIntersect =
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \_ _ -> pure $ work n
        , CSP.recvMsgIntersectNotFound = throw NoIntersectionFound
        }

    work
      :: Word32 -> CSP.ClientPipelinedStIdle 'Z (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
    work pipelineSize = requestMore Origin Origin Zero
      where
        requestMore -- was clientIdle_RequestMoreN
          :: WithOrigin C.BlockNo
          -> WithOrigin C.BlockNo
          -> Nat n
          -> CSP.ClientPipelinedStIdle n (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
        requestMore clientTip serverTip rqsInFlight =
          let
           in -- handle a response

              -- fire more requests

              case pipelineDecisionMax pipelineSize rqsInFlight clientTip serverTip of
                Collect -> case rqsInFlight of
                  Succ predN -> CSP.CollectResponse Nothing (clientNextN predN)
                _ -> CSP.SendMsgRequestNextPipelined (requestMore clientTip serverTip (Succ rqsInFlight))

        clientNextN
          :: Nat n
          -> CSP.ClientStNext n (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
        clientNextN rqsInFlight =
          CSP.ClientStNext
            { CSP.recvMsgRollForward = \bim@(C.BlockInMode (C.Block (C.BlockHeader _ _ blockNo) _) _) ct -> do
                callback $ RollForward bim ct
                return $ requestMore (At blockNo) (fromChainTip ct) rqsInFlight
            , CSP.recvMsgRollBackward = \cp ct -> do
                callback $ RollBackward cp ct
                return $ requestMore Origin (fromChainTip ct) rqsInFlight
            }

    fromChainTip :: C.ChainTip -> WithOrigin C.BlockNo
    fromChainTip ct = case ct of
      C.ChainTipAtGenesis -> Origin
      C.ChainTip _ _ bno -> At bno

blocksCallback
  :: C.LocalNodeConnectInfo C.CardanoMode
  -> C.ChainPoint
  -> (ChainSyncEvent (C.BlockInMode C.CardanoMode) -> IO ())
  -> IO ()
blocksCallback con point callback =
  C.connectToLocalNode con $
    C.LocalNodeClientProtocols
      { C.localChainSyncClient =
          C.LocalChainSyncClient $ CS.ChainSyncClient $ pure $ CS.SendMsgFindIntersect [point] onIntersect
      , C.localTxSubmissionClient = Nothing
      , C.localStateQueryClient = Nothing
      , C.localTxMonitoringClient = Nothing
      }
  where
    onIntersect =
      CS.ClientStIntersect
        { CS.recvMsgIntersectFound = \_ _ -> CS.ChainSyncClient sendRequestNext
        , CS.recvMsgIntersectNotFound = throw NoIntersectionFound
        }
    sendRequestNext = pure $ CS.SendMsgRequestNext onNext (pure onNext)
      where
        onNext =
          CS.ClientStNext
            { CS.recvMsgRollForward = \bim ct -> CS.ChainSyncClient $ do
                callback $ RollForward bim ct
                sendRequestNext
            , CS.recvMsgRollBackward = \cp ct -> CS.ChainSyncClient $ do
                callback $ RollBackward cp ct
                sendRequestNext
            }
