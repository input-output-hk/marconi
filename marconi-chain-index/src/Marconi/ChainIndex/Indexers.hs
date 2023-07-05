{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.Indexers where

import Cardano.Api (
  Block (Block),
  BlockHeader (BlockHeader),
  BlockInMode (BlockInMode),
  CardanoMode,
  ChainPoint (ChainPoint),
  Hash,
  ScriptData,
  SlotNo,
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Streaming (
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
  withChainSyncEventStream,
 )
import Cardano.Streaming.Helpers (bimSlotNo)
import Control.Concurrent (
  MVar,
  forkIO,
  isEmptyMVar,
  modifyMVar,
  newEmptyMVar,
  newMVar,
  readMVar,
  tryPutMVar,
  tryReadMVar,
 )
import Control.Concurrent.QSemN (
  QSemN,
  newQSemN,
  signalQSemN,
  waitQSemN,
 )
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  readTChan,
  writeTChan,
 )
import Control.Exception (Exception, catch, finally)
import Control.Exception.Base (throw)
import Control.Lens (makeLenses, view)
import Control.Lens.Operators ((%~), (&), (+~), (.~), (^.))
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (
  ExceptT,
  runExceptT,
 )
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Word (Word64)
import Marconi.ChainIndex.Error (IndexerError (CantRollback, CantStartIndexer))
import Marconi.ChainIndex.Indexers.AddressDatum (
  AddressDatumDepth (AddressDatumDepth),
  AddressDatumHandle,
  AddressDatumIndex,
 )
import Marconi.ChainIndex.Indexers.AddressDatum qualified as AddressDatum
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle)
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Indexers.ScriptTx qualified as ScriptTx
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Logging (chainSyncEventStreamLogging)
import Marconi.ChainIndex.Node.Client.GenesisConfig (
  NetworkConfigFile (NetworkConfigFile),
  initExtLedgerStateVar,
  mkProtocolInfoCardano,
  readCardanoGenesisConfig,
  readNetworkConfig,
  renderGenesisConfigError,
 )
import Marconi.ChainIndex.Types (
  IndexingDepth (MaxIndexingDepth, MinIndexingDepth),
  SecurityParam (SecurityParam),
  TargetAddresses,
  UtxoIndexerConfig,
 )
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Storable qualified as Storable
import Ouroboros.Consensus.Ledger.Abstract qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Node qualified as O
import Prettyprinter (
  defaultLayoutOptions,
  layoutPretty,
  pretty,
  (<+>),
 )
import Prettyprinter.Render.Text (renderStrict)
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Timeout (timeout)

-- DatumIndexer

scriptDataFromCardanoTxBody :: C.TxBody era -> Map (Hash ScriptData) ScriptData
scriptDataFromCardanoTxBody (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ dats _) _ _) =
  extractData dats
  where
    extractData :: Alonzo.TxDats era -> Map (Hash ScriptData) ScriptData
    extractData (Alonzo.TxDats' xs) =
      Map.fromList
        . fmap ((\x -> (C.hashScriptDataBytes x, C.getScriptData x)) . C.fromAlonzoData)
        . Map.elems
        $ xs
scriptDataFromCardanoTxBody _ = mempty

data Buffer a = Buffer
  { _capacity :: !Word64
  , _bufferLength :: !Word64
  , _content :: !(Seq a)
  }

newBuffer :: Word64 -> Buffer a
newBuffer capacity = Buffer capacity 0 Seq.empty

makeLenses 'Buffer

{- | The way we synchronise channel consumption is by waiting on a QSemN for each
     of the spawn indexers to finish processing the current event.

     The channel is used to transmit the next event to the listening indexers. Note
     that even if the channel is unbound it will actually only ever hold one event
     because it will be blocked until the processing of the event finishes on all
     indexers.

     The indexer count is where we save the number of running indexers so we know for
     how many we are waiting.
-}
data Coordinator' a = Coordinator
  { _channel :: !(TChan (ChainSyncEvent a))
  , _errorVar :: !(MVar IndexerError)
  , _barrier :: !QSemN
  , _indexerCount :: !Int
  , _buffer :: !(Buffer a)
  }

makeLenses 'Coordinator

type Coordinator = Coordinator' (BlockInMode CardanoMode)

initialCoordinator :: Int -> Word64 -> IO (Coordinator' a)
initialCoordinator indexerCount' minIndexingDepth =
  Coordinator
    <$> newBroadcastTChanIO
    <*> newEmptyMVar
    <*> newQSemN 0
    <*> pure indexerCount'
    <*> pure (newBuffer minIndexingDepth)

-- The points should/could provide shared access to the indexers themselves. The result
-- is a list of points (rather than just one) since it offers more resume possibilities
-- to the node (in the unlikely case there were some rollbacks during downtime).
type Worker = SecurityParam -> Coordinator -> FilePath -> IO (Storable.StorablePoint ScriptTx.ScriptTxHandle)

utxoWorker_
  :: (Utxo.UtxoIndexer -> IO ())
  -- ^ Callback function used in the queryApi thread, needs to be non-blocking
  -> Utxo.Depth
  -> UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO (), C.ChainPoint)
utxoWorker_ callback depth utxoIndexerConfig Coordinator{_barrier, _errorVar} ch path = do
  ix <- toException $ Utxo.open path depth False -- open SQLite with depth=depth and DO NOT perform SQLite vacuum
  -- TODO consider adding a CLI param to allow user to perfomr Vaccum or not.
  cp <- toException $ Storable.resumeFromStorage $ view Storable.handle ix
  mIndexer <- newMVar ix
  pure (loop mIndexer, cp)
  where
    loop :: MVar Utxo.UtxoIndexer -> IO ()
    loop index = forever $ do
      failWhenFull _errorVar
      readMVar index >>= callback
      event <- atomically . readTChan $ ch
      case event of
        RollForward (BlockInMode block _) _ct ->
          let utxoEvents = Utxo.getUtxoEventsFromBlock utxoIndexerConfig block
           in void $ updateWith index _errorVar $ Storable.insert utxoEvents
        RollBackward cp _ct ->
          void $ updateWith index _errorVar $ Storable.rewind cp
      signalQSemN _barrier 1

utxoWorker
  :: (Utxo.UtxoIndexer -> IO ())
  -- ^ callback function used in the queryApi thread
  -> UtxoIndexerConfig
  -- ^ Utxo Indexer Configuration, containing targetAddresses and showReferenceScript flag
  -> Worker
utxoWorker callback utxoIndexerConfig securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, cp) <-
    utxoWorker_
      callback
      (Utxo.Depth $ fromIntegral securityParam)
      utxoIndexerConfig
      coordinator
      workerChannel
      path
  void $ forkIO loop
  return cp

addressDatumWorker
  :: (Storable.StorableEvent AddressDatumHandle -> IO [()])
  -> Maybe TargetAddresses
  -> Worker
addressDatumWorker onInsert targetAddresses securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, cp) <-
    addressDatumWorker_
      onInsert
      targetAddresses
      (AddressDatumDepth $ fromIntegral securityParam)
      coordinator
      workerChannel
      path
  void $ forkIO loop
  return cp

addressDatumWorker_
  :: (Storable.StorableEvent AddressDatumHandle -> IO [()])
  -> Maybe TargetAddresses
  -- ^ Target addresses to filter for
  -> AddressDatumDepth
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO (), C.ChainPoint)
addressDatumWorker_ onInsert targetAddresses depth Coordinator{_barrier, _errorVar} ch path = do
  index <- toException $ AddressDatum.open path depth
  cp <- toException . Storable.resumeFromStorage . view Storable.handle $ index
  mIndex <- newMVar index
  pure (innerLoop mIndex, cp)
  where
    innerLoop :: MVar AddressDatumIndex -> IO ()
    innerLoop index = forever $ do
      failWhenFull _errorVar
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo bh _) txs) _) _ -> do
          -- TODO Redo. Inefficient filtering
          let addressDatumIndexEvent =
                AddressDatum.toAddressDatumIndexEvent (Utils.addressesToPredicate targetAddresses) txs (C.ChainPoint slotNo bh)
          void $ updateWith index _errorVar $ Storable.insert addressDatumIndexEvent
          void $ onInsert addressDatumIndexEvent
        RollBackward cp _ct -> do
          void $ updateWith index _errorVar $ Storable.rewind cp
      signalQSemN _barrier 1

-- * ScriptTx indexer

scriptTxWorker_
  :: (Storable.StorableEvent ScriptTx.ScriptTxHandle -> IO [()])
  -> ScriptTx.Depth
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO (), C.ChainPoint, MVar ScriptTx.ScriptTxIndexer)
scriptTxWorker_ onInsert depth Coordinator{_barrier, _errorVar} ch path = do
  indexer <- toException $ ScriptTx.open path depth
  cp <- toException . Storable.resumeFromStorage . view Storable.handle $ indexer
  mIndexer <- newMVar indexer
  pure (loop mIndexer, cp, mIndexer)
  where
    loop :: MVar ScriptTx.ScriptTxIndexer -> IO ()
    loop index = forever $ do
      failWhenFull _errorVar
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo hsh _) txs :: Block era) _ :: BlockInMode CardanoMode) _ct -> do
          let u = ScriptTx.toUpdate txs (ChainPoint slotNo hsh)
          void $ updateWith index _errorVar $ Storable.insert u
          void $ onInsert u
        RollBackward cp _ct -> do
          void $ updateWith index _errorVar $ Storable.rewind cp
      signalQSemN _barrier 1

scriptTxWorker
  :: (Storable.StorableEvent ScriptTx.ScriptTxHandle -> IO [()])
  -> Worker
scriptTxWorker onInsert securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, cp, _indexer) <- scriptTxWorker_ onInsert (ScriptTx.Depth $ fromIntegral securityParam) coordinator workerChannel path
  void $ forkIO loop
  return cp

-- * Epoch state indexer

epochStateWorker_
  :: FilePath
  -> (Storable.State EpochStateHandle -> IO ())
  -> SecurityParam
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO b, C.ChainPoint, MVar (Storable.State EpochStateHandle))
epochStateWorker_
  nodeConfigPath
  callback
  securityParam
  Coordinator{_barrier, _errorVar}
  ch
  dbPath = do
    nodeConfigE <- runExceptT $ readNetworkConfig (NetworkConfigFile nodeConfigPath)
    nodeConfig <- either (throw . CantStartIndexer . Text.pack . show) pure nodeConfigE
    genesisConfigE <- runExceptT $ readCardanoGenesisConfig nodeConfig
    genesisConfig <- either (throw . CantStartIndexer . Text.pack . show . renderGenesisConfigError) pure genesisConfigE

    let initialLedgerState = initExtLedgerStateVar genesisConfig
        topLevelConfig = O.pInfoConfig (mkProtocolInfoCardano genesisConfig)
        hfLedgerConfig = O.ExtLedgerCfg topLevelConfig

    let ledgerStateDir = takeDirectory dbPath </> "ledgerStates"
    createDirectoryIfMissing False ledgerStateDir
    indexer <- toException $ EpochState.open topLevelConfig dbPath ledgerStateDir securityParam

    cp <- toException $ Storable.resumeFromStorage $ view Storable.handle indexer
    indexerMVar <- newMVar indexer

    let loop currentLedgerState currentEpochNo = do
          failWhenFull _errorVar
          void $ readMVar indexerMVar >>= callback
          chainSyncEvent <- atomically $ readTChan ch

          (newLedgerState, newEpochNo) <- case chainSyncEvent of
            RollForward blockInMode@(C.BlockInMode (C.Block (C.BlockHeader slotNo bh bn) _) _) chainTip -> do
              let newLedgerState' =
                    O.lrResult $
                      O.tickThenReapplyLedgerResult
                        hfLedgerConfig
                        (C.toConsensusBlock blockInMode)
                        currentLedgerState
                  newEpochNo = EpochState.getEpochNo newLedgerState'

              -- If the block is rollbackable, we always store the LedgerState. If the block is
              -- immutable, we only store it right at the beginning of a new epoch.
              let isFirstEventOfEpoch = newEpochNo > currentEpochNo
              let storableEvent =
                    EpochState.toStorableEvent
                      newLedgerState'
                      slotNo
                      bh
                      bn
                      chainTip
                      securityParam
                      isFirstEventOfEpoch

              void $ updateWith indexerMVar _errorVar $ Storable.insert storableEvent

              -- Compute new LedgerState given block and old LedgerState
              pure (newLedgerState', newEpochNo)
            RollBackward C.ChainPointAtGenesis _ct -> do
              void $ updateWith indexerMVar _errorVar $ Storable.rewind C.ChainPointAtGenesis
              pure (initialLedgerState, Nothing)
            RollBackward cp' _ct -> do
              newIndex <- updateWith indexerMVar _errorVar $ Storable.rewind cp'

              -- We query the LedgerState from disk at the point where we need to rollback to.
              -- For that to work, we need to be sure that any volatile LedgerState are stored
              -- on disk. For immutable LedgerStates, they are only stored on disk at the first
              -- slot of an epoch.
              maybeLedgerState <-
                runExceptT $ Storable.query newIndex (EpochState.LedgerStateAtPointQuery cp')
              case maybeLedgerState of
                Right (EpochState.LedgerStateAtPointResult (Just ledgerState)) -> pure (ledgerState, EpochState.getEpochNo ledgerState)
                Right (EpochState.LedgerStateAtPointResult Nothing) -> do
                  void $
                    tryPutMVar _errorVar $
                      CantRollback "Could not find LedgerState from which to rollback from in EpochState indexer. Should not happen!"
                  pure (initialLedgerState, Nothing)
                Right _ -> do
                  void $
                    tryPutMVar _errorVar $
                      CantRollback "LedgerStateAtPointQuery returned a result mismatch when applying a rollback. Should not happen!"
                  pure (initialLedgerState, Nothing)
                Left err -> do
                  void $ tryPutMVar _errorVar err
                  pure (initialLedgerState, Nothing)

          signalQSemN _barrier 1
          loop newLedgerState newEpochNo

    pure (loop initialLedgerState (EpochState.getEpochNo initialLedgerState), cp, indexerMVar)

epochStateWorker
  :: FilePath
  -> (Storable.State EpochStateHandle -> IO ())
  -> Worker
epochStateWorker nodeConfigPath callback securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, cp, _indexer) <-
    epochStateWorker_
      nodeConfigPath
      callback
      securityParam
      coordinator
      workerChannel
      path
  void $ forkIO loop
  return cp

-- * Mint/burn indexer

mintBurnWorker_
  :: SecurityParam
  -> (MintBurn.MintBurnIndexer -> IO ())
  -> Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -- ^ Target assets to filter for
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO b, C.ChainPoint)
mintBurnWorker_ securityParam callback mAssets c ch dbPath = do
  indexer <- toException (MintBurn.open dbPath securityParam)
  indexerMVar <- newMVar indexer
  cp <- toException $ Storable.resumeFromStorage $ view Storable.handle indexer
  let loop = forever $ do
        failWhenFull (c ^. errorVar)
        void $ readMVar indexerMVar >>= callback
        event <- atomically $ readTChan ch
        case event of
          RollForward blockInMode _ct -> do
            let event' = MintBurn.toUpdate mAssets blockInMode
            void $
              updateWith indexerMVar (c ^. errorVar) $
                Storable.insert $
                  MintBurn.MintBurnEvent event'
          RollBackward cp' _ct ->
            void $ updateWith indexerMVar (c ^. errorVar) $ Storable.rewind cp'
        signalQSemN (c ^. barrier) 1
  pure (loop, cp)

mintBurnWorker
  :: (MintBurn.MintBurnIndexer -> IO ())
  -> Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -- ^ Target assets to filter for
  -> Worker
mintBurnWorker callback mAssets securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, cp) <- mintBurnWorker_ securityParam callback mAssets coordinator workerChannel path
  void $ forkIO loop
  return cp

initializeIndexers
  :: SecurityParam
  -> IndexingDepth
  -> [(Worker, FilePath)]
  -> IO (ChainPoint, Coordinator)
initializeIndexers securityParam@(SecurityParam sec) indexingDepth indexers = do
  let resolvedDepth = case indexingDepth of
        MinIndexingDepth w -> w
        MaxIndexingDepth -> sec
  when (resolvedDepth > sec) $
    fail "Indexing depth is greater than security param"
  coordinator <- initialCoordinator (length indexers) resolvedDepth
  startingPoints <- mapM (\(ix, fp) -> ix securityParam coordinator fp) indexers
  -- We want to use the set of points that are common to all indexers
  -- giving priority to recent ones.
  let oldestStartingPoint = minimum startingPoints
  pure
    ( oldestStartingPoint
    , coordinator
    )

cleanExit :: Coordinator' a -> IO ()
cleanExit c = do
  putStrLn ""
  putStrLn "Marconi is shutting down."
  putStrLn "Waiting for indexers to finish their work..."

  void $ timeout 180_000_000 $ waitQSemN (c ^. barrier) (c ^. indexerCount)
  putStrLn "Done."

mkIndexerStream'
  :: (a -> SlotNo)
  -> Coordinator' a
  -> S.Stream (S.Of (ChainSyncEvent a)) IO r
  -> IO ()
mkIndexerStream' f coordinator =
  S.foldM_ step initial finish
  where
    initial = pure coordinator

    indexersHealthCheck :: Coordinator' a -> IO ()
    indexersHealthCheck c = do
      err <- tryReadMVar (c ^. errorVar)
      forM_ err throw

    blockAfterChainPoint C.ChainPointAtGenesis _bim = True
    blockAfterChainPoint (C.ChainPoint slotNo' _) bim = f bim > slotNo'

    bufferIsFull c = c ^. buffer . bufferLength >= c ^. buffer . capacity

    coordinatorHandleEvent c (RollForward bim ct)
      | bufferIsFull c = case c ^. buffer . content of
          buff Seq.:|> event' -> do
            let c' = c & buffer . content .~ (bim Seq.:<| buff)
            pure (c', Just $ RollForward event' ct)
          Seq.Empty -> do
            pure (c, Just $ RollForward bim ct)
      | otherwise = do
          let c' =
                c
                  & buffer . content %~ (bim Seq.:<|)
                  & buffer . bufferLength +~ 1
          pure (c', Nothing)
    coordinatorHandleEvent c e@(RollBackward cp _) = case c ^. buffer . content of
      buff@(_ Seq.:|> event') ->
        let content' = Seq.dropWhileL (blockAfterChainPoint cp) buff
            c' =
              c
                & buffer . content .~ content'
                & buffer . bufferLength .~ fromIntegral (length content')
         in if blockAfterChainPoint cp event'
              then pure (c', Just e)
              else pure (c', Nothing)
      Seq.Empty -> pure (c, Just e)

    step c@Coordinator{_barrier, _errorVar, _indexerCount, _channel} event = do
      indexersHealthCheck c
      (c', mevent) <- coordinatorHandleEvent c event
      case mevent of
        Nothing -> signalQSemN _barrier _indexerCount
        Just event' -> atomically $ writeTChan _channel event'
      waitQSemN _barrier _indexerCount
      pure c'

    finish _ = pure ()

mkIndexerStream
  :: Coordinator
  -> S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
  -> IO ()
mkIndexerStream = mkIndexerStream' bimSlotNo

runIndexers
  :: FilePath
  -> C.NetworkId
  -> ChainPoint
  -> IndexingDepth
  -> Text.Text
  -> [(Worker, Maybe FilePath)]
  -> IO ()
runIndexers socketPath networkId cliChainPoint indexingDepth traceName list = do
  securityParam <- toException $ Utils.querySecurityParam networkId socketPath
  (oldestCommonChainPoint, coordinator) <- initializeIndexers securityParam indexingDepth $ mapMaybe sequenceA list
  let chainPoint = case cliChainPoint of
        C.ChainPointAtGenesis -> oldestCommonChainPoint -- User didn't specify a chain point, use oldest common chain point,
        cliCp -> cliCp -- otherwise use what was provided on CLI.
  c <- defaultConfigStdout
  withTrace c traceName $ \trace ->
    let io = withChainSyncEventStream socketPath networkId [chainPoint] (mkIndexerStream coordinator . chainSyncEventStreamLogging trace)
        handleException NoIntersectionFound =
          logError trace $
            renderStrict $
              layoutPretty defaultLayoutOptions $
                "No intersection found when looking for the chain point"
                  <+> pretty chainPoint <> "."
                  <+> "Please check the slot number and the block hash do belong to the chain"
     in finally (io `catch` handleException) (cleanExit coordinator)

toException :: Exception err => ExceptT err IO a -> IO a
toException mx = do
  x <- runExceptT mx
  case x of
    Left err -> throw err
    Right res -> pure res

updateWith
  :: MVar a
  -> MVar err
  -> (a -> ExceptT err IO a)
  -> IO a
updateWith xBox errBox f = modifyMVar xBox $ \x -> do
  res <- runExceptT $ f x
  case res of
    Left err -> do
      tryPutMVar errBox err $> (x, x)
    Right x' -> pure (x', x')

failWhenFull :: Show a => MonadIO m => MVar a -> m ()
failWhenFull x = do
  isEmpty <- liftIO $ isEmptyMVar x
  if isEmpty
    then pure ()
    else do
      err <- liftIO $ readMVar x
      error $ "an indexer raised an error: " <> show err
