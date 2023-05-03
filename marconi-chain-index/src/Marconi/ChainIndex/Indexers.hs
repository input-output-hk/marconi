{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Marconi.ChainIndex.Indexers where

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint, ChainPointAtGenesis), Hash, ScriptData, SlotNo, Tx (Tx), chainPointToSlotNo)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Ledger.Alonzo.TxWits qualified as Alonzo
import Cardano.Streaming (ChainSyncEvent (RollBackward, RollForward), ChainSyncEventException (NoIntersectionFound),
                          withChainSyncEventStream)
import Control.Concurrent (MVar, forkIO, isEmptyMVar, modifyMVar, newEmptyMVar, newMVar, readMVar, tryPutMVar,
                           tryReadMVar)
import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (Exception, catch)
import Control.Exception.Base (throw)
import Control.Lens (makeLenses, view)
import Control.Lens.Operators ((^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Functor (($>))
import Data.List (findIndex, foldl1', intersect)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as TS
import Data.Text qualified as Text
import Marconi.ChainIndex.Error (IndexerError (CantRollback, CantStartIndexer))
import Marconi.ChainIndex.Indexers.AddressDatum (AddressDatumDepth (AddressDatumDepth), AddressDatumHandle,
                                                 AddressDatumIndex)
import Marconi.ChainIndex.Indexers.AddressDatum qualified as AddressDatum
import Marconi.ChainIndex.Indexers.Datum (DatumIndex)
import Marconi.ChainIndex.Indexers.Datum qualified as Datum
import Marconi.ChainIndex.Indexers.EpochState (EpochStateHandle, EpochStateIndex)
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Indexers.ScriptTx qualified as ScriptTx
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Logging (logging)
import Marconi.ChainIndex.Node.Client.GenesisConfig (NetworkConfigFile (NetworkConfigFile), initExtLedgerStateVar,
                                                     mkProtocolInfoCardano, readCardanoGenesisConfig, readNetworkConfig,
                                                     renderGenesisConfigError)
import Marconi.ChainIndex.Types (SecurityParam, TargetAddresses)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Index.VSplit qualified as Ix
import Marconi.Core.Storable qualified as Storable
import Ouroboros.Consensus.Ledger.Abstract qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Node qualified as O
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

-- DatumIndexer
getDatums :: BlockInMode CardanoMode -> [(SlotNo, (Hash ScriptData, ScriptData))]
getDatums (BlockInMode (Block (BlockHeader slotNo _ _) txs) _) = concatMap extractDatumsFromTx txs
    where
        extractDatumsFromTx :: Tx era -> [(SlotNo, (Hash ScriptData, ScriptData))]
        extractDatumsFromTx (Tx txBody _) =
            fmap (slotNo,)
            . Map.assocs
            . scriptDataFromCardanoTxBody
            $ txBody

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

{- | The way we synchronise channel consumption is by waiting on a QSemN for each
     of the spawn indexers to finish processing the current event.

     The channel is used to transmit the next event to the listening indexers. Note
     that even if the channel is unbound it will actually only ever hold one event
     because it will be blocked until the processing of the event finishes on all
     indexers.

     The indexer count is where we save the number of running indexers so we know for
     how many we are waiting.
-}
data Coordinator = Coordinator
  { _channel      :: !(TChan (ChainSyncEvent (BlockInMode CardanoMode)))
  , _errorVar     :: !(MVar IndexerError)
  , _barrier      :: !QSemN
  , _indexerCount :: !Int
  }

makeLenses 'Coordinator

initialCoordinator :: Int -> IO Coordinator
initialCoordinator indexerCount' =
  Coordinator <$> newBroadcastTChanIO
              <*> newEmptyMVar
              <*> newQSemN 0
              <*> pure indexerCount'

-- The points should/could provide shared access to the indexers themselves. The result
-- is a list of points (rather than just one) since it offers more resume possibilities
-- to the node (in the unlikely case there were some rollbacks during downtime).
type Worker = SecurityParam -> Coordinator -> FilePath -> IO [Storable.StorablePoint ScriptTx.ScriptTxHandle]

datumWorker :: Worker
datumWorker securityParam Coordinator{_barrier, _channel, _errorVar} path = do
  ix <- Datum.open path (Datum.Depth $ fromIntegral securityParam)
  workerChannel <- atomically . dupTChan $ _channel
  void . forkIO $ innerLoop workerChannel ix
  pure [ChainPointAtGenesis]
  where
    innerLoop :: TChan (ChainSyncEvent (BlockInMode CardanoMode)) -> DatumIndex -> IO ()
    innerLoop ch index = do
      signalQSemN _barrier 1
      failWhenFull _errorVar
      event <- atomically $ readTChan ch
      case event of
        RollForward blk _ct ->
          Ix.insert (getDatums blk) index >>= innerLoop ch
        RollBackward cp _ct -> do
          events <- Ix.getEvents (index ^. Ix.storage)
          innerLoop ch $
            fromMaybe index $ do
              slot   <- chainPointToSlotNo cp
              offset <- findIndex (any (\(s, _) -> s < slot)) events
              Ix.rewind offset index

utxoWorker_
  :: (Utxo.UtxoIndexer -> IO ())  -- ^ callback function used in the queryApi thread, needs to be non-blocking
  -> Utxo.Depth
  -> Maybe TargetAddresses        -- ^ Target addresses to filter for
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO (), MVar Utxo.UtxoIndexer)
utxoWorker_ callback depth maybeTargetAddresses Coordinator{_barrier, _errorVar} ch path = do
  ix <- toException $ Utxo.open path depth False -- open SQLite with depth=depth and DO NOT perform SQLite vacuum
  -- TODO consider adding a CLI param to allow user to perfomr Vaccum or not.
  mIndexer <- newMVar ix
  pure (loop mIndexer, mIndexer)
  where
    loop :: MVar Utxo.UtxoIndexer -> IO ()
    loop index = do
      signalQSemN _barrier 1
      failWhenFull _errorVar
      readMVar index >>= callback -- refresh the query STM/CPS with new storage pointers/counters state
      event <- atomically . readTChan $ ch
      case event of
        RollForward (BlockInMode block _) _ct -> do
            let utxoEvents = Utxo.getUtxoEventsFromBlock maybeTargetAddresses block
            void $ updateWith index _errorVar $ Storable.insert utxoEvents
            loop index

        RollBackward cp _ct -> do
            void $ updateWith index _errorVar $ Storable.rewind cp

      loop index

utxoWorker
  :: (Utxo.UtxoIndexer -> IO ())  -- ^ callback function used in the queryApi thread
  -> Maybe TargetAddresses        -- ^ Target addresses to filter for
  -> Worker
utxoWorker callback maybeTargetAddresses securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <- utxoWorker_ callback (Utxo.Depth $ fromIntegral securityParam) maybeTargetAddresses coordinator workerChannel path
  void . forkIO $ loop
  readMVar ix >>= toException . Storable.resumeFromStorage . view Storable.handle

addressDatumWorker
  :: (Storable.StorableEvent AddressDatumHandle -> IO [()])
  -> Maybe TargetAddresses
  -> Worker
addressDatumWorker onInsert targetAddresses securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <-
      addressDatumWorker_
        onInsert
        targetAddresses
        (AddressDatumDepth $ fromIntegral securityParam)
        coordinator
        workerChannel
        path
  void . forkIO $ loop
  readMVar ix >>= toException . Storable.resumeFromStorage . view Storable.handle

addressDatumWorker_
    :: (Storable.StorableEvent AddressDatumHandle -> IO [()])
    -> Maybe TargetAddresses  -- ^ Target addresses to filter for
    -> AddressDatumDepth
    -> Coordinator
    -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
    -> FilePath
    -> IO (IO (), MVar AddressDatumIndex)
addressDatumWorker_ onInsert targetAddresses depth Coordinator{_barrier, _errorVar} ch path = do
    index <- toException $ AddressDatum.open path depth
    mIndex <- newMVar index
    pure (innerLoop mIndex, mIndex)
  where
    innerLoop :: MVar AddressDatumIndex -> IO ()
    innerLoop index = do
      signalQSemN _barrier 1
      failWhenFull _errorVar
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo bh _) txs) _) _ -> do
            -- TODO Redo. Inefficient filtering
            let addressFilter =
                    fmap (flip elem)
                         targetAddresses
                addressDatumIndexEvent =
                    AddressDatum.toAddressDatumIndexEvent addressFilter txs (C.ChainPoint slotNo bh)
            void $ updateWith index _errorVar $ Storable.insert addressDatumIndexEvent
            void $ onInsert addressDatumIndexEvent
            innerLoop index
        RollBackward cp _ct -> do
          void $ updateWith index _errorVar $ Storable.rewind cp
          innerLoop index

-- * ScriptTx indexer

scriptTxWorker_
  :: (Storable.StorableEvent ScriptTx.ScriptTxHandle -> IO [()])
  -> ScriptTx.Depth
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO (), MVar ScriptTx.ScriptTxIndexer)
scriptTxWorker_ onInsert depth Coordinator{_barrier, _errorVar} ch path = do
  indexer <- toException $ ScriptTx.open path depth
  mIndexer <- newMVar indexer
  pure (loop mIndexer, mIndexer)
  where
    loop :: MVar ScriptTx.ScriptTxIndexer -> IO ()
    loop index = do
      signalQSemN _barrier 1
      failWhenFull _errorVar
      event <- atomically $ readTChan ch
      case event of
        RollForward (BlockInMode (Block (BlockHeader slotNo hsh _) txs :: Block era) _ :: BlockInMode CardanoMode) _ct -> do
          let u = ScriptTx.toUpdate txs (ChainPoint slotNo hsh)
          void $ updateWith index _errorVar $ Storable.insert u
          void $ onInsert u
          loop index

        RollBackward cp _ct -> do
          void $ updateWith index _errorVar $ Storable.rewind cp
          loop index

scriptTxWorker
  :: (Storable.StorableEvent ScriptTx.ScriptTxHandle -> IO [()])
  -> Worker
scriptTxWorker onInsert securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <- scriptTxWorker_ onInsert (ScriptTx.Depth $ fromIntegral securityParam) coordinator workerChannel path
  void . forkIO $ loop
  readMVar ix >>= toException . Storable.resumeFromStorage . view Storable.handle

-- * Epoch state indexer

epochStateWorker_
  :: FilePath
  -> ((Storable.State EpochStateHandle, Storable.StorableEvent EpochStateHandle) -> IO ())
  -> SecurityParam
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO b, MVar EpochStateIndex)
epochStateWorker_
        nodeConfigPath
        onInsert
        securityParam
        Coordinator{_barrier, _errorVar}
        ch
        dbPath = do
    nodeConfigE <- runExceptT $ readNetworkConfig (NetworkConfigFile nodeConfigPath)
    nodeConfig <- either (throw . CantStartIndexer . Text.pack . show) pure nodeConfigE
    genesisConfigE <- runExceptT $ readCardanoGenesisConfig nodeConfig
    genesisConfig <- either (throw . CantStartIndexer . Text.pack .  show . renderGenesisConfigError) pure genesisConfigE

    let initialLedgerState = initExtLedgerStateVar genesisConfig
        topLevelConfig = O.pInfoConfig (mkProtocolInfoCardano genesisConfig)
        hfLedgerConfig = O.ExtLedgerCfg topLevelConfig

    let ledgerStateDir = takeDirectory dbPath </> "ledgerStates"
    createDirectoryIfMissing False ledgerStateDir
    indexer <- toException $ EpochState.open topLevelConfig dbPath ledgerStateDir securityParam
    indexerMVar <- newMVar indexer

    let loop currentLedgerState currentEpochNo previousEpochNo = do
            signalQSemN _barrier 1
            failWhenFull _errorVar
            chainSyncEvent <- atomically $ readTChan ch

            newLedgerState <- case chainSyncEvent of
              RollForward blockInMode@(C.BlockInMode (C.Block (C.BlockHeader slotNo bh bn) _) _) chainTip -> do
                  -- If the block is rollbackable, we always store the LedgerState. If the block is
                  -- immutable, we only store it right at the beginning of a new epoch.
                  let isFirstEventOfEpoch = currentEpochNo > previousEpochNo
                  let storableEvent =
                          EpochState.toStorableEvent
                            currentLedgerState
                            slotNo
                            bh
                            bn
                            chainTip
                            securityParam
                            isFirstEventOfEpoch
                  newIndexer <- updateWith indexerMVar _errorVar $ Storable.insert storableEvent
                  onInsert (newIndexer, storableEvent)

                  -- Compute new LedgerState given block and old LedgerState
                  pure $ O.lrResult
                       $ O.tickThenReapplyLedgerResult
                            hfLedgerConfig
                            (C.toConsensusBlock blockInMode)
                            currentLedgerState

              RollBackward C.ChainPointAtGenesis _ct -> do
                  void $ updateWith indexerMVar _errorVar $ Storable.rewind C.ChainPointAtGenesis
                  pure initialLedgerState
              RollBackward cp _ct -> do
                  newIndex <- updateWith indexerMVar _errorVar $ Storable.rewind cp
                  -- We query the LedgerState from disk at the point where we need to rollback to.
                  -- For that to work, we need to be sure that any volatile LedgerState are stored
                  -- on disk. For immutable LedgerStates, they are only stored on disk at the first
                  -- slot of an epoch.
                  maybeLedgerState <-
                         runExceptT $ Storable.query Storable.QEverything newIndex (EpochState.LedgerStateAtPointQuery cp)
                  case maybeLedgerState of
                    Right (EpochState.LedgerStateAtPointResult (Just ledgerState)) -> pure ledgerState
                    Right _ -> do
                        void $ tryPutMVar _errorVar
                            $ CantRollback "Could not find LedgerState from which to rollback from in EpochState indexer. Should not happen!"
                        pure initialLedgerState
                    Left err         -> do
                        void $ tryPutMVar _errorVar err
                        pure initialLedgerState

            loop newLedgerState (EpochState.getEpochNo newLedgerState) currentEpochNo

    pure (loop initialLedgerState (EpochState.getEpochNo initialLedgerState) Nothing, indexerMVar)

epochStateWorker
    :: FilePath
    -> ((Storable.State EpochStateHandle, Storable.StorableEvent EpochStateHandle) -> IO ())
    -> Worker
epochStateWorker nodeConfigPath onInsert securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <-
      epochStateWorker_
        nodeConfigPath
        onInsert
        securityParam
        coordinator
        workerChannel
        path
  void $ forkIO loop
  readMVar ix >>= toException . Storable.resumeFromStorage . view Storable.handle

-- * Mint/burn indexer

mintBurnWorker_
  :: SecurityParam
  -> (MintBurn.MintBurnIndexer -> IO ())
  -> Coordinator
  -> TChan (ChainSyncEvent (BlockInMode CardanoMode))
  -> FilePath
  -> IO (IO b, MVar MintBurn.MintBurnIndexer)
mintBurnWorker_ securityParam callback Coordinator{_barrier, _errorVar} ch dbPath = do
  indexerMVar <- newMVar =<< toException (MintBurn.open dbPath securityParam)
  let
    loop = forever $ do
      signalQSemN _barrier 1
      failWhenFull _errorVar
      event <- atomically $ readTChan ch
      case event of
        RollForward blockInMode _ct
          | Just event' <- MintBurn.toUpdate blockInMode -> do
              void $ updateWith indexerMVar _errorVar $ Storable.insert $ MintBurn.MintBurnEvent event'
              void $ readMVar indexerMVar >>= callback
          | otherwise -> pure ()
        RollBackward cp _ct ->
          void $ updateWith indexerMVar _errorVar $ Storable.rewind cp
  pure (loop, indexerMVar)

mintBurnWorker :: (MintBurn.MintBurnIndexer -> IO ()) -> Worker
mintBurnWorker callback securityParam coordinator path = do
  workerChannel <- atomically . dupTChan $ _channel coordinator
  (loop, ix) <- mintBurnWorker_ securityParam callback coordinator workerChannel path
  void $ forkIO loop
  readMVar ix >>= toException . Storable.resumeFromStorage . view Storable.handle

initializeIndexers
  :: SecurityParam
  -> [(Worker, FilePath)]
  -> IO ([ChainPoint], Coordinator)
initializeIndexers securityParam indexers = do
  coordinator <- initialCoordinator $ length indexers
  startingPoints <- mapM (\(ix, fp) -> ix securityParam coordinator fp) indexers
  -- We want to use the set of points that are common to all indexers
  -- giving priority to recent ones.
  pure ( foldl1' intersect startingPoints
       , coordinator
       )

mkIndexerStream
  :: Coordinator
  -> S.Stream (S.Of (ChainSyncEvent (BlockInMode CardanoMode))) IO r
  -> IO r
mkIndexerStream = S.mapM_ . step
  where
    indexersHealthCheck :: Coordinator -> IO ()
    indexersHealthCheck Coordinator{_errorVar} = do
      err <- tryReadMVar _errorVar
      case err of
          Nothing -> pure ()
          Just e  -> throw e

    step :: Coordinator -> ChainSyncEvent (BlockInMode CardanoMode) -> IO ()
    step c@Coordinator{_barrier, _errorVar, _indexerCount, _channel} event = do
      waitQSemN _barrier _indexerCount
      indexersHealthCheck c
      atomically $ writeTChan _channel event

runIndexers
  :: FilePath
  -> C.NetworkId
  -> ChainPoint
  -> TS.Text
  -> [(Worker, Maybe FilePath)]
  -> IO ()
runIndexers socketPath networkId cliChainPoint traceName list = do
  securityParam <- toException $ Utils.querySecurityParam networkId socketPath
  (returnedCp, coordinator) <- initializeIndexers securityParam $ mapMaybe sequenceA list

  -- If the user specifies the chain point then use that,
  -- otherwise use what the indexers provide.
  let chainPoints = case cliChainPoint of
        C.ChainPointAtGenesis -> returnedCp
        cliCp                 -> [cliCp]

  c <- defaultConfigStdout
  withTrace c traceName $ \trace -> let
      io = withChainSyncEventStream socketPath networkId chainPoints (mkIndexerStream coordinator . logging trace)
      handleException NoIntersectionFound =
        logError trace $
          renderStrict $
            layoutPretty defaultLayoutOptions $
              "No intersection found when looking for the chain point"
              <+> pretty chainPoints <> "."
              <+> "Please check the slot number and the block hash do belong to the chain"
    in io `catch` handleException

toException :: Exception err => ExceptT err IO a -> IO a
toException mx = do
    x <- runExceptT mx
    case x of
        Left err  -> throw err
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

failWhenFull :: MonadIO m => MVar a -> m ()
failWhenFull x = do
  isEmpty <- liftIO $ isEmptyMVar x
  if isEmpty
  then pure ()
  else error "an indexer raised an error"
