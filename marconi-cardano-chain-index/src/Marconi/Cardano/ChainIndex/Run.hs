{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.Cardano.ChainIndex.Run where

import Cardano.Api qualified as C
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Trace (logError, logInfo)
import Cardano.BM.Tracing qualified as BM
import Control.Concurrent.Async (race_)
import Control.Exception (finally)
import Control.Monad (guard, unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (toJSON)
import Data.List.NonEmpty qualified as NEList
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import Data.Void (Void)
import Marconi.Cardano.ChainIndex.Api.HttpServer (
  runHttpServer,
 )
import Marconi.Cardano.ChainIndex.Api.Types (
  HttpServerConfig (HttpServerConfig),
 )
import Marconi.Cardano.ChainIndex.CLI qualified as Cli
import Marconi.Cardano.ChainIndex.Indexers (buildIndexers)
import Marconi.Cardano.ChainIndex.Utils qualified as Utils
import Marconi.Cardano.Core.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.Cardano.Core.Node.Client.Retry (withNodeConnectRetry)
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Core.Types (SecurityParam (SecurityParam), TargetAddresses)
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Core qualified as Core
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pShowDarkBg)

-- See note 4e8b9e02-fae4-448b-8b32-1eee50dd95ab
#ifndef mingw32_HOST_OS
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (
  newEmptyMVar,
  takeMVar,
  tryPutMVar,
 )
import Data.Functor (void)
import System.Posix.Signals (
  Handler (CatchOnce),
  installHandler,
  sigTERM,
 )
import qualified Marconi.Cardano.Indexers.ExtLedgerStateCoordinator as ExtLedgerState
import qualified Marconi.Cardano.Core.Extract.WithDistance as Distance
import qualified Marconi.Cardano.Indexers.EpochSDD as EpochSDD
import qualified Marconi.Cardano.Indexers.EpochNonce as EpochNonce
import qualified Marconi.Cardano.ChainIndex.Indexers as Indexers
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (ExtLedgerStateEvent(extLedgerState))
#endif

run :: Text -> IO ()
run appName = withGracefulTermination_ $ do
  o <- Cli.parseOptions

  let logLevel =
        if Cli.debugMode $ Cli.commonOptions o
          then BM.Debug
          else BM.Info
  (trace, sb) <- defaultStdOutLogger appName logLevel

  let marconiTrace = mkMarconiTrace trace

  logInfo trace $ appName <> "-" <> Text.pack Cli.getVersion
  logInfo trace $ Text.toStrict . pShowDarkBg $ o

  createDirectoryIfMissing True (Cli.optionsDbPath o)

  let batchSize = Cli.batchSizeConfig $ Cli.commonOptions o
      epochStateSnapshotInterval = 100000
      filteredAddresses = shelleyAddressesToAddressAny $ Cli.optionsTargetAddresses o
      filteredAssetIds = Cli.optionsTargetAssets o
      includeScript = not $ Cli.optionsDisableScript o
      socketPath = Cli.optionsSocketPath $ Cli.commonOptions o
      networkId = Cli.optionsNetworkId $ Cli.commonOptions o
      retryConfig = Cli.optionsRetryConfig $ Cli.commonOptions o
      preferredStartingPoint = Cli.optionsChainPoint $ Cli.commonOptions o

  {-
      Logging in 'Cardano.BM' works by putting items on a switchboard queue and processing them
      asynchronously.

      In order to ensure that we always get errors written to the console, we need to wait for the
      queue to complete before exiting. We can use the 'shutdown' function to guarantee this.

      'shutdown' puts a kill-pill on the switchboard's queue, then waits for the switchboard's
      dispatcher to exit, which requires the queue to be drained, ensuring all messages are
      processed.
  -}
  let withLogFullError :: IO a -> Text -> IO a
      withLogFullError action msg = do
        logError trace msg
        BM.shutdown sb
        action

  nodeConfigPath <- case Cli.optionsNodeConfigPath o of
    Just cfg -> do
      exists <- doesFileExist cfg
      unless exists $
        withLogFullError exitFailure $
          Text.pack $
            "Config file does not exist at the provided path: " <> cfg
      pure cfg
    Nothing -> withLogFullError exitFailure "No node config path provided"

  securityParam <-
    withNodeConnectRetry marconiTrace retryConfig socketPath $
      Utils.toException $
        Utils.querySecurityParam @Void networkId socketPath

  let SecurityParam stopCatchupDistance = securityParam
      extLedgerStateAsEvent previousLedgerStateEvent ledgerStateEvent _blockEvent = do
        previousEpochNo <- ExtLedgerState.getEpochNo $ extLedgerState previousLedgerStateEvent
        epochNo <- ExtLedgerState.getEpochNo $ extLedgerState ledgerStateEvent
        guard $ epochNo /= previousEpochNo
        let sdd = EpochSDD.getEpochSDD ledgerStateEvent
            nonce = EpochNonce.getEpochNonce ledgerStateEvent
        pure $ Indexers.EpochEvent epochNo sdd nonce

  mindexers <-
    runExceptT $
      buildIndexers
        securityParam
        (Core.mkCatchupConfig batchSize stopCatchupDistance)
        (Utxo.UtxoIndexerConfig filteredAddresses includeScript)
        (MintTokenEvent.MintTokenEventConfig filteredAssetIds)
        ( ExtLedgerState.ExtLedgerStateWorkerConfig
            Distance.getEvent
            Distance.chainDistance
            nodeConfigPath
            epochStateSnapshotInterval
            securityParam
            extLedgerStateAsEvent
        )
        trace
        marconiTrace
        (Cli.optionsDbPath o)
  (indexerLastStablePoint, queryables, coordinator) <-
    ( case mindexers of
        Left err -> withLogFullError exitFailure $ Text.pack $ show err
        Right result -> pure result
      )

  let startingPoint = getStartingPoint preferredStartingPoint indexerLastStablePoint

  logInfo trace $ appName <> "-" <> Text.pack Cli.getVersion

  let runIndexer' =
        Runner.runIndexerOnChainSync
          ( Runner.RunIndexerConfig
              marconiTrace
              Runner.withDistanceAndTipPreprocessor
              retryConfig
              securityParam
              networkId
              startingPoint
              socketPath
          )
          coordinator
      runHttpServer' =
        runReaderT runHttpServer $
          HttpServerConfig
            trace
            (Cli.optionsRpcPort o)
            securityParam
            filteredAddresses
            (toJSON o)
            queryables

  race_
    runIndexer'
    runHttpServer'
    `finally` BM.shutdown sb

shelleyAddressesToAddressAny :: Maybe TargetAddresses -> [C.AddressAny]
shelleyAddressesToAddressAny Nothing = []
shelleyAddressesToAddressAny (Just targetAddresses) =
  fmap C.AddressShelley $ NEList.toList $ NESet.toList targetAddresses

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

getStartingPoint :: Cli.StartingPoint -> C.ChainPoint -> C.ChainPoint
getStartingPoint preferredStartingPoint indexerLastSyncPoint =
  case preferredStartingPoint of
    Cli.StartFromGenesis -> C.ChainPointAtGenesis
    Cli.StartFromLastSyncPoint -> indexerLastSyncPoint
    Cli.StartFrom cp -> cp

{- Note 4e8b9e02-fae4-448b-8b32-1eee50dd95ab:

  In order to ensure we can gracefully exit on a SIGTERM, we need the below functions. However,
  this code is not necessary on Windows, and the `unix` package (which it depends upon) is not
  supported by Windows. As such, in order to be able to cross-compile, the following `if` is
  unfortunately required. -}
#ifndef mingw32_HOST_OS
{- | Ensure that @SIGTERM@ is handled gracefully, because it's how containers are stopped.

 @action@ will receive an 'AsyncCancelled' exception if @SIGTERM@ is received by the process.

 Typical use:

 > main :: IO ()
 > main = withGracefulTermination_ $ do

 Note that although the Haskell runtime handles @SIGINT@ it doesn't do anything with @SIGTERM@.
 Therefore, when running as PID 1 in a container, @SIGTERM@ will be ignored unless a handler is
 installed for it.
-}
withGracefulTermination :: IO a -> IO (Maybe a)
withGracefulTermination action = do
  var <- newEmptyMVar
  let terminate = void $ tryPutMVar var ()
      waitForTermination = takeMVar var
  void $ installHandler sigTERM (CatchOnce terminate) Nothing
  either (const Nothing) Just <$> race waitForTermination action

-- | Like 'withGracefulTermination' but ignoring the return value
withGracefulTermination_ :: IO a -> IO ()
withGracefulTermination_ = void . withGracefulTermination
#else
withGracefulTermination_ :: a -> a
withGracefulTermination_ = id
#endif
