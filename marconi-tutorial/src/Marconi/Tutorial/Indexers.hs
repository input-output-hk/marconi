{-# LANGUAGE FlexibleContexts #-}

module Marconi.Tutorial.Indexers where

import Cardano.BM.Trace (logError)
import Control.Lens (view, (^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Marconi.ChainIndex.CLI qualified as CommonCLI
import Marconi.ChainIndex.Experimental.Indexers.Worker qualified as Core
import Marconi.ChainIndex.Experimental.Runner (
  RunIndexerConfig (RunIndexerConfig),
  withDistancePreprocessor,
 )
import Marconi.ChainIndex.Experimental.Runner qualified as Runner
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Core qualified as Core
import Marconi.Tutorial.CLI qualified as CLI
import Marconi.Tutorial.Env (Env, envCliArgs, envStdoutTrace)
import Marconi.Tutorial.Env qualified as Env
import Marconi.Tutorial.Indexers.AddressCount qualified as AddressCount
import Prettyprinter qualified as Pretty
import System.Exit (exitFailure)
import System.FilePath ((</>))

buildIndexersEnv :: (MonadIO m) => SecurityParam -> CLI.Options -> m Env.IndexersEnv
buildIndexersEnv securityParam cliOptions = do
  addressCountWorker <-
    liftIO $
      AddressCount.addressCountWorker
        (CLI.optionsDbPath cliOptions </> "addresscount.db")
        securityParam
  pure $ Env.IndexersEnv $ Env.AddressCountIndexerEnv addressCountWorker

runIndexers :: ReaderT (Env ann) IO ()
runIndexers = do
  env <- ask
  cliOptions <- view envCliArgs
  stdoutTrace <- view envStdoutTrace
  securityParam <- view Env.securityParam
  let socketPath = CommonCLI.optionsSocketPath $ CLI.commonOptions cliOptions
      networkId = CommonCLI.optionsNetworkId $ CLI.commonOptions cliOptions
      retryConfig = CommonCLI.optionsRetryConfig $ CLI.commonOptions cliOptions
      preferedStartingPoint = CommonCLI.optionsChainPoint $ CLI.commonOptions cliOptions

  indexer <-
    liftIO $ Core.mkCoordinator $ fmap Core.standardWorker [env ^. Env.addressCountIndexerWorker]

  startingPoint <- getStartingPoint preferedStartingPoint indexer

  liftIO $
    Runner.runIndexer
      ( RunIndexerConfig
          stdoutTrace
          withDistancePreprocessor
          retryConfig
          securityParam
          networkId
          startingPoint
          socketPath
      )
      indexer

getStartingPoint
  :: forall event indexer m ann
   . ( Core.HasGenesis (Core.Point event)
     , Ord (Core.Point event)
     , MonadIO m
     , MonadReader (Env ann) m
     , Core.IsSync (ExceptT Core.IndexerError m) event indexer
     )
  => Core.Point event
  -> indexer event
  -> m (Core.Point event)
getStartingPoint preferredStartingPoint indexer = do
  if preferredStartingPoint == Core.genesis
    then do
      (indexerLastSyncPointE :: Either Core.IndexerError (Core.Point event)) <-
        runExceptT $ Core.lastSyncPoint indexer
      case indexerLastSyncPointE of
        Left err -> do
          stdoutTrace <- view envStdoutTrace
          liftIO $ do
            logError stdoutTrace $ Pretty.viaShow err
            exitFailure
        Right result -> pure result
    else do
      pure preferredStartingPoint
