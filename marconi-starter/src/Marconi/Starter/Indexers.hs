{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Marconi.Starter.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Trace (logError)
import Control.Lens (view, (^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Marconi.Cardano.Core.Indexer.Worker qualified as Core
import Marconi.Cardano.Core.Runner (
  RunIndexerConfig (RunIndexerConfig),
  withDistancePreprocessor,
 )
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Core.Types (SecurityParam)
import Marconi.ChainIndex.CLI qualified as CommonCLI
import Marconi.Core qualified as Core
import Marconi.Starter.CLI qualified as CLI
import Marconi.Starter.Env (Env, envCliArgs, envStdoutTrace)
import Marconi.Starter.Env qualified as Env
import Marconi.Starter.Indexers.AddressCount qualified as AddressCount
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

runIndexers :: ReaderT Env IO ()
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
      undefined

getStartingPoint
  :: forall event indexer m
   . ( MonadIO m
     , MonadReader Env m
     , Core.IsSync (ExceptT Core.IndexerError m) event indexer
     , Core.Point event ~ C.ChainPoint
     )
  => CommonCLI.StartingPoint
  -> indexer event
  -> m C.ChainPoint
getStartingPoint preferredStartingPoint indexer = do
  case preferredStartingPoint of
    CommonCLI.StartFromGenesis -> pure Core.genesis
    CommonCLI.StartFrom cp -> pure cp
    CommonCLI.StartFromLastSyncPoint -> do
      (indexerLastSyncPointE :: Either Core.IndexerError (Core.Point event)) <-
        runExceptT $ Core.lastSyncPoint indexer
      case indexerLastSyncPointE of
        Left err -> do
          stdoutTrace <- view envStdoutTrace
          liftIO $ do
            logError stdoutTrace $ Pretty.viaShow err
            exitFailure
        Right result -> pure result
