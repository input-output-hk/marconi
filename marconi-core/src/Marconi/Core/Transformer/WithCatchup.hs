{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The catch-up mechanism buffered events to pass them in batch to an indexer, to reduce the time
 needed for an indexer to catch-up with the tip.

 Once we are close enough to the tip of the chain, `WithCatchup` deactivate itself and pass directly
 the blocks to the indexer.

 When you use several workers and coordinators, you may want to put the catchup on the final
 indexers and not on the coordinators to improve performances.
-}
module Marconi.Core.Transformer.WithCatchup (
  WithCatchup,
  withCatchup,
  CatchupConfig (CatchupConfig),
  mkCatchupConfig,
  configCatchupEventHook,
  HasCatchupConfig (..),
  CatchupEvent (Synced),
) where

import Control.Lens qualified as Lens
import Control.Lens.Operators ((%~), (+~), (.~), (?~), (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Word (Word64)
import Marconi.Core.Class (
  Closeable,
  IsIndex (index, rollback, setLastStablePoint),
  IsSync,
  Queryable,
  Resetable (reset),
 )
import Marconi.Core.Indexer.SQLiteAggregateQuery (HasDatabasePath)
import Marconi.Core.Transformer.Class (
  IndexerMapTrans (unwrapMap),
  IndexerTrans (unwrap),
 )
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
  indexAllDescendingVia,
  indexVia,
  resetVia,
  rollbackVia,
  setLastStablePointVia,
  wrappedIndexer,
  wrapperConfig,
 )
import Marconi.Core.Type (Point, Timed (Timed), point)

{- | The visible part of the catchup configuration, it allows you to configure the size of the batch
and to control when the batch mecanism stops
-}
data CatchupConfig = CatchupConfig
  { _configCatchupBatchSize :: Word64
  -- ^ Maximal number of events in one batch
  , _configCatchupBypassDistance :: Word64
  -- ^ How far from the block should we be to bypass the catchup mechanism (in number of blocks).
  , _configCatchupEventHook :: Maybe (CatchupEvent -> IO ())
  -- ^ Hook to execute when specific events are triggered.
  }

data CatchupEvent = Synced

mkCatchupConfig :: Word64 -> Word64 -> CatchupConfig
mkCatchupConfig batchSize bypassDistance = CatchupConfig batchSize bypassDistance Nothing

Lens.makeLenses ''CatchupConfig

data CatchupContext event = CatchupContext
  { _contextDistanceComputation :: Point event -> event -> Word64
  -- ^ How we compute distance to tip
  , _contextCatchupConfig :: CatchupConfig
  -- ^ How far from the block should we be to bypass the catchup mechanism (in number of blocks)
  , _contextCatchupBufferLength :: Word64
  -- ^ How many event do we have in the batch
  , _contextCatchupBuffer :: [Timed (Point event) (Maybe event)]
  -- ^ Where we store the event that must be batched
  , _contextCatchupLastStable :: Maybe (Point event)
  -- ^ The latest last stable point stored in the batch
  }

Lens.makeLenses ''CatchupContext

contextCatchupBypassDistance :: Lens.Lens' (CatchupContext event) Word64
contextCatchupBypassDistance = contextCatchupConfig . configCatchupBypassDistance

contextCatchupBatchSize :: Lens.Lens' (CatchupContext event) Word64
contextCatchupBatchSize = contextCatchupConfig . configCatchupBatchSize

contextCatchupEventHook :: Lens.Lens' (CatchupContext event) (Maybe (CatchupEvent -> IO ()))
contextCatchupEventHook = contextCatchupConfig . configCatchupEventHook

{-- | WithCatchup is used to speed up the synchronisation of indexers by preparing batches of events
 - that will be submitted via `indexAll` to the underlying indexer.
 -
 - Once the indexer is close enough to the tip, the transformer stops the batch to insert element
 - one by one.
 -}
newtype WithCatchup indexer event = WithCatchup {_catchupWrapper :: IndexTransformer CatchupContext indexer event}

Lens.makeLenses 'WithCatchup

-- | A smart constructor for 'WithCatchup'
withCatchup
  :: (Point event -> event -> Word64)
  -- ^ The distance function
  -> CatchupConfig
  -- ^ Configure how many element we put in a batch and until when we use it
  -> indexer event
  -- ^ the underlying indexer
  -> WithCatchup indexer event
withCatchup computeDistance config =
  WithCatchup . IndexTransformer (CatchupContext computeDistance config 0 [] Nothing)

deriving via
  (IndexTransformer CatchupContext indexer)
  instance
    (IsSync m event indexer) => IsSync m event (WithCatchup indexer)

deriving via
  (IndexTransformer CatchupContext indexer)
  instance
    (HasDatabasePath indexer) => HasDatabasePath (WithCatchup indexer)

deriving via
  (IndexTransformer CatchupContext indexer)
  instance
    (Closeable m indexer) => Closeable m (WithCatchup indexer)

deriving via
  (IndexTransformer CatchupContext indexer)
  instance
    (Queryable m event query indexer) => Queryable m event query (WithCatchup indexer)

caughtUpIndexer :: Lens.Lens' (WithCatchup indexer event) (indexer event)
caughtUpIndexer = catchupWrapper . wrappedIndexer

instance IndexerTrans WithCatchup where
  unwrap = caughtUpIndexer

{- | A typeclass that allows an indexer with a @WitchCatchup@ transformer to configure
 the behaviour of this transformer
-}
class HasCatchupConfig indexer where
  catchupBypassDistance :: Lens.Lens' (indexer event) Word64
  catchupBatchSize :: Lens.Lens' (indexer event) Word64
  catchupEventHook :: Lens.Lens' (indexer event) (Maybe (CatchupEvent -> IO ()))

instance {-# OVERLAPPING #-} HasCatchupConfig (WithCatchup indexer) where
  catchupBypassDistance = catchupWrapper . wrapperConfig . contextCatchupBypassDistance
  catchupBatchSize = catchupWrapper . wrapperConfig . contextCatchupBatchSize
  catchupEventHook = catchupWrapper . wrapperConfig . contextCatchupEventHook

instance
  {-# OVERLAPPABLE #-}
  (IndexerTrans t, HasCatchupConfig indexer)
  => HasCatchupConfig (t indexer)
  where
  catchupBypassDistance = unwrap . catchupBypassDistance
  catchupBatchSize = unwrap . catchupBatchSize
  catchupEventHook = unwrap . catchupEventHook

instance
  {-# OVERLAPPABLE #-}
  (IndexerMapTrans t, HasCatchupConfig indexer)
  => HasCatchupConfig (t indexer output)
  where
  catchupBypassDistance = unwrapMap . catchupBypassDistance
  catchupBatchSize = unwrapMap . catchupBatchSize
  catchupEventHook = unwrapMap . catchupEventHook

catchupDistance :: Lens.Lens' (WithCatchup indexer event) (Point event -> event -> Word64)
catchupDistance = catchupWrapper . wrapperConfig . contextDistanceComputation

catchupBuffer :: Lens.Lens' (WithCatchup indexer event) [Timed (Point event) (Maybe event)]
catchupBuffer = catchupWrapper . wrapperConfig . contextCatchupBuffer

catchupBufferLength :: Lens.Lens' (WithCatchup indexer event) Word64
catchupBufferLength = catchupWrapper . wrapperConfig . contextCatchupBufferLength

catchupLastStable :: Lens.Lens' (WithCatchup indexer event) (Maybe (Point event))
catchupLastStable = catchupWrapper . wrapperConfig . contextCatchupLastStable

resetBuffer :: WithCatchup indexer event -> WithCatchup indexer event
resetBuffer = (catchupBufferLength .~ 0) . (catchupBuffer .~ [])

instance
  (MonadIO m, IsIndex m event indexer, Ord (Point event))
  => IsIndex m event (WithCatchup indexer)
  where
  index timedEvent indexer =
    let bufferIsFull ix = (ix ^. catchupBufferLength) >= (ix ^. catchupBatchSize)
        pushEvent ix =
          ix
            & catchupBuffer %~ (timedEvent :)
            & catchupBufferLength +~ 1
        hasCaughtUp (Timed p e) =
          maybe False ((< indexer ^. catchupBypassDistance) . (indexer ^. catchupDistance) p) e
        sendBatch ix = do
          ix' <- indexAllDescendingVia caughtUpIndexer (ix ^. catchupBuffer) ix
          ix'' <- case ix' ^. catchupLastStable of
            Nothing -> pure ix'
            Just lastStable -> setLastStablePointVia caughtUpIndexer lastStable ix'
          pure $ resetBuffer ix''
     in if hasCaughtUp timedEvent
          then do
            maybe (pure ()) (\f -> liftIO $ f Synced) $ indexer ^. catchupEventHook
            indexer' <-
              if null (indexer ^. catchupBuffer)
                then pure indexer
                else sendBatch indexer
            indexVia caughtUpIndexer timedEvent indexer'
          else do
            let indexer' = pushEvent indexer
            if bufferIsFull indexer'
              then sendBatch indexer'
              else pure indexer'

  rollback p indexer =
    let updateBuffer ix = ix & catchupBuffer %~ dropWhile ((> p) . Lens.view point)
        setBufferSize ix = ix & catchupBufferLength .~ (fromIntegral $ length $ ix ^. catchupBuffer)
        indexer' = setBufferSize $ updateBuffer indexer
     in if indexer' ^. catchupBufferLength == 0
          then rollbackVia caughtUpIndexer p indexer'
          else pure indexer'

  setLastStablePoint p ix =
    if null $ ix ^. catchupBuffer
      then -- on an empty buffer, we just forward the update
        setLastStablePointVia caughtUpIndexer p ix
      else pure $ ix & catchupLastStable ?~ p

instance
  (Applicative m, Resetable m event indexer)
  => Resetable m event (WithCatchup indexer)
  where
  reset indexer = do
    indexer' <- resetVia caughtUpIndexer indexer
    pure $ resetBuffer indexer'
