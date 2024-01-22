{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The catch-up mechanism buffered events to pass them in batch to an indexer, to reduce the time
 needed for an indexer to catch-up with the tip.

 Once we are close enough to the tip of the chain, `WithCatchup` deactivate itself and pass directly
 the blocks to the indexer.

 When you use several workers and coordinators, you may want to put the catchup on the final
 indexers and not on the coordinators to improve performances

 Similarly, if you want to filter the events before sending them to the indexer
 (for example, using 'WithTransform'), you should do it after the catchup, to  ensure that
 each event is properly handle by the catchup.
 If you don't do it, batch can be send very late to the indexer and it would impact resuming.
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
import Control.Lens.Operators ((%~), (+~), (.~), (^.), (^?))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
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

data Catchingup event = Catchingup
  { _catchingupDistanceComputation :: Point event -> event -> Word64
  -- ^ How we compute distance to tip
  , _catchingupConfig :: CatchupConfig
  -- ^ How far from the block should we be to bypass the catchup mechanism (in number of blocks)
  , _catchingupConfigBufferLength :: Word64
  -- ^ How many event do we have in the batch
  , _catchingupConfigBuffer :: [Timed (Point event) (Maybe event)]
  -- ^ Where we store the event that must be batched
  , _catchingupConfigLastStable :: Maybe (Point event)
  -- ^ The latest last stable point stored in the batch
  }

Lens.makeLenses ''Catchingup

data CatchupContext event = Ongoing (Catchingup event) | Done

Lens.makePrisms ''CatchupContext

contextCatchupBypassDistance :: Lens.Lens' (Catchingup event) Word64
contextCatchupBypassDistance = catchingupConfig . configCatchupBypassDistance

contextCatchupBatchSize :: Lens.Lens' (Catchingup event) Word64
contextCatchupBatchSize = catchingupConfig . configCatchupBatchSize

contextCatchupEventHook :: Lens.Lens' (Catchingup event) (Maybe (CatchupEvent -> IO ()))
contextCatchupEventHook = catchingupConfig . configCatchupEventHook

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
withCatchup computeDistance cfg =
  WithCatchup . IndexTransformer (Ongoing $ Catchingup computeDistance cfg 0 [] Nothing)

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
  catchupBypassDistance :: Lens.Traversal' (indexer event) Word64
  catchupBatchSize :: Lens.Traversal' (indexer event) Word64
  catchupEventHook :: Lens.Traversal' (indexer event) (CatchupEvent -> IO ())

instance {-# OVERLAPPING #-} HasCatchupConfig (WithCatchup indexer) where
  catchupBypassDistance = catchupWrapper . wrapperConfig . _Ongoing . contextCatchupBypassDistance
  catchupBatchSize = catchupWrapper . wrapperConfig . _Ongoing . contextCatchupBatchSize
  catchupEventHook = catchupWrapper . wrapperConfig . _Ongoing . contextCatchupEventHook . traverse

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

config :: Lens.Lens' (WithCatchup indexer event) (CatchupContext event)
config = catchupWrapper . wrapperConfig

catchupBuffer :: Lens.Traversal' (WithCatchup indexer event) [Timed (Point event) (Maybe event)]
catchupBuffer = config . _Ongoing . catchingupConfigBuffer

catchupBufferLength :: Lens.Traversal' (WithCatchup indexer event) Word64
catchupBufferLength = config . _Ongoing . catchingupConfigBufferLength

catchupLastStable :: Lens.Traversal' (WithCatchup indexer event) (Point event)
catchupLastStable = config . _Ongoing . catchingupConfigLastStable . traverse

resetBuffer :: WithCatchup indexer event -> WithCatchup indexer event
resetBuffer = (catchupBufferLength .~ 0) . (catchupBuffer .~ [])

instance
  (MonadIO m, IsIndex m event indexer, Ord (Point event))
  => IsIndex m event (WithCatchup indexer)
  where
  index timedEvent@(Timed p e) indexer = case indexer ^. config of
    Done -> indexVia caughtUpIndexer timedEvent indexer
    Ongoing cfg -> do
      let batchSize ix = ix ^? catchupBatchSize
          bufferLength ix = ix ^? catchupBufferLength
          bufferIsFull ix = fromMaybe False $ do
            l <- bufferLength ix
            s <- batchSize ix
            pure $ l >= s
          bypassDistance = cfg ^. catchingupConfig . configCatchupBypassDistance
          pushEvent ix =
            ix
              & catchupBuffer %~ (timedEvent :)
              & catchupBufferLength +~ 1
          hasCaughtUp = case e of
            Nothing -> False
            Just e' -> (cfg ^. catchingupDistanceComputation) p e' < bypassDistance
          sendBatch ix = do
            let catchupBuffer' = ix ^. catchupBuffer
            let lastStable = ix ^? catchupLastStable
            ix' <- indexAllDescendingVia caughtUpIndexer catchupBuffer' ix
            ix'' <- case lastStable of
              Nothing -> pure ix'
              Just lastStable' -> setLastStablePointVia caughtUpIndexer lastStable' ix'
            pure $ resetBuffer ix''
          triggerHook = case cfg ^. contextCatchupEventHook of
            Nothing -> pure ()
            Just hook -> liftIO $ hook Synced
      if hasCaughtUp
        then do
          triggerHook
          indexer' <- sendBatch indexer
          indexVia caughtUpIndexer timedEvent (indexer' & config .~ Done)
        else do
          let indexer' = pushEvent indexer
          if bufferIsFull indexer'
            then sendBatch indexer'
            else pure indexer'

  rollback p indexer =
    let updateBuffer ix = ix & catchupBuffer %~ dropWhile ((> p) . Lens.view point)
        setBufferSize ix = ix & catchupBufferLength .~ (fromIntegral $ length $ ix ^. catchupBuffer)
        indexer' = setBufferSize $ updateBuffer indexer
     in if null $ indexer' ^. catchupBuffer -- on an empty buffer, we just forward the rollback
          then rollbackVia caughtUpIndexer p indexer'
          else pure indexer'

  setLastStablePoint p ix =
    if null $ ix ^. catchupBuffer
      then -- on an empty buffer, we just forward the update
        setLastStablePointVia caughtUpIndexer p ix
      else pure $ ix & catchupLastStable .~ p

instance
  (Applicative m, Resetable m event indexer)
  => Resetable m event (WithCatchup indexer)
  where
  reset indexer = do
    indexer' <- resetVia caughtUpIndexer indexer
    pure $ resetBuffer indexer'
