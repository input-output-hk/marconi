{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}

{- | The catch-up mechanism buffered events to pass them in batch to an indexer, to reduce the time
 needed for an indexer to catch-up with the tip.

 Once we are close enough to the tip of the chain, `WithCatchup` deactivate itself and pass directly
 the blocks to the indexer.

 When you use several workers and coordinators, you may want to put the catchup on the final
 indexers and not on the coordinators to improve performances.
-}
module Marconi.Core.Experiment.Transformer.WithCatchup (
  WithCatchup,
  withCatchup,
  CatchupConfig (CatchupConfig),
  HasCatchupConfig (..),
) where

import Control.Lens qualified as Lens
import Control.Lens.Operators ((%~), (+~), (.~), (^.))
import Data.Function ((&))
import Data.Word (Word64)
import Marconi.Core.Experiment.Class (
  Closeable,
  IsIndex (index, rollback),
  IsSync,
  Queryable,
  Resetable (reset),
 )
import Marconi.Core.Experiment.Indexer.SQLiteAggregateQuery (HasDatabasePath)
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (unwrapMap))
import Marconi.Core.Experiment.Transformer.IndexWrapper (
  IndexWrapper (IndexWrapper),
  IndexerTrans (Config, unwrap, wrap),
  indexAllDescendingVia,
  indexVia,
  resetVia,
  rollbackVia,
  wrappedIndexer,
  wrapperConfig,
 )
import Marconi.Core.Experiment.Type (Point, Timed (Timed), point)

data CatchupConfig = CatchupConfig
  { _configCatchupBatchSize :: Word64
  -- ^ Maximal number of events in one batch
  , _configCatchupBypassDistance :: Word64
  -- ^ How far from the block should we be to bypass the catchup mechanism (in number of blocks)
  }

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
  }

Lens.makeLenses ''CatchupContext

contextCatchupBypassDistance :: Lens.Lens' (CatchupContext event) Word64
contextCatchupBypassDistance = contextCatchupConfig . configCatchupBypassDistance

contextCatchupBatchSize :: Lens.Lens' (CatchupContext event) Word64
contextCatchupBatchSize = contextCatchupConfig . configCatchupBatchSize

newtype WithCatchup indexer event = WithCatchup {_catchupWrapper :: IndexWrapper CatchupContext indexer event}

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
  WithCatchup . IndexWrapper (CatchupContext computeDistance config 0 [])

deriving via
  (IndexWrapper CatchupContext indexer)
  instance
    (IsSync m event indexer) => IsSync m event (WithCatchup indexer)

deriving via
  (IndexWrapper CatchupContext indexer)
  instance
    (HasDatabasePath indexer) => HasDatabasePath (WithCatchup indexer)

deriving via
  (IndexWrapper CatchupContext indexer)
  instance
    (Closeable m indexer) => Closeable m (WithCatchup indexer)

deriving via
  (IndexWrapper CatchupContext indexer)
  instance
    (Queryable m event query indexer) => Queryable m event query (WithCatchup indexer)

caughtUpIndexer :: Lens.Lens' (WithCatchup indexer event) (indexer event)
caughtUpIndexer = catchupWrapper . wrappedIndexer

instance IndexerTrans WithCatchup where
  type Config WithCatchup = CatchupContext

  wrap cfg = WithCatchup . IndexWrapper cfg

  unwrap = caughtUpIndexer

{- | A typeclass that allows an indexer with a @WitchCatchup@ transformer to configure
 the behaviour of this transformer
-}
class HasCatchupConfig indexer where
  catchupBypassDistance :: Lens.Lens' (indexer event) Word64
  catchupBatchSize :: Lens.Lens' (indexer event) Word64

instance {-# OVERLAPPING #-} HasCatchupConfig (WithCatchup indexer) where
  catchupBypassDistance = catchupWrapper . wrapperConfig . contextCatchupBypassDistance
  catchupBatchSize = catchupWrapper . wrapperConfig . contextCatchupBatchSize

instance
  {-# OVERLAPPABLE #-}
  (IndexerTrans t, HasCatchupConfig indexer)
  => HasCatchupConfig (t indexer)
  where
  catchupBypassDistance = unwrap . catchupBypassDistance
  catchupBatchSize = unwrap . catchupBatchSize

instance
  {-# OVERLAPPABLE #-}
  (IndexerMapTrans t, HasCatchupConfig indexer)
  => HasCatchupConfig (t indexer output)
  where
  catchupBypassDistance = unwrapMap . catchupBypassDistance
  catchupBatchSize = unwrapMap . catchupBatchSize

catchupDistance :: Lens.Lens' (WithCatchup indexer event) (Point event -> event -> Word64)
catchupDistance = catchupWrapper . wrapperConfig . contextDistanceComputation

catchupBuffer :: Lens.Lens' (WithCatchup indexer event) [Timed (Point event) (Maybe event)]
catchupBuffer = catchupWrapper . wrapperConfig . contextCatchupBuffer

catchupBufferLength :: Lens.Lens' (WithCatchup indexer event) Word64
catchupBufferLength = catchupWrapper . wrapperConfig . contextCatchupBufferLength

resetBuffer :: WithCatchup indexer event -> WithCatchup indexer event
resetBuffer = (catchupBufferLength .~ 0) . (catchupBuffer .~ [])

instance (IsIndex m event indexer) => IsIndex m event (WithCatchup indexer) where
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
          pure $ resetBuffer ix'
     in if hasCaughtUp timedEvent
          then do
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

instance
  (Applicative m, Resetable m event indexer)
  => Resetable m event (WithCatchup indexer)
  where
  reset indexer = do
    indexer' <- resetVia caughtUpIndexer indexer
    pure $ resetBuffer indexer'
