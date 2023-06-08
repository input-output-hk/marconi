{- | Typeclass for indexer transformers,

   It allows the generalisation of properties amongst all indexer transformers.
-}
module Marconi.Core.Experiment.Transformer.Class (
  IndexerTrans (Config, wrap, unwrap),
  IndexerMapTrans (ConfigMap, wrapMap, unwrapMap),
) where

import Control.Lens (Lens')
import Data.Kind (Type)

-- | An indexer transformer: it adds a configurable capability to a tranformer
class IndexerTrans t where
  -- | The type of the configuration of a transformer
  type Config t :: Type -> Type

  -- | Wrap an existing indexer in its transformer
  wrap :: Config t event -> indexer event -> t indexer event

  -- | Unwray the underlying indexer
  unwrap :: Lens' (t indexer event) (indexer event)

{- | An indexer transformer: it adds a configurable capability to a tranformer

 This one allow also the transformation of the event, contrary to 'IndexerTrans'.
-}
class IndexerMapTrans t where
  -- | The type of the configuration of a transformer
  type ConfigMap t :: Type -> Type -> Type

  -- | Wrap an existing indexer in its transformer
  wrapMap :: ConfigMap t output event -> indexer output -> t indexer output event

  -- | Unwray the underlying indexer
  unwrapMap :: Lens' (t indexer output event) (indexer output)
