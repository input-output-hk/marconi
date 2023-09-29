{- | Typeclass for indexer transformers,

   It allows the generalisation of properties amongst all indexer transformers.
-}
module Marconi.Core.Transformer.Class (
  IndexerTrans (unwrap),
  IndexerMapTrans (unwrapMap),
) where

import Control.Lens (Lens')

-- | An indexer transformer: it adds a configurable capability to a tranformer
class IndexerTrans t where
  -- | Unwrap the underlying indexer
  unwrap :: Lens' (t indexer event) (indexer event)

{- | An indexer transformer: it adds a configurable capability to a tranformer

 This one allow also the transformation of the event, contrary to 'IndexerTrans'.
-}
class IndexerMapTrans t where
  -- | Unwrap the underlying indexer
  unwrapMap :: Lens' (t indexer output event) (indexer output)
