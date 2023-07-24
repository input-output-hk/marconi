{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Error where

import Control.Exception (Exception, catch, throw)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Database.SQLite.Simple qualified as SQL

data IndexerError a
  = CantStartIndexer !Text
  | CantExtractEvent !Text
  | CantQueryIndexer !Text
  | QueryError a
  | CantInsertEvent !Text
  | CantRollback !Text
  | InvalidIndexer !Text
  deriving stock (Show)

instance (Exception a) => Exception (IndexerError a)

liftCatch :: (IO (Either err a) -> IO (Either err a)) -> IO a -> ExceptT err IO a
liftCatch f x = ExceptT $ f (Right <$> x)

liftSQLError :: (Text -> IndexerError err) -> IO a -> ExceptT (IndexerError err) IO a
liftSQLError errorWrapper =
  liftCatch
    ( `catch`
        \(err0 :: SQL.SQLError) ->
          pure (Left (errorWrapper $ Text.pack $ show err0))
            `catch` \(err1 :: SQL.FormatError) ->
              pure (Left (InvalidIndexer $ Text.pack $ show err1))
                `catch` \(err2 :: SQL.ResultError) -> pure (Left (InvalidIndexer $ Text.pack $ show err2))
    )

raiseException :: (Exception err) => ExceptT (IndexerError err) IO a -> IO a
raiseException x = do
  x' <- runExceptT x
  case x' of
    Left err -> throw err
    Right result -> pure result

ignoreQueryError :: ExceptT (IndexerError err) IO a -> ExceptT (IndexerError Void) IO a
ignoreQueryError x = ExceptT $ do
  x' <- runExceptT x
  pure $ case x' of
    Left (CantStartIndexer msg) -> Left (CantStartIndexer msg)
    Left (CantExtractEvent msg) -> Left (CantExtractEvent msg)
    Left (CantQueryIndexer msg) -> Left (CantQueryIndexer msg)
    Left (QueryError _msg) -> Left (CantQueryIndexer "Query was invalid")
    Left (CantInsertEvent msg) -> Left (CantInsertEvent msg)
    Left (CantRollback msg) -> Left (CantRollback msg)
    Left (InvalidIndexer msg) -> Left (InvalidIndexer msg)
    Right result -> Right result
