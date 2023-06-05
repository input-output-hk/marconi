{-# LANGUAGE DerivingStrategies #-}

module Marconi.ChainIndex.Error where

import Control.Exception (Exception, catch, throw)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as SQL

data IndexerError
  = CantStartIndexer !Text
  | CantExtractEvent !Text
  | CantQueryIndexer !Text
  | InvalidQueryInterval !Text
  | CantInsertEvent !Text
  | CantRollback !Text
  | InvalidIndexer !Text
  deriving stock (Show)

instance Exception IndexerError

liftCatch :: (IO (Either err a) -> IO (Either err a)) -> IO a -> ExceptT err IO a
liftCatch f x = ExceptT $ f (Right <$> x)

liftSQLError
  :: (Text -> IndexerError)
  -> IO a
  -> ExceptT IndexerError IO a
liftSQLError errorWrapper =
  liftCatch
    ( `catch`
        \(err0 :: SQL.SQLError) ->
          pure (Left (errorWrapper $ Text.pack $ show err0))
            `catch` \(err1 :: SQL.FormatError) ->
              pure (Left (InvalidIndexer $ Text.pack $ show err1))
                `catch` \(err2 :: SQL.ResultError) -> pure (Left (InvalidIndexer $ Text.pack $ show err2))
    )

raiseException
  :: ExceptT IndexerError IO a -> IO a
raiseException x = do
  x' <- runExceptT x
  case x' of
    Left err -> throw err
    Right result -> pure result
