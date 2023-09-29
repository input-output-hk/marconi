module Marconi.Core.Util.WithGracefulTermination where

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
