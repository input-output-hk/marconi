{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Concurrency (
  HandledAction (Handled, Unhandled),
  raceSignalHandled_,
  raceSignalHandled,
) where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (Exception, catch, throwIO)
import Control.Monad (void)
import Data.Void (Void)
import Marconi.Sidechain.Error (HasExitCode (toExitCode), signalToExit, withSignalHandling)
import System.Exit (ExitCode (ExitFailure), exitWith)

-- * Graceful exception handling after SIGINT or SIGTERM

{- These purpose of these functions is as follows:

  If we try to naiively exit with a different exit code than that of the received signal, our new
	exit code will be ignored, We can work around this by using `installHandler`; catching the
	signal and setting our own exit code.

  We can use exceptions to map our domain failures to these exit codes.

  To do this, we use an MVar to hold any overwritten exit code. The exit code is overwritten
  by a handler function, which catches exceptions of the desired type. The type is then
  pattern matched and the exit code is chosen based on the contructor.

  If the program is exiting due to a SIGINT or SIGTERM and if we've got an MVar in our result,
  we set the exit code to whatever it specifies. If either of the previous points aren't true,
  we exit with whatever signal (SIGINT or SIGTERM) requested shutdown. -}

{- | A data type representing an action with explicit instructions as to whether concurrent
		 operations should attempt to map any exceptions it throws to Unix exit codes.

		It has two type parameters:

		* 'e' the exception type, 'Void' in the unhandled case

    * 'a' the type returned by the action
-}
data HandledAction e a where
  Handled :: (Exception e, HasExitCode e) => IO a -> HandledAction e a
  Unhandled :: IO a -> HandledAction Void a

{- | Perform two actions, with explicit handling instructions, concurrently (with the chosen scheme)

		Takes:

		* 'concOp' - A concurrent operation (`race` or `concurrent`)
		* 'left' - The "left" `HandledAction` for the concurrent operation to run
		* 'right - The "right" `HandledAction` for the concurrent operation to run

		Returns whatever the concurrent operation would, if called plainly.

		If the program is already shutting as a result of `SIGINT` or `SIGTERM`, any handled actions
		will have their exceptions caught and mapped to an exit code.
-}
concOpSignalHandled
  :: forall e1 e2 a b f
   . (IO a -> IO b -> IO (f a b))
  -> HandledAction e1 a
  -> HandledAction e2 b
  -> IO (f a b)
concOpSignalHandled concOp hleft hright = do
  mvar <- newEmptyMVar
  let withHandler :: forall e c. HandledAction e c -> IO c
      withHandler handledAction = case handledAction of
        Handled action -> action `catch` (\e -> putMVar mvar (toExitCode @e e) >> throwIO e)
        Unhandled action -> action
  res <-
    withSignalHandling $
      concOp
        (withHandler hleft)
        (withHandler hright)
  var' <- tryTakeMVar mvar
  case var' of
    Just code -> exitWith (ExitFailure code)
    Nothing -> case res of
      Right result -> pure result
      Left cint -> exitWith (ExitFailure (signalToExit $ fromIntegral cint))

{- | Race two actions, with explicit handling instructions, concurrently.

		Takes two `HandledAction`s (the mechanism to explicitly specify handling instructions), and
		returns whatever `race` would, if called directly.

		If the program is already shutting as a result of `SIGINT` or `SIGTERM`, any handled actions
		will have their exceptions caught and mapped to an exit code.
-}
raceSignalHandled
  :: forall e1 e2 a b
   . HandledAction e1 a
  -> HandledAction e2 b
  -> IO (Either a b)
raceSignalHandled = concOpSignalHandled race

-- | The same as `raceSignalHandled`, but discards its result.
raceSignalHandled_
  :: forall e1 e2 a b
   . HandledAction e1 a
  -> HandledAction e2 b
  -> IO ()
raceSignalHandled_ hleft hright = void $ raceSignalHandled hleft hright
