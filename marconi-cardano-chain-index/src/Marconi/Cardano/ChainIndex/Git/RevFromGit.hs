module Marconi.Cardano.ChainIndex.Git.RevFromGit (
  gitRevFromGit,
) where

import Control.Exception (catch)
import Language.Haskell.TH qualified as TH
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO qualified as IO
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode)

{- | Git revision found by running git rev-parse. If git could not be
executed, then this will be an empty string.
-}
gitRevFromGit :: TH.Q TH.Exp
gitRevFromGit =
  TH.LitE . TH.StringL <$> TH.runIO runGitRevParse
  where
    runGitRevParse :: IO String
    runGitRevParse = do
      (exitCode, output, errorMessage) <-
        readProcessWithExitCode_ "git" ["rev-parse", "--verify", "HEAD"] ""
      case exitCode of
        ExitSuccess -> pure output
        ExitFailure _ -> do
          IO.hPutStrLn IO.stderr $ "WARNING: " ++ errorMessage
          pure ""

    readProcessWithExitCode_ :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
    readProcessWithExitCode_ cmd args input =
      catch (readProcessWithExitCode cmd args input) $ \e ->
        if isDoesNotExistError e
          then return (ExitFailure 127, "", show e)
          else return (ExitFailure 999, "", show e)
