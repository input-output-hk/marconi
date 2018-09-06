{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import qualified Language.PlutusCore  as PC

import qualified Data.ByteString.Lazy as BSL
import           Data.Semigroup
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.Text.IO         as T

import           System.Exit

import           Options.Applicative

data Input = FileInput FilePath | StdInput

getInput :: Input -> IO String
getInput (FileInput s) = readFile s
getInput StdInput      = getContents

input :: Parser Input
input = fileInput <|> stdInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

newtype TypecheckOptions = TypecheckOptions Input
newtype EvalOptions = EvalOptions Input
data Command = Typecheck TypecheckOptions | Eval EvalOptions

plutus :: ParserInfo Command
plutus = info (plutusOpts <**> helper) (progDesc "Plutus Core tool")

plutusOpts :: Parser Command
plutusOpts = hsubparser (
    command "typecheck" (info (Typecheck <$> typecheckOpts) (progDesc "Typecheck a Plutus Core program"))
    <> command "evaluate" (info (Eval <$> evalOpts) (progDesc "Evaluate a Plutus Core program"))
  )

typecheckOpts :: Parser TypecheckOptions
typecheckOpts = TypecheckOptions <$> input

evalOpts :: Parser EvalOptions
evalOpts = EvalOptions <$> input

main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) plutus
    case options of
        Typecheck (TypecheckOptions inp) -> do
            contents <- getInput inp
            case (PC.runQuoteT . PC.parseTypecheck 1000 . BSL.fromStrict . encodeUtf8 . T.pack) contents of
                Left e -> do
                    T.putStrLn $ PC.prettyCfgText e
                    exitFailure
                Right ty -> do
                    T.putStrLn $ PC.prettyCfgText ty
                    exitSuccess
        Eval (EvalOptions inp) -> do
            contents <- getInput inp
            case (PC.parseRunCk . BSL.fromStrict . encodeUtf8 . T.pack) contents of
                Left e -> do
                    T.putStrLn $ PC.prettyCfgText e
                    exitFailure
                Right v -> do
                    T.putStrLn $ PC.prettyCfgText v
                    exitSuccess
