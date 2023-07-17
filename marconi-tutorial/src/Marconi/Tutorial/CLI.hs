module Marconi.Tutorial.CLI where

import Marconi.ChainIndex.CLI qualified as CLI
import Options.Applicative qualified as Opt

data Options = Options
  { commonOptions :: !CLI.CommonOptions
  , optionsDbPath :: !FilePath
  , optionsHttpPort :: !(Maybe Int)
  }

parseOptions :: IO Options
parseOptions = Opt.execParser programParser

programParser :: Opt.ParserInfo Options
programParser =
  Opt.info
    (Opt.helper <*> optionsParser)
    programDescription

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> CLI.commonOptionsParser
    <*> CLI.commonDbDirParser
    <*> CLI.commonMaybePortParser

programDescription :: Opt.InfoMod a
programDescription =
  Opt.fullDesc
    <> Opt.progDesc "marconi-tutorial"
    <> Opt.header "Marconi tutorial executable"
