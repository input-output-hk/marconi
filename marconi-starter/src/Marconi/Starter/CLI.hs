module Marconi.Starter.CLI where

import Marconi.Cardano.ChainIndex.CLI qualified as CLI
import Options.Applicative qualified as Opt

data Options = Options
  { commonOptions :: !CLI.CommonOptions
  , optionsDbPath :: !FilePath
  , optionsHttpPort :: !Int
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
    <*> CLI.commonPortParser

programDescription :: Opt.InfoMod a
programDescription =
  Opt.fullDesc
    <> Opt.progDesc "marconi-starter"
    <> Opt.header "Marconi starter executable"
