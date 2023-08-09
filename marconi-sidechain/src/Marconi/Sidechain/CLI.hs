module Marconi.Sidechain.CLI (
  parseCli,
  Cli.getVersion,
  programParser,
) where

import Cardano.Api qualified as C
import Data.List.NonEmpty (NonEmpty)
import Marconi.ChainIndex.CLI qualified as Cli
import Marconi.ChainIndex.Types (
  IndexingDepth,
  ShouldFailIfResync (ShouldFailIfResync),
  TargetAddresses,
 )
import Options.Applicative qualified as Opt

-- | Type represents http port for JSON-RPC
data CliArgs = CliArgs
  { socketFilePath :: !FilePath
  -- ^ POSIX socket file to communicate with cardano node
  , nodeConfigPath :: !FilePath
  -- ^ Path to the node config
  , dbDir :: !FilePath
  -- ^ Directory path containing the SQLite database files
  , httpPort :: !(Maybe Int)
  -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId :: !C.NetworkId
  -- ^ cardano network id
  , minIndexingDepth :: !IndexingDepth
  -- ^ Required depth of a block before it is indexed
  , targetAddresses :: !(Maybe TargetAddresses)
  -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  , targetAssets :: !(Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)))
  -- ^ a list of asset to track
  , optionsFailsIfResync :: !ShouldFailIfResync
  -- ^ Fails resuming if at least one indexer will resync from genesis instead of one of its lastest
  -- synced point.
  }
  deriving (Show)

parseCli :: IO CliArgs
parseCli = Opt.execParser programParser

programParser :: Opt.ParserInfo CliArgs
programParser =
  Opt.info
    (Opt.helper <*> Cli.commonVersionOptionParser <*> parserCliArgs)
    (Cli.marconiDescr "marconi-sidechain")

parserCliArgs :: Opt.Parser CliArgs
parserCliArgs =
  CliArgs
    <$> Cli.commonSocketPathParser
    <*> Opt.strOption
      ( Opt.long "node-config-path"
          <> Opt.help "Path to node configuration which you are connecting to."
      )
    <*> Cli.commonDbDirParser
    <*> Cli.commonMaybePortParser
    <*> Cli.pNetworkIdParser
    <*> Cli.commonMinIndexingDepthParser
    <*> Cli.commonMaybeTargetAddressParser
    <*> Cli.commonMaybeTargetAssetParser
    <*> ( ShouldFailIfResync
            <$> Opt.switch
              ( Opt.long "fail-if-resyncing-from-genesis"
                  <> Opt.help
                    "Fails resuming if one indexer must resync from genesis when it can resume from a later point."
              )
        )
