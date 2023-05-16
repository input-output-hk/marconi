{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.CLI where

import Control.Applicative (optional, some)
import Data.ByteString.Char8 qualified as C8
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative qualified as Opt
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Cardano.Api (ChainPoint, NetworkId)
import Cardano.Api qualified as C
import Data.List.NonEmpty qualified as NonEmpty
import Marconi.ChainIndex.Types (
  IndexingDepth (MaxIndexingDepth, MinIndexingDepth),
  TargetAddresses,
  UtxoIndexerConfig (UtxoIndexerConfig),
  addressDatumDbName,
  epochStateDbName,
  mintBurnDbName,
  scriptTxDbName,
  utxoDbName,
 )

{- | Allow the user to set a starting point for indexing the user needs to provide both
 a @BlockHeaderHash@ (encoded in RawBytesHex) and a @SlotNo@ (a natural number).
-}
chainPointParser :: Opt.Parser C.ChainPoint
chainPointParser =
  pure C.ChainPointAtGenesis Opt.<|> (C.ChainPoint <$> slotNoParser <*> blockHeaderHashParser)
  where
    blockHeaderHashParser :: Opt.Parser (C.Hash C.BlockHeader)
    blockHeaderHashParser =
      Opt.option
        (Opt.maybeReader maybeParseHashBlockHeader Opt.<|> Opt.readerError "Malformed block header hash")
        ( Opt.long "block-header-hash"
            <> Opt.short 'b'
            <> Opt.metavar "BLOCK-HEADER-HASH"
        )
    slotNoParser :: Opt.Parser C.SlotNo
    slotNoParser =
      Opt.option
        (C.SlotNo <$> Opt.auto)
        ( Opt.long "slot-no"
            <> Opt.short 'n'
            <> Opt.metavar "SLOT-NO"
        )
    maybeParseHashBlockHeader :: String -> Maybe (C.Hash C.BlockHeader)
    maybeParseHashBlockHeader =
      either (const Nothing) Just
        . C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy)
        . C8.pack

-- TODO: `pNetworkId` and `pTestnetMagic` are copied from
-- https://github.com/input-output-hk/cardano-node/blob/988c93085022ed3e2aea5d70132b778cd3e622b9/cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs#L2009-L2027
-- Use them from there whenever they are exported.
pNetworkIdParser :: Opt.Parser C.NetworkId
pNetworkIdParser = pMainnetParser Opt.<|> fmap C.Testnet pTestnetMagicParser

pMainnetParser :: Opt.Parser C.NetworkId
pMainnetParser = Opt.flag' C.Mainnet (Opt.long "mainnet" <> Opt.help "Use the mainnet magic id.")

pTestnetMagicParser :: Opt.Parser C.NetworkMagic
pTestnetMagicParser =
  C.NetworkMagic
    <$> Opt.option
      Opt.auto
      ( Opt.long "testnet-magic"
          <> Opt.metavar "NATURAL"
          <> Opt.help "Specify a testnet magic id."
      )

{- | parses CLI params to valid NonEmpty list of Shelley addresses
 We error out if there are any invalid addresses
-}
multiAddressesParser
  :: Opt.Mod Opt.OptionFields [C.Address C.ShelleyAddr] -> Opt.Parser TargetAddresses
multiAddressesParser = fmap (NonEmpty.fromList . concat) . some . single
  where
    single :: Opt.Mod Opt.OptionFields [C.Address C.ShelleyAddr] -> Opt.Parser [C.Address C.ShelleyAddr]
    single = Opt.option (Opt.str >>= fmap nub . traverse parseCardanoAddresses . Text.words)

    deserializeToCardano :: Text -> Either C.Bech32DecodeError (C.Address C.ShelleyAddr)
    deserializeToCardano = C.deserialiseFromBech32 (C.proxyToAsType Proxy)

    parseCardanoAddresses :: Text -> Opt.ReadM (C.Address C.ShelleyAddr)
    parseCardanoAddresses arg = case deserializeToCardano arg of
      Left _ -> fail $ "Invalid address (not a valid Bech32 address representation): " <> show arg
      Right addr -> pure addr

{- | This executable is meant to exercise a set of indexers (for now datumhash -> datum)
     against the mainnet (meant to be used for testing).

     In case you want to access the results of the datumhash indexer you need to query
     the resulting database:
     $ sqlite3 datums.sqlite
     > select slotNo, datumHash, datum from kv_datumhsh_datum where slotNo = 39920450;
     39920450|679a55b523ff8d61942b2583b76e5d49498468164802ef1ebe513c685d6fb5c2|X(002f9787436835852ea78d3c45fc3d436b324184
-}
data CommonOptions = CommonOptions
  { optionsSocketPath :: !String
  -- ^ POSIX socket file to communicate with cardano node
  , optionsNetworkId :: !NetworkId
  -- ^ cardano network id
  , optionsChainPoint :: !ChainPoint
  -- ^ Required depth of a block before it is indexed
  , optionsMinIndexingDepth :: !IndexingDepth
  -- ^ Required depth of a block before it is indexed
  }
  deriving (Show)

data Options = Options
  { commonOptions :: !CommonOptions
  , optionsDbPath :: !FilePath
  -- ^ Directory path containing the SQLite database files
  , optionsEnableUtxoTxOutRef :: !Bool
  -- ^ enable storing txout refScript,
  , optionsDisableUtxo :: !Bool
  -- ^ disable Utxo indexer
  , optionsDisableAddressDatum :: !Bool
  -- ^ disable AddressDatum indexer
  , optionsDisableScript :: !Bool
  -- ^ disable Script indexer
  , optionsDisableEpochState :: !Bool
  -- ^ disable EpochState indexer
  , optionsDisableMintBurn :: !Bool
  -- ^ disable MintBurn indexer
  , optionsTargetAddresses :: !(Maybe TargetAddresses)
  -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  , optionsTargetAssets :: !(Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)))
  , optionsNodeConfigPath :: !(Maybe FilePath)
  -- ^ Path to the node config
  }
  deriving (Show)

parseOptions :: IO Options
parseOptions = getGitSha >>= Opt.execParser . programParser

programParser :: String -> Opt.ParserInfo Options
programParser gitSha =
  Opt.info
    (Opt.helper <*> commonVersionOptionParser gitSha <*> optionsParser)
    (marconiDescr "marconi")

commonOptionsParser :: Opt.Parser CommonOptions
commonOptionsParser =
  CommonOptions
    <$> commonSocketPathParser
    <*> pNetworkIdParser
    <*> chainPointParser
    <*> commonMinIndexingDepthParser

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> commonOptionsParser
    <*> commonDbDirParser
    <*> Opt.switch
      ( Opt.long "enable-txoutref"
          <> Opt.help "enable txout ref storage."
      )
    <*> Opt.switch
      ( Opt.long "disable-utxo"
          <> Opt.help "disable utxo indexers."
      )
    <*> Opt.switch
      ( Opt.long "disable-address-datum"
          <> Opt.help "disable address->datum indexers."
      )
    <*> Opt.switch
      ( Opt.long "disable-script-tx"
          <> Opt.help "disable script-tx indexers."
      )
    <*> Opt.switch
      ( Opt.long "disable-epoch-stakepool-size"
          <> Opt.help "disable epoch stakepool size indexers."
      )
    <*> Opt.switch
      ( Opt.long "disable-mintburn"
          <> Opt.help "disable mint/burn indexers."
      )
    <*> commonMaybeTargetAddressParser
    <*> commonMaybeTargetAssetParser
    <*> ( optional $
            Opt.strOption $
              Opt.long "node-config-path"
                <> Opt.help "Path to node configuration which you are connecting to."
        )

-- * Database paths

utxoDbPath :: Options -> Maybe FilePath
utxoDbPath o =
  if optionsDisableUtxo o
    then Nothing
    else Just (optionsDbPath o </> utxoDbName)

addressDatumDbPath :: Options -> Maybe FilePath
addressDatumDbPath o =
  if optionsDisableAddressDatum o
    then Nothing
    else Just (optionsDbPath o </> addressDatumDbName)

scriptTxDbPath :: Options -> Maybe FilePath
scriptTxDbPath o =
  if optionsDisableScript o
    then Nothing
    else Just (optionsDbPath o </> scriptTxDbName)

epochStateDbPath :: Options -> Maybe FilePath
epochStateDbPath o = do
  if optionsDisableEpochState o
    then Nothing
    else Just $ optionsDbPath o </> epochStateDbName

mintBurnDbPath :: Options -> Maybe FilePath
mintBurnDbPath o =
  if optionsDisableMintBurn o
    then Nothing
    else Just (optionsDbPath o </> mintBurnDbName)

-- * Common CLI parsers for other derived programs.

commonSocketPathParser :: Opt.Parser String
commonSocketPathParser =
  Opt.strOption $
    Opt.long "socket-path"
      <> Opt.short 's'
      <> Opt.help "Path to node socket."
      <> Opt.metavar "FILE-PATH"

-- | Root directory for the SQLite storage of all the indexers
commonDbDirParser :: Opt.Parser String
commonDbDirParser =
  Opt.strOption $
    Opt.short 'd'
      <> Opt.long "db-dir"
      <> Opt.metavar "DIR"
      <> Opt.help "Directory path where all Marconi-related SQLite databases are located."

commonVersionOptionParser :: String -> Opt.Parser (a -> a)
commonVersionOptionParser sha = Opt.infoOption sha $ Opt.long "version" <> Opt.help "Show git SHA"

getGitSha :: IO String
getGitSha = fromMaybe "GIHUB_SHA environment variable not set!" <$> lookupEnv "GITHUB_SHA"

marconiDescr :: String -> Opt.InfoMod a
marconiDescr programName =
  Opt.fullDesc
    <> Opt.progDesc programName
    <> Opt.header
      ( programName
          <> " - a lightweight customizable solution for indexing and querying the Cardano blockchain"
      )

commonMaybePortParser :: Opt.Parser (Maybe Int)
commonMaybePortParser =
  Opt.optional $
    Opt.option Opt.auto $
      Opt.long "http-port"
        <> Opt.metavar "HTTP-PORT"
        <> Opt.help "JSON-RPC http port number, default is port 3000."

{- | Parse the addresses to index. Addresses should be given in Bech32 format
 Several addresses can be given in a single string, if they are separated by a space
-}
commonMaybeTargetAddressParser :: Opt.Parser (Maybe TargetAddresses)
commonMaybeTargetAddressParser =
  Opt.optional $
    multiAddressesParser $
      Opt.long "addresses-to-index"
        <> Opt.short 'a'
        <> Opt.metavar "BECH32-ADDRESS"
        <> Opt.help
          "Bech32 Shelley addresses to index. \
          \ i.e \"--address-to-index address-1 --address-to-index address-2 ...\"\
          \ or \"--address-to-index \"address-1 address-2\" ...\""

{- | Parse target assets, both the @PolicyId@ and the @AssetName@ are expected to be in their
 RawBytesHex representation, they must be separated by a comma.
 The asset name can be omited, if it is the case, any asset with the expected policy ID will
 be matched.
 Several assets can be given in a single string if you separate them with a space.
-}
commonMaybeTargetAssetParser :: Opt.Parser (Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)))
commonMaybeTargetAssetParser =
  let assetPair
        :: Opt.Mod Opt.OptionFields [(C.PolicyId, Maybe C.AssetName)]
        -> Opt.Parser [(C.PolicyId, Maybe C.AssetName)]
      assetPair = Opt.option $ Opt.str >>= fmap nub . traverse parseAsset . Text.words
   in Opt.optional $
        (fmap (NonEmpty.fromList . concat) . some . assetPair) $
          Opt.long "match-asset-id"
            <> Opt.metavar "POLICY_ID[.ASSET_NAME]"
            <> Opt.help
              "Asset to index, defined by the policy id and an optional asset name\
              \ i.e \"--match-asset-id assetname-1.policy-id-1 --match-asset-id policy-id-2 ...\"\
              \ or \"--match-asset-id \"assetname-1.policy-id-1 policy-id-2\" ...\""

-- | Asset parser, see @commonMaybeTargetAssetParser@ for more info.
parseAsset :: Text -> Opt.ReadM (C.PolicyId, Maybe C.AssetName)
parseAsset arg = do
  let parseAssetName :: Text -> Opt.ReadM C.AssetName
      parseAssetName = either (fail . show) pure . C.deserialiseFromRawBytesHex C.AsAssetName . Text.encodeUtf8

      parsePolicyId :: Text -> Opt.ReadM C.PolicyId
      parsePolicyId = either (fail . show) pure . C.deserialiseFromRawBytesHex C.AsPolicyId . Text.encodeUtf8
  case Text.splitOn "." arg of
    [rawPolicyId, rawAssetName] ->
      (,) <$> parsePolicyId rawPolicyId <*> (Just <$> parseAssetName rawAssetName)
    [rawPolicyId] ->
      (,Nothing) <$> parsePolicyId rawPolicyId
    _other ->
      fail $ "Invalid format: expected POLICY_ID[,ASSET_NAME]. Got " <> Text.unpack arg

-- | Allow the user to specify how deep must be a block before we index it.
commonMinIndexingDepthParser :: Opt.Parser IndexingDepth
commonMinIndexingDepthParser =
  let maxIndexingDepth =
        Opt.flag'
          MaxIndexingDepth
          (Opt.long "max-indexing-depth" <> Opt.help "Only index blocks that are not rollbackable")
      givenIndexingDepth =
        MinIndexingDepth
          <$> Opt.option
            Opt.auto
            ( Opt.long "min-indexing-depth"
                <> Opt.metavar "NATURAL"
                <> Opt.help "Depth of a block before it is indexed in relation to the tip of the local connected node"
                <> Opt.value 0
            )
   in maxIndexingDepth Opt.<|> givenIndexingDepth

-- | Extract UtxoIndexerConfig from CLI Options
mkUtxoIndexerConfig :: Options -> UtxoIndexerConfig
mkUtxoIndexerConfig o = UtxoIndexerConfig (optionsTargetAddresses o) (optionsEnableUtxoTxOutRef o)
