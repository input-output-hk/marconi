{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.CLI where

import Control.Applicative (many, optional, some)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as C8
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Version (showVersion)
import Options.Applicative qualified as Opt
import System.FilePath ((</>))
import Text.Read (readEither)

import Cardano.Api (ChainPoint, NetworkId)
import Cardano.Api qualified as C
import Data.List.NonEmpty qualified as NEList
import Data.Set.NonEmpty qualified as NESet
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (
  BlockRange,
  RetryConfig (RetryConfig),
  TargetAddresses,
  UtxoIndexerConfig (UtxoIndexerConfig),
  addressDatumDbName,
  epochStateDbName,
  mintBurnDbName,
  mkBlockRange,
  scriptTxDbName,
  utxoDbName,
 )
import Marconi.ChainIndex.Git.Rev (gitRev)
import Options.Applicative (ReadM, eitherReader, execParserPure)
import Paths_marconi_chain_index (version)

-- | Represents a specified point from which to start indexing
data StartingPoint
  = StartFromGenesis
  | StartFromLastSyncPoint
  | StartFrom ChainPoint
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- | Allow the user to set a starting point for indexing.

  * If the user picks a specific point with the @start-from@ command, they need to provide both
      a @BlockHeaderHash@ (encoded in RawBytesHex) and a @SlotNo@ (a natural number).
  * If the chooses either @--start-from-genesis@ or @--start-from-last-sync-points@, no args
      are required.
  * Not using any of these commands results in the same behaviour as @--start-from-last-sync-points@
-}
commonStartFromParser :: Opt.Parser StartingPoint
commonStartFromParser =
  pure StartFromLastSyncPoint
    Opt.<|> fromGenesis
    Opt.<|> fromLastSyncPoint
    Opt.<|> givenPoint
  where
    fromGenesis =
      Opt.flag'
        StartFromGenesis
        ( Opt.long "start-from-genesis"
            <> Opt.help "Start from genesis"
        )
    fromLastSyncPoint =
      Opt.flag'
        StartFromLastSyncPoint
        ( Opt.long "start-from-last-sync-points"
            <> Opt.help "Start from the minimum last sync point"
        )
    givenPoint :: Opt.Parser StartingPoint
    givenPoint = StartFrom . uncurry C.ChainPoint <$> parseStartFrom
    parseStartFrom :: Opt.Parser (C.SlotNo, C.Hash C.BlockHeader)
    parseStartFrom =
      Opt.option
        parser
        ( Opt.long "start-from"
            <> Opt.metavar "SLOT-NO:BLOCK-HEADER-HASH"
            <> Opt.help
              "Start from a given slot and block header hash. Usage: `--start-from SLOT-NO:BLOCK-HEADER-HASH`. Might fail if the target indexers can't resume from arbitrary points."
        )
    parser :: ReadM (C.SlotNo, C.Hash C.BlockHeader)
    parser = eitherReader $ \s ->
      case break (== ':') s of
        (l, ':' : r) -> case (exec slotNoParser [l], exec blockHeaderHashParser [r]) of
          (Opt.Success sn, Opt.Success bhh) -> Right (sn, bhh)
          (Opt.Failure _, Opt.Success _) -> Left $ badSlotNo l
          (Opt.Success _, Opt.Failure _) -> Left $ badBhh r
          (_, _) -> Left $ badSlotNo l ++ ". " ++ badBhh r
        _ -> Left "Invalid format, expected SLOT-NO:BLOCK-HEADER-HASH"
    badSlotNo l = "Expected SLOT-NO, got " ++ show l
    badBhh bhh = "Expected BLOCK-HEADER-HASH, got " ++ show bhh
    exec p = execParserPure Opt.defaultPrefs (Opt.info p mempty)
    blockHeaderHashParser :: Opt.Parser (C.Hash C.BlockHeader)
    blockHeaderHashParser =
      Opt.argument
        (Opt.maybeReader maybeParseHashBlockHeader Opt.<|> Opt.readerError "Malformed block header hash")
        (Opt.metavar "BLOCK-HEADER-HASH")
    slotNoParser :: Opt.Parser C.SlotNo
    slotNoParser =
      Opt.argument
        (C.SlotNo <$> Opt.auto)
        (Opt.metavar "SLOT-NO")
    maybeParseHashBlockHeader :: String -> Maybe (C.Hash C.BlockHeader)
    maybeParseHashBlockHeader =
      either (const Nothing) Just
        . C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy)
        . C8.pack

-- TODO: `pNetworkId` and `pTestnetMagic` are copied from
-- https://github.com/input-output-hk/cardano-node/blob/988c93085022ed3e2aea5d70132b778cd3e622b9/cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs#L2009-L2027
-- Use them from there whenever they are exported.
commonNetworkIdParser :: Opt.Parser C.NetworkId
commonNetworkIdParser = pMainnetParser Opt.<|> fmap C.Testnet pTestnetMagicParser

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
multiAddressesParser = fmap (NESet.fromList . NEList.fromList . concat) . some . single
  where
    single :: Opt.Mod Opt.OptionFields [C.Address C.ShelleyAddr] -> Opt.Parser [C.Address C.ShelleyAddr]
    single = Opt.option (Opt.str >>= traverse parseCardanoAddresses . Text.words)

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
  , optionsChainPoint :: !StartingPoint
  -- ^ The starting point of the indexers
  , optionsRetryConfig :: !RetryConfig
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
  , optionsRpcPort :: !Int
  -- ^ port the RPC server should listen on
  , optionsTargetAddresses :: !(Maybe TargetAddresses)
  -- ^ white-space separated list of Bech32 Cardano Shelley addresses
  , optionsTargetAssets :: !(Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName)))
  -- ^ white-space separated list of target asset policy id and optionally asset name,
  -- separated by @.@.
  , optionsNodeConfigPath :: !(Maybe FilePath)
  -- ^ Path to the node config
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

parseOptions :: IO Options
parseOptions = Opt.execParser programParser

parseSnapshotOptions :: IO SnapshotOptions
parseSnapshotOptions = Opt.execParser snapshotProgramParser

getVersion :: String
getVersion = showVersion version <> "-" <> Text.unpack gitRev

programParser :: Opt.ParserInfo Options
programParser =
  Opt.info
    (Opt.helper <*> commonVersionOptionParser <*> optionsParser)
    (marconiDescr "marconi")

snapshotProgramParser :: Opt.ParserInfo SnapshotOptions
snapshotProgramParser =
  Opt.info
    (Opt.helper <*> snapshotOptionsParser)
    (marconiDescr "marconi")

commonOptionsParser :: Opt.Parser CommonOptions
commonOptionsParser =
  CommonOptions
    <$> commonSocketPathParser
    <*> commonNetworkIdParser
    <*> commonStartFromParser
    <*> commonRetryConfigParser

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
    <*> commonPortParser
    <*> commonMaybeTargetAddressParser
    <*> commonMaybeTargetAssetParser
    <*> optional commonNodeConfigPathParser

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

snapshotDirParser :: Opt.Parser String
snapshotDirParser =
  Opt.strOption $
    Opt.short 'd'
      <> Opt.long "snapshot-dir"
      <> Opt.metavar "DIR"
      <> Opt.help "Directory path containing the resulting snapshot files for each sub-chain."

commonVersionOptionParser :: Opt.Parser (a -> a)
commonVersionOptionParser = Opt.infoOption getVersion $ Opt.long "version" <> Opt.help "Show marconi version"

marconiDescr :: String -> Opt.InfoMod a
marconiDescr programName =
  Opt.fullDesc
    <> Opt.progDesc programName
    <> Opt.header
      ( programName
          <> " - a lightweight customizable solution for indexing and querying the Cardano blockchain"
      )

commonPortParser :: Opt.Parser Int
commonPortParser =
  Opt.option Opt.auto $
    Opt.long "http-port"
      <> Opt.metavar "INT"
      <> Opt.value 3000
      <> Opt.help "JSON-RPC http port number"
      <> Opt.showDefault

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
          \ i.e \"--addresses-to-index address-1 --addresses-to-index address-2 ...\"\
          \ or \"--addresses-to-index \"address-1 address-2\" ...\""

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
        (fmap (NEList.fromList . concat) . some . assetPair) $
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
      parseAssetName =
        either (fail . C.displayError) pure . C.deserialiseFromRawBytesHex C.AsAssetName . Text.encodeUtf8

      parsePolicyId :: Text -> Opt.ReadM C.PolicyId
      parsePolicyId =
        either (fail . displayError') pure . C.deserialiseFromRawBytesHex C.AsPolicyId . Text.encodeUtf8

      -- Modify the error message to avoid mentioning `ScriptHash` when a `PolicyId` was being
      -- given. We get this because `PolicyId` is a `newtype` of `ScriptHash`. The only possible
      -- cause of failure in a `RawBytesHexErrorRawBytesDecodeFail` error is an incorrect length.
      -- (See `Cardano.Crypto.Hash.hashFromBytes`.)
      displayError' =
        C.displayError . \case
          C.RawBytesHexErrorRawBytesDecodeFail input asType (C.SerialiseAsRawBytesError _) ->
            C.RawBytesHexErrorRawBytesDecodeFail
              input
              asType
              (C.SerialiseAsRawBytesError "Incorrect number of bytes")
          e -> e
  case Text.splitOn "." arg of
    [rawPolicyId, rawAssetName] ->
      (,) <$> parsePolicyId rawPolicyId <*> (Just <$> parseAssetName rawAssetName)
    [rawPolicyId] ->
      (,Nothing) <$> parsePolicyId rawPolicyId
    _other ->
      fail $ "Invalid format: expected POLICY_ID[.ASSET_NAME]. Got " <> Text.unpack arg

commonNodeConfigPathParser :: Opt.Parser FilePath
commonNodeConfigPathParser =
  Opt.strOption $
    Opt.long "node-config-path"
      <> Opt.help "Path to node configuration which you are connecting to."

-- | Allow the user to specify the retry config when the connection to the node is lost.
commonRetryConfigParser :: Opt.Parser RetryConfig
commonRetryConfigParser =
  RetryConfig <$> initialRetryTimeParser <*> (noMaxRetryTimeParser Opt.<|> maxRetryTimeParser)
  where
    initialRetryTimeParser :: Opt.Parser Word64
    initialRetryTimeParser =
      Opt.option
        Opt.auto
        ( Opt.long "initial-retry-time"
            <> Opt.metavar "NATURAL"
            <> Opt.help "Initial time (in seconds) before a retry after a failed node connection. Defaults to 1s."
            <> Opt.value 1
        )

    noMaxRetryTimeParser :: Opt.Parser (Maybe Word64)
    noMaxRetryTimeParser =
      Opt.flag' Nothing (Opt.long "no-max-retry-time" <> Opt.help "Unlimited retries.")

    maxRetryTimeParser :: Opt.Parser (Maybe Word64)
    maxRetryTimeParser =
      Just
        <$> Opt.option
          Opt.auto
          ( Opt.long "max-retry-time"
              <> Opt.metavar "NATURAL"
              <> Opt.help "Max time (in seconds) allowed after startup for retries. Defaults to 30min."
              <> Opt.value 1_800
          )

-- | Extract UtxoIndexerConfig from CLI Options
mkUtxoIndexerConfig :: Options -> UtxoIndexerConfig
mkUtxoIndexerConfig o = UtxoIndexerConfig (optionsTargetAddresses o) (optionsEnableUtxoTxOutRef o)

-- | CL options for the marconi-chain-snapshot executable.
data SnapshotOptions = SnapshotOptions
  { snapshotOptionsSocketPath :: !String
  -- ^ POSIX socket file to communicate with cardano node
  , snapshotOptionsNetworkId :: !NetworkId
  -- ^ Cardano network id
  , snapshotOptionsRetryConfig :: !RetryConfig
  -- ^ Time to wait until retrying socket connection
  , snapshotOptionsSnapshotDir :: !FilePath
  -- ^ Directory path containing the files for each snapshot
  , snapshotOptionsNodeConfigPath :: !(Maybe FilePath)
  -- ^ Path to the node config
  , snapshotOptionsBlockRanges :: ![BlockRange]
  -- ^ A list of block ranges to snapshot
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

snapshotOptionsParser :: Opt.Parser SnapshotOptions
snapshotOptionsParser =
  SnapshotOptions
    <$> commonSocketPathParser
    <*> commonNetworkIdParser
    <*> commonRetryConfigParser
    <*> snapshotDirParser
    <*> optional commonNodeConfigPathParser
    <*> snapshotBlockRangesParser

snapshotBlockRangesParser :: Opt.Parser [BlockRange]
snapshotBlockRangesParser = many blockRangeParser
  where
    blockRangeParser :: Opt.Parser BlockRange
    blockRangeParser =
      Opt.option
        (Opt.str >>= readBlockRange)
        ( Opt.long "block-range"
            <> Opt.help
              "Specify one or multiple block ranges to snapshot. \
              \For example, \"--block-range 10,200 --block-range 210,1000\"."
        )

readBlockRange :: String -> Opt.ReadM BlockRange
readBlockRange rawBlockRange =
  worker (Text.split isComma (Text.pack rawBlockRange))
  where
    isComma ',' = True
    isComma _ = False

    worker :: [Text] -> Opt.ReadM BlockRange
    worker [] = fail "No block range specified."
    worker [x, y] = do
      i1 <- either fail pure $ readEither $ Text.unpack x
      i2 <- either fail pure $ readEither $ Text.unpack y
      either fail pure $ mkBlockRange i1 i2
    worker _ =
      fail $
        "Block range is not formatted correctly. "
          <> "Please provide a pair of positive numbers, "
          <> "without any spaces."
