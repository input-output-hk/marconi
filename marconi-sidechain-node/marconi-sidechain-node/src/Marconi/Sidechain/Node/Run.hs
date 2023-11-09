{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Node.Run where

import Cardano.Crypto.Init qualified as Crypto
import Cardano.Node.Configuration.POM (PartialNodeConfiguration)
import Cardano.Node.Handlers.TopLevel (toplevelExceptionHandler)
import Cardano.Node.LedgerEvent (
  AnchoredEvent (AnchoredEvent),
  LedgerEvent (LedgerNewEpochEvent),
  LedgerEventReader,
  LedgerEventWriter,
  LedgerNewEpochEvent (LedgerStakeDistEvent),
  mkLedgerEventHandler,
  withLedgerEventsChan,
 )
import Cardano.Node.Parsers (
  nodeCLIParser,
  parserHelpHeader,
  parserHelpOptions,
  renderHelpDoc,
 )
import Cardano.Node.Run qualified as Node
import Cardano.Node.Tracing.Documentation (
  TraceDocumentationCmd,
  parseTraceDocumentationCmd,
  runTraceDocumentationCmd,
 )
import Control.Concurrent.Async (race_)
import Control.Monad (forever)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Options.Applicative qualified as Opt
import Options.Applicative.Help ((<$$>))
import Paths_marconi_sidechain_node (version)
import System.Info (arch, compilerName, compilerVersion, os)

{- | Most of this is copied from the `Main` file of cardano-node. If we upgrade the cardano-node
version, this needs to be change to reflect the changes in the new cardano-node version.
-}
run :: IO ()
run = do
  Crypto.cryptoInit

  toplevelExceptionHandler $ do
    cmd <- Opt.customExecParser p opts

    case cmd of
      VersionCmd -> runVersionCommand
      TraceDocumentation tdc -> runTraceDocumentationCmd tdc
      RunCmd config ->
        withLedgerEventsChan $ \writer reader ->
          race_
            (runNode config writer)
            (runSidechainChainIndex reader)
  where
    p = Opt.prefs Opt.showHelpOnEmpty

    opts :: Opt.ParserInfo Command
    opts =
      Opt.info
        ( fmap RunCmd nodeCLIParser
            Opt.<|> fmap TraceDocumentation parseTraceDocumentationCmd
            Opt.<|> parseVersionCmd
            Opt.<**> helperBrief "help" "Show this help text" nodeCliHelpMain
        )
        ( Opt.fullDesc
            <> Opt.progDesc "Start node of the Cardano blockchain."
        )

    helperBrief :: String -> String -> String -> Opt.Parser (a -> a)
    helperBrief l d helpText =
      Opt.abortOption (Opt.InfoMsg helpText) $
        mconcat
          [ Opt.long l
          , Opt.help d
          ]

    nodeCliHelpMain :: String
    nodeCliHelpMain =
      renderHelpDoc 80 $
        parserHelpHeader "marconi-sidechain-node" nodeCLIParser
          <$$> ""
          <$$> parserHelpOptions nodeCLIParser

data Command
  = RunCmd PartialNodeConfiguration
  | TraceDocumentation TraceDocumentationCmd
  | VersionCmd

-- Yes! A --version flag or version command. Either guess is right!
parseVersionCmd :: Opt.Parser Command
parseVersionCmd =
  Opt.subparser
    ( mconcat
        [ Opt.commandGroup "Miscellaneous commands"
        , Opt.metavar "version"
        , Opt.hidden
        , command'
            "version"
            "Show the marconi-sidechain-node version"
            (pure VersionCmd)
        ]
    )
    Opt.<|> Opt.flag'
      VersionCmd
      ( Opt.long "version"
          <> Opt.help "Show the marconi-sidechain-node version"
          <> Opt.hidden
      )

runVersionCommand :: IO ()
runVersionCommand =
  Text.putStrLn $
    mconcat
      [ "marconi-sidechain-node "
      , renderVersion version
      , " - "
      , Text.pack os
      , "-"
      , Text.pack arch
      , " - "
      , Text.pack compilerName
      , "-"
      , renderVersion compilerVersion
      ]
  where
    renderVersion = Text.pack . showVersion

command' :: String -> String -> Opt.Parser a -> Opt.Mod Opt.CommandFields a
command' c descr p =
  mconcat
    [ Opt.command c (Opt.info (p Opt.<**> Opt.helper) $ mconcat [Opt.progDesc descr])
    , Opt.metavar c
    ]

runNode :: PartialNodeConfiguration -> LedgerEventWriter -> IO ()
runNode config writer = Node.runNode config [mkLedgerEventHandler writer]

runSidechainChainIndex :: LedgerEventReader -> IO ()
runSidechainChainIndex reader = do
  putStrLn "Getting SDD..."
  forever $ do
    reader >>= \case
      (_, AnchoredEvent _ _ _ _ (LedgerNewEpochEvent (LedgerStakeDistEvent e))) ->
        putStrLn $ "LedgerStakeDistEvent " <> show e
      _otherEvent ->
        pure ()
