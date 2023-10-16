{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Marconi.Sidechain.Node.Run where

import Cardano.Crypto.Init qualified as Crypto
import Cardano.Node.Configuration.POM (PartialNodeConfiguration (pncLedgerEventHandlerPort))
import Cardano.Node.Handlers.TopLevel (toplevelExceptionHandler)
import Cardano.Node.LedgerEvent (
  AnchoredEvent (AnchoredEvent),
  LedgerEvent (LedgerNewEpochEvent),
  LedgerNewEpochEvent (LedgerStakeDistEvent),
  foldEvent,
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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (bracket, bracketOnError)
import Data.Monoid (Last (getLast))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Network.Socket (HostName, ServiceName, Socket)
import Network.Socket qualified as Socket
import Options.Applicative qualified as Opt
import Options.Applicative.Help ((<$$>))
import Paths_marconi_sidechain_node (version)
import System.IO (IOMode (ReadMode))
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
      RunCmd args ->
        -- TODO We should remove the ledgerEventHandler from the node's 'PartialNodeConfiguration'
        -- and instead it put it in our custom CLI datatype. That way, the type will be 'PortNumber'
        -- instead of 'Maybe PortNumber'.
        case getLast $ pncLedgerEventHandlerPort args of
          Nothing -> error "--ledger-event-handler option not provided"
          Just portNumber ->
            race_ (runSidechainChainIndex portNumber) (Node.runNode args)
      TraceDocumentation tdc -> runTraceDocumentationCmd tdc
      VersionCmd -> runVersionCommand
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

runSidechainChainIndex :: Socket.PortNumber -> IO ()
runSidechainChainIndex portNumber = do
  -- TODO Client should wait for socket connection to be established instead of this random
  -- threadDelay.
  -- Or better, we should use LedgerEventHandler directly instead of relying in a socket connection.
  -- This is planned.
  threadDelay 5000000
  runTCPClient "localhost" (show portNumber) $ \sock -> do
    h <- Socket.socketToHandle sock ReadMode

    putStrLn "Getting SDD..."
    foldEvent h () $ \() -> \case
      AnchoredEvent _ _ (LedgerNewEpochEvent (LedgerStakeDistEvent e)) -> print e
      _otherEvent -> pure ()
  where
    runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
    runTCPClient host port client = Socket.withSocketsDo $ do
      addrInfo <- resolve
      putStrLn $ "Connecting to " <> show addrInfo
      bracket (open addrInfo) Socket.close client
      where
        resolve = do
          let hints = Socket.defaultHints{Socket.addrSocketType = Socket.Stream, Socket.addrFamily = Socket.AF_INET}
          head <$> Socket.getAddrInfo (Just hints) (Just host) (Just port)
        open addr = bracketOnError (Socket.openSocket addr) Socket.close $ \sock -> do
          Socket.connect sock $ Socket.addrAddress addr
          return sock

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
