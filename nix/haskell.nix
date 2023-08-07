{
  supportedCompilers = [ "ghc928" ];
  enableCrossCompilation = true;
  defaultChangelogPackages = [
    "cardano-straming"
    "marconi-chain-index"
    "marconi-core"
    "marconi-sidechain"
  ];
}
