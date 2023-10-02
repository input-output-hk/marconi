# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#32-nixhaskellnix

{
  supportedCompilers = [ "ghc928" ];

  enableCrossCompilation = true;

  defaultChangelogPackages = [
    "marconi-chain-index"
    "marconi-core"
    "marconi-core-json-rpc"
    "marconi-sidechain"
    "marconi-tutorial"
  ];
}
