{ repoRoot, inputs, ... }:

_cabalProject:

let
  # cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;
in

{
  name = "marconi-sidechain-node";

  packages = [
    # cardano-cli
    cardano-node
    inputs.mithril.packages.mithril-client
  ];

  scripts = { };

  env = {
    # CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };

  preCommit = {
    fourmolu.enable = true;
    shellcheck.enable = false;
    cabal-fmt.enable = true;
    optipng.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
