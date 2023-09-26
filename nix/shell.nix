{ repoRoot, inputs, ... }:

cabalProject:

let
  cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;
in

{
  name = "marconi";

  packages = [
    cardano-cli
    cardano-node
    inputs.mithril.packages.mithril-client
  ];

  scripts = {
    start-benchmark-machine = {
      enable = false;
      exec = repoRoot.scripts.start-benchmarking-machine cabalProject;
      description = ''
        Start the benchmarking NixOS VM exposing Grafana dashboards and Prometheus metrics for Marconi.
      '';
      group = "benchmarking";
    };
  };

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
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
