{ repoRoot, inputs, pkgs, ... }:

_cabalProject:

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
    pkgs.ghcid
    pkgs.haskellPackages.hoogle
  ];

  scripts = {
    start-benchmark-machine = {
      enable = false;
      group = "benchmarking";
      exec = repoRoot.nix.scripts.start-benchmarking-machine;
      description = ''
        Start the benchmarking NixOS VM exposing Grafana dashboards and Prometheus metrics for Marconi.
      '';
    };
  };

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
    CARDANO_NODE_CONFIG = ../config;
  };

  preCommit = {
    fourmolu.enable = true;
    shellcheck.enable = false;
    cabal-fmt.enable = true;
    optipng.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
