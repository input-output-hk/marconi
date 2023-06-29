# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ inputs, inputs', pkgs, project }:

let
  cardano-cli = project.hsPkgs.cardano-cli.components.exes.cardano-cli;
  cardano-node = project.hsPkgs.cardano-node.components.exes.cardano-node;
in
{
  name = "marconi";

  packages = [ cardano-cli cardano-node ];

  env = {
    CARDANO_CLI = pkgs.lib.getExe cardano-cli;
    CARDANO_NODE = pkgs.lib.getExe cardano-node;
  };
}
