# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ inputs, inputs', pkgs, project }:

let
  cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;
in
{
  name = "marconi";

  packages = [ cardano-cli cardano-node ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };
}
