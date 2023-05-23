{ inputs, cell }:

let
  cardano-node-compat = import inputs.flake-compat {
    inherit (cell.library) pkgs;
    src = builtins.fetchTree {
      type = "github";
      owner = "input-output-hk";
      repo = "cardano-node";
      ref = "8.0.0";
      narHash = "sha256-uIgWw/VQN/bf9UjazqxW37YrNWqSgnHi27G+bKYcTvk=";
    };
  };
in
cardano-node-compat.defaultNix.packages.${cell.library.pkgs.system}
