{ inputs, cell }:

let
  cardano-node-compat = import inputs.flake-compat {
    inherit (cell.library) pkgs;
    src = builtins.fetchTree {
      type = "github";
      owner = "input-output-hk";
      repo = "cardano-node";
      rev = "a158a679690ed8b003ee06e1216ac8acd5ab823d";
      narHash = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
    };
  };
in
cardano-node-compat.defaultNix.packages.${cell.library.pkgs.system}
