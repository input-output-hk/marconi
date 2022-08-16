{ 
  description = "Marconi Chain Indexer";

  inputs = {
    nixpkgs = {
      url = github:NixOS/nixpkgs/30d3d79b7d3607d56546dd2a6b49e156ba0ec634;
    };
    std = {
      url = "github:divnix/std/b8a1db5f04e20b2e929572fe483dbd97dc23a9b8";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
    nixpkgs-haskell.follows = "haskell-nix/nixpkgs-unstable";
  };

  outputs = { std, ... }@inputs:
    std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      organelles = [
        (inputs.std.devshells "devshells")
        (inputs.std.installables "packages")
      ];
    }
    {
      devShells = inputs.std.harvest inputs.self ["marconi" "devshells"];
      packages = inputs.std.harvest inputs.self ["marconi" "packages"];
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra.iohk.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
