{ 
  description = "WIP";

  inputs.std.url = "github:divnix/std";
  inputs.std.inputs.nixpkgs.follows = "nixpkgs";
  
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;

  outputs = { std, ... }@inputs:
    std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      organelles = [
        (inputs.std.devshells "devshells")
      ];
    }
    {
      devShells = inputs.std.harvest inputs.self ["hello" "devshells"];
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
