{ 
  description = "WIP";

  inputs = {
    std.url = "github:divnix/std";
    nixpkgs.url = github:NixOS/nixpkgs;
  };

  outputs = { std, ... }@inputs:
    std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      # organelles = [
      #   (inputs.std.clades.devshells "devshells")
      # ];
    }
    {
      # devShells = inputs.std.harvest inputs.self ["automation" "devshells"];
      # packages = inputs.std.harvest inputs.self ["std" "cli"];
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
