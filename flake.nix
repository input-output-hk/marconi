{ 
  description = "WIP";

  inputs = {
    std.url = "github:divnix/std";
  };

  outputs = { std, ... }@inputs:
    std.grow {
      inherit inputs;
      cellsFrom = ./nix;
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
