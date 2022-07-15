rec {
  description = "VS Code Haskell Template";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/c01480f0a32d8584e60f128b9c7fc6cb98d64ede;
    std = {
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };  
  };

  outputs = inputs:
    inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      organelles = [
        (inputs.std.installables "organelle1")
      ];
    }
    {
      packages = inputs.std.harvest inputs.self ["cell1" "organelle1"];
    };

  nixConfig = {
    bash-prompt = "\[nix-develop\]$ ";
  };
}
