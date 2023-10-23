{
  description = "Marconi Chain Indexer";


  inputs = {

    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2305";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    # Used to provide the cardano-node and cardano-cli executables.
    cardano-node.url = "github:input-output-hk/cardano-node?ref=8.4.0-pre";

    mithril.url = "github:input-output-hk/mithril";
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-linux" "x86_64-darwin" ];
    outputs = import ./nix/outputs.nix;
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
