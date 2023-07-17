# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Marconi Chain Indexer";


  inputs = {

    iogx.url = "github:input-output-hk/iogx";
    iogx.inputs.CHaP.follows = "CHaP_2";

    CHaP_2 = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    # Used to provide the cardano-node and cardano-cli executables.
    cardano-node = {
      url = "github:input-output-hk/cardano-node";
      flake = false;
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake inputs ./.;


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
