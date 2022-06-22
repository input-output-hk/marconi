{
  description = "Marconi Project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs }: {
    
    defaultPackage.x86_64-linux = nixpkgs.stdenv.mkDerivation {
      name = "hello";
      src = self;
      buildPhase = "gcc -o hello ./hello.c";
      installPhase = "mkdir -p $out/bin; install -t $out/bin hello";
    };
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
  };
}