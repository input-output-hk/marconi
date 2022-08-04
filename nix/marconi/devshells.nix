{
  inputs,
  cell,
}: {

  # FIXME zlib doesn't get added to the ENV property and cabal fails to build with 
  # Configuring library for digest-0.0.1.3..
  # cabal: Missing dependency on a foreign library:
  # * Missing (or bad) header file: zlib.h
  # * Missing (or bad) C library: z
  broken-default = inputs.std.std.lib.mkShell {
    
    name = "marconi-devshell";

    # see also: https://github.com/numtide/devshell/pull/6
    packages = [
      inputs.nixpkgs.haskell.packages.ghc8107.cabal-install
      inputs.nixpkgs.haskell.compiler.ghc8107

      inputs.nixpkgs.zlib
    ];

    imports = [
      # inputs.std.std.devshellProfiles.default
    ];
  };


  default = inputs.nixpkgs.stdenv.mkDerivation {
    name = "marconi-devshell";

    buildInputs = [
      inputs.nixpkgs.haskell.packages.ghc8107.cabal-install
      inputs.nixpkgs.haskell.compiler.ghc8107
      inputs.nixpkgs.pkgconfig

      inputs.nixpkgs.zlib # Needed by the digest package
      inputs.nixpkgs.libsodium # Needed by cardano-crypto-class and cardano-crypto-praos
    ];
  };
}


