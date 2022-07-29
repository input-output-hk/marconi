{
  inputs,
  cell,
}: {
  # One `std` is the input, the other one is the cell
  default = inputs.std.std.lib.mkShell {
    
    name = "marconi-devshell"; # mandatory

    # see also: https://github.com/numtide/devshell/pull/6
    packages = [
      inputs.nixpkgs.haskell.packages.ghc8107.cabal-install
      inputs.nixpkgs.haskell.compiler.ghc8107

      inputs.nixpkgs.zlib
    ];

    imports = [
      #inputs.std.std.devshellProfiles.default
    ];
  };
}


