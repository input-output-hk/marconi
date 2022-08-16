{
  inputs,
  cell,
}: {

  default = inputs.std.std.lib.mkShell {
    
    name = "test-devshell";

    packages = [
      inputs.nixpkgs.haskell.packages.ghc8107.cabal-install
      inputs.nixpkgs.haskell.compiler.ghc8107
    ];

    imports = [
      inputs.std.std.devshellProfiles.default
    ];
  };
}


