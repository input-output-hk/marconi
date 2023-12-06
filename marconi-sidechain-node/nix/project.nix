{ repoRoot, inputs, pkgs, lib, ... }:

let

  cabalProject' = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }:
    let
      # Only a limited subset of components can be cross-compiled on windows.
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      name = "marconi";

      src = ../.;

      compiler-nix-name = "ghc962";

      shell.withHoogle = false;

      sha256map = {
        "https://github.com/input-output-hk/marconi"."25670eb6f2d89b26b6e173de268bdfb33c49a2d2" = "05my78gmd6hyxhv8vrblr5p9i99cqlii76idkqy2h7s3wr4lnrjw";
        "https://github.com/input-output-hk/cardano-node"."f118fa8c34c601bb8148ad33cc99afa915001d13" = "1qccckj57z95nlgzc3a358g0bl9cqllk9sapyrsll122agqm2my1";
        "https://github.com/input-output-hk/ouroboros-consensus"."79da9a368cb6d2e7ed5ff5e89bb318b94c3c606d" = "02wjbvgbfdr5rybncxgx6slxh4gzx1vh4i34n6h834qghiq4yykb";
      };

      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
      };

      modules = [{
        packages = {
          # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
          marconi-sidechain-node.ghcOptions = [ "-Werror" ];
        };
      }];
    });


  cabalProject = cabalProject'.appendOverlays [
    (_: prev: {
      hsPkgs = prev.pkgs.pkgsHostTarget.setGitRevForPaths prev.pkgs.gitrev [
        "marconi-sidechain-node.components.exes.marconi-sidechain-node"
      ]
        prev.hsPkgs;
    })
  ];


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    includeMingwW64HydraJobs = false;
    shellArgs = repoRoot.nix.shell;
  };

in

project
