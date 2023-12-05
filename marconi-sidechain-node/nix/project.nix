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
        "https://github.com/input-output-hk/cardano-node"."5071bf4b88a035a843e20ebb5fbdefdc99ab839e" = "1c4vqh6hk7hbwja7v3fl918bhdr94r4jn439n8c20nxcin5l0zai";
        "https://github.com/input-output-hk/ouroboros-consensus"."26cfb9666efd78692912fbdcc41e927f64f282ae" = "0p4axag77by7bwc3wnbdphpc6ib57b0di4clzcb8c0za0gf1sw46";
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
