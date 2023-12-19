{ repoRoot, inputs, pkgs, lib, system }:

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

      compiler-nix-name = "ghc928";

      flake.variants.profiled.modules = [{
        enableProfiling = true;
        enableLibraryProfiling = true;
      }];

      shell.withHoogle = false;

      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
      };

      modules = [{
        packages = {
          marconi-core.doHaddock = false;
          marconi-core.flags.defer-plugin-errors = false;

          marconi-cardano-core.doHaddock = false;
          marconi-cardano-core.flags.defer-plugin-errors = false;

          marconi-cardano-indexers.doHaddock = false;
          marconi-cardano-indexers.flags.defer-plugin-errors = false;

          marconi-cardano-chain-index.doHaddock = false;
          marconi-cardano-chain-index.flags.defer-plugin-errors = false;

          marconi-sidechain.doHaddock = false;
          marconi-sidechain.flags.defer-plugin-errors = false;

          marconi-sidechain-experimental.doHaddock = false;
          marconi-sidechain-experimental.flags.defer-plugin-errors = false;

          marconi-starter.doHaddock = false;
          marconi-starter.flags.defer-plugin-errors = false;

          marconi-core-legacy.doHaddock = false;
          marconi-core-legacy.flags.defer-plugin-errors = false;

          marconi-chain-index-legacy.doHaddock = false;
          marconi-chain-index-legacy.flags.defer-plugin-errors = false;

          # The lines `export CARDANO_NODE=...` and `export CARDANO_CLI=...`
          # is necessary to prevent the error
          # `../dist-newstyle/cache/plan.json: openBinaryFile: does not exist (No such file or directory)`.
          # See https://github.com/input-output-hk/cardano-node/issues/4194.
          marconi-cardano-chain-index.preCheck = "
            export CARDANO_CLI=${inputs.cardano-node.legacyPackages.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
            export CARDANO_NODE=${inputs.cardano-node.legacyPackages.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
            export MARCONI_CHAIN_INDEX=${inputs.self.packages.marconi-cardano-chain-index}/bin/marconi-cardano-chain-index
          ";

          # Needed for running the marconi-sidechain integration tests in CI
          marconi-sidechain.preCheck = "
            export MARCONI_SIDECHAIN=${inputs.self.packages.marconi-sidechain}/bin/marconi-sidechain
          ";

          # CARDANO_NODE_CONFIG needed for tests of handlers, which include ExtLedgerStateCoordinator.
          # The coordinator's build function calls `readGenesisFile` so must know where to look.
          marconi-sidechain-experimental.preCheck = "
            export MARCONI_SIDECHAIN_EXPERIMENTAL=${inputs.self.packages.marconi-sidechain-experimental}/bin/marconi-sidechain-experimental
            export CARDANO_NODE_CONFIG=${../config}
          ";

          # Needed for running marconi-cardano-indexers snapshot tests in CI
          marconi-cardano-indexers.preCheck = "
            export CARDANO_NODE_CONFIG=${../config}
          ";

          # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
          marconi-core.ghcOptions = [ "-Werror" ];
          marconi-cardano-core.ghcOptions = [ "-Werror" ];
          marconi-cardano-indexers.ghcOptions = [ "-Werror" ];
          marconi-cardano-chain-index.ghcOptions = [ "-Werror" ];
          marconi-sidechain.ghcOptions = [ "-Werror" ];
          marconi-sidechain-experimental.ghcOptions = [ "-Werror" ];
          marconi-starter.ghcOptions = [ "-Werror" ];
        };
      }];
    });


  cabalProject = cabalProject'.appendOverlays [
    (_: prev: {
      hsPkgs = prev.pkgs.pkgsHostTarget.setGitRevForPaths prev.pkgs.gitrev [
        "marconi-chain-index-legacy.components.exes.marconi-chain-index-legacy"
        "marconi-cardano-chain-index.components.exes.marconi-cardano-chain-index"
        "marconi-sidechain.components.exes.marconi-sidechain"
        "marconi-sidechainexperimental.components.exes.marconi-sidechain-experimental"
      ]
        prev.hsPkgs;
    })
  ];


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    includeMingwW64HydraJobs = false;
    shellArgs = repoRoot.nix.shell;
    readTheDocs = {
      enable = true;
      siteFolder = "doc/read-the-docs-site";
    };
  };

in

project
