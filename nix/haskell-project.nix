# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixhaskell-projectnix

{ inputs, inputs', meta, config, pkgs, lib }:

let

  lib = pkgs.lib;


  isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;


  sha256map = {
    "https://github.com/input-output-hk/cardano-node"."a158a679690ed8b003ee06e1216ac8acd5ab823d" = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
  };


  packages = {
    # These rely on the plutus-tx-plugin
    marconi-sidechain.package.buildable = !isCross;
    marconi-chain-index.components.tests.marconi-chain-index-test.buildable = lib.mkForce (!isCross);
    marconi-chain-index.components.sublibs.marconi-chain-index-test-lib.buildable = lib.mkForce (!isCross);

    marconi-core.doHaddock = meta.enableHaddock;
    marconi-core.flags.defer-plugin-errors = meta.enableHaddock;

    marconi-chain-index.doHaddock = meta.enableHaddock;
    marconi-chain-index.flags.defer-plugin-errors = meta.enableHaddock;

    marconi-sidechain.doHaddock = meta.enableHaddock;
    marconi-sidechain.flags.defer-plugin-errors = meta.enableHaddock;

    # The lines `export CARDANO_NODE=...` and `export CARDANO_CLI=...`
    # is necessary to prevent the error
    # `../dist-newstyle/cache/plan.json: openBinaryFile: does not exist (No such file or directory)`.
    # See https://github.com/input-output-hk/cardano-node/issues/4194.
    #
    # The line 'export CARDANO_NODE_SRC=...' is used to specify the
    # root folder used to fetch the `configuration.yaml` file (in
    # marconi, it's currently in the
    # `configuration/defaults/byron-mainnet` directory.
    # Else, we'll get the error
    # `/nix/store/ls0ky8x6zi3fkxrv7n4vs4x9czcqh1pb-marconi/marconi/test/configuration.yaml: openFile: does not exist (No such file or directory)`
    marconi-chain-index.preCheck = "
      export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE_SRC=${../.}
    ";

    # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
    cardano-streaming.ghcOptions = [ "-Werror" ];
    marconi-chain-index.ghcOptions = [ "-Werror" ];
    marconi-core.ghcOptions = [ "-Werror" ];
    marconi-sidechain.ghcOptions = [ "-Werror" ];
  };


  modules = [{ inherit packages; }];


  project = { inherit sha256map modules; };

in

project 
