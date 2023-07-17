# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixhaskell-projectnix

{ inputs, inputs', meta, config, pkgs, lib }:

let

  lib = pkgs.lib;


  # Only a limited subset of components can be cross-compiled on windows.
  # When `isCross` is `true`, it means that we are cross-compiling the project.
  isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;


  sha256map = {
    "https://github.com/input-output-hk/cardano-node"."a158a679690ed8b003ee06e1216ac8acd5ab823d" = "sha256-uY7wPyCgKuIZcGu0+vGacjGw2kox8H5ZsVGsfTNtU0c=";
  };


  packages = {
    # These rely on the plutus-tx-plugin, so they don't cross-compile.
    marconi-chain-index.components.tests.marconi-chain-index-test.buildable = lib.mkForce (!isCross);
    marconi-chain-index.components.sublibs.marconi-chain-index-test-lib.buildable = lib.mkForce (!isCross);

    # These don't cross-compile anymore after updating hackage.
    # There was an error with wai-app-static (a transitive dependency).
    # Last lines of the error logs was:
    #
    # ```
    # ---> Starting remote-iserv.exe on port 8774
    # ---| remote-iserv.exe should have started on 8774
    # Could not find Wine Gecko. HTML rendering will be disabled.
    # wine: configuration in L"/build" has been updated.
    # Listening on port 8774
    # GHC runtime linker: fatal error: I found a duplicate definition for symbol
    #    blake2s
    # whilst processing object file
    #    /nix/store/y31y0za7y6szxb4rzvq7fhmwl1c13qn8-cryptonite-lib-cryptonite-x86_64-w64-mingw32-0.30/lib/x86_64-windows-ghc-9.2.7/cryptonite-0.30-9s0xPy8zQCnKwLSDNLcDtp/HScryptonite-0.30-9s0xPy8zQCnKwLSDNLcDtp.o
    # The symbol was previously defined in
    #    /nix/store/6jlh7fyx3xhnzb503pq6wv7kr0bhq494-crypton-lib-crypton-x86_64-w64-mingw32-0.32/lib/x86_64-windows-ghc-9.2.7/crypton-0.32-7GhYsNRCqn8ASD1vUpQYGB/HScrypton-0.32-7GhYsNRCqn8ASD1vUpQYGB.o
    # This could be caused by:
    #    * Loading two different object files which export the same symbol
    #    * Specifying the same object file twice on the GHCi command line
    #    * An incorrect `package.conf' entry, causing some object to be
    #      loaded twice.
    # remote-iserv.exe: loadObj "/nix/store/y31y0za7y6szxb4rzvq7fhmwl1c13qn8-cryptonite-lib-cryptonite-x86_64-w64-mingw32-0.30/lib/x86_64-windows-ghc-9.2.7/cryptonite-0.30-9s0xPy8zQCnKwLSDNLcDtp/HScryptonite-0.30-9s0xPy8zQCnKwLSDNLcDtp.o": failed
    # iserv-proxy: {handle: <socket: 5>}: GHCi.Message.remoteCall: end of file
    # ghc: ghc-iserv terminated (1)
    # The error is here: https://ci.iog.io/build/265023/nixlog/2
    # ```
    marconi-chain-index.components.sublibs.json-rpc.buildable = lib.mkForce (!isCross);
    marconi-sidechain.package.buildable = !isCross;
    marconi-tutorial.package.buildable = !isCross;

    marconi-core.doHaddock = meta.enableHaddock;
    marconi-core.flags.defer-plugin-errors = meta.enableHaddock;

    marconi-chain-index.doHaddock = meta.enableHaddock;
    marconi-chain-index.flags.defer-plugin-errors = meta.enableHaddock;

    marconi-sidechain.doHaddock = meta.enableHaddock;
    marconi-sidechain.flags.defer-plugin-errors = meta.enableHaddock;

    marconi-tutorial.doHaddock = meta.enableHaddock;
    marconi-tutorial.flags.defer-plugin-errors = meta.enableHaddock;

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
      export CARDANO_CLI=${inputs.cardano-node.legacyPackages.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE=${inputs.cardano-node.legacyPackages.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE_SRC=${../.}
    ";

    # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
    cardano-streaming.ghcOptions = [ "-Werror" ];
    marconi-chain-index.ghcOptions = [ "-Werror" ];
    marconi-core.ghcOptions = [ "-Werror" ];
    marconi-sidechain.ghcOptions = [ "-Werror" ];
    marconi-tutorial.ghcOptions = [ "-Werror" ];
  };


  modules = [{ inherit packages; }];


  project = { inherit sha256map modules; };

in

project
