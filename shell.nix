{ crossSystem ? null
, system ? builtins.currentSystem
, config ? { allowUnfreePredicate = (import ./lib.nix).unfreePredicate; }
, rev ? "in-nix-shell"
, sourcesOverride ? { }
, packages ? import ./. { inherit crossSystem config sourcesOverride rev; }
}:
let
  inherit (packages) pkgs plutus plutusMusl plutus-playground marlowe-playground plutus-pab;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt;
  inherit (plutus) haskell agdaPackages stylish-haskell sphinxcontrib-haddock nix-pre-commit-hooks;
  inherit (plutus) agdaWithStdlib;
  inherit (plutus) purty purty-pre-commit purs spargo;

  # For Sphinx, and ad-hoc usage
  sphinxTools = python3.withPackages (ps: [ sphinxcontrib-haddock.sphinxcontrib-domaintools ps.sphinx ps.sphinx_rtd_theme ]);

  # Configure project pre-commit hooks
  pre-commit-check = nix-pre-commit-hooks.run {
    src = (lib.cleanSource ./.);
    tools = {
      stylish-haskell = stylish-haskell;
      nixpkgs-fmt = nixpkgs-fmt;
      shellcheck = pkgs.shellcheck;
      purty = purty-pre-commit;
    };
    hooks = {
      purty.enable = true;
      stylish-haskell.enable = true;
      nixpkgs-fmt = {
        enable = true;
        # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
        # does not appear to work inside the hook. For now we have to thus
        # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
        excludes = [ ".*nix/stack.materialized/.*" ".*nix/sources.nix$" ".*/spago-packages.nix$" ".*/packages.nix$" ];
      };
      shellcheck.enable = true;
    };
  };

  # build inputs from nixpkgs ( -> ./nix/default.nix )
  nixpkgsInputs = (with pkgs; [
    # pkgs.sqlite-analyzer -- Broken on 20.03, needs a backport
    awscli
    cacert
    ghcid
    niv
    nixpkgs-fmt
    nodejs
    pass
    shellcheck
    sqlite-interactive
    stack
    terraform
    yubikey-manager
    z3
    zlib
  ] ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));

  plutus-playground-generate-purs = pkgs.writeShellScriptBin "plutus-playground-generate-purs" ''
    rm -rf ./generated

    if [[ $1 = "-r" ]]; then
      $(nix-build --quiet --no-build-output ../default.nix -A plutus-playground.server-invoker)/bin/plutus-playground psgenerator generated
    else
      ${plutus-playground.server-invoker}/bin/plutus-playground psgenerator generated
    fi
  '';
  plutus-playground-server = pkgs.writeShellScriptBin "plutus-playground-server" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    ${plutus-playground.server-invoker}/bin/plutus-playground webserver
  '';
  marlowe-playground-generate-purs = pkgs.writeShellScriptBin "marlowe-playground-generate-purs" ''
    rm -rf ./generated

    if [[ $1 = "-r" ]]; then
      $(nix-build ../default.nix --quiet --no-build-output -A marlowe-playground.server-invoker)/bin/marlowe-playground psgenerator generated
    else
      ${marlowe-playground.server-invoker}/bin/marlowe-playground psgenerator generated
    fi
  '';
  marlowe-playground-server = pkgs.writeShellScriptBin "marlowe-playground-server" ''
    export FRONTEND_URL=https://localhost:8009
    ${marlowe-playground.server-invoker}/bin/marlowe-playground webserver
  '';
  plutus-scb-generate-purs = pkgs.writeShellScriptBin "plutus-scb-generate-purs" ''
    rm -rf ./generated
    cp ${haskell.packages.plutus-scb.src}/plutus-scb.yaml.sample plutus-scb.yaml

    if [[ $1 = "-r" ]]; then
      $(nix-build ../default.nix --quiet --no-build-output -A plutus-scb.server-invoker)/bin/plutus-scb psgenerator generated
    else
      ${plutus-scb.server-invoker}/bin/plutus-scb psgenerator generated
    fi
  '';
  plutus-scb-server = pkgs.writeShellScriptBin "plutus-scb-server" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    rm -rf ./generated
    cp ${haskell.packages.plutus-scb.src}/plutus-scb.yaml.sample plutus-scb.yaml
    ${plutus-scb.server-invoker}/bin/plutus-scb webserver
  '';


  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with plutus; [
    cabal-install
    fixPurty
    fixStylishHaskell
    haskell-language-server
    hie-bios
    gen-hie
    hlint
    marlowe-playground-generate-purs
    marlowe-playground-server
    plutus-scb-generate-purs
    plutus-scb-server
    plutus-playground-generate-purs
    plutus-playground-server
    purs
    purty
    spago
    stylish-haskell
    updateHie
    updateClientDeps
    updateMetadataSamples
  ]);

in
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs ++ localInputs ++ [ agdaWithStdlib sphinxTools ];
  # We don't currently use this, and it's a pain to materialize, and otherwise
  # costs a fair bit of eval time.
  withHoogle = false;

  # we have a local passwords store that we use for deployments etc.
  PASSWORD_STORE_DIR = toString ./. + "/secrets";

  shellHook = ''
    ${pre-commit-check.shellHook}
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix-shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$
  '';
}
