# TODO: replace with shellFor once our pinned nixpkgs advances past
# 5523ec8f3c78704c6e76b7675bfce41d24a3feb1, before which it doesn't
# handle overridden dependencies properly
let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

let
  plutusPkgs = import ./. {};
  ghc = plutusPackages.ghcWithPackages (ps: with ps; [
    language-plutus-core
    core-to-plc
    plutus-th
    tasty-hedgehog
    tasty
    tasty-golden
    tasty-hunit
    hedgehog
  ]);
  fixStylishHaskell = pkgs.stdenv.mkDerivation {
    name = "fix-stylish-haskell";
    buildInputs = with pkgs; [ haskellPackages.stylish-haskell git fd ];
    shellHook = ''
      git diff > pre-stylish.diff
      fd --extension hs --exclude '*/dist/*' --exclude '*/docs/*' --exec stylish-haskell -i {}
      git diff > post-stylish.diff
      diff pre-stylish.diff post-stylish.diff > /dev/null
      if [ $? != 0 ]
      then
        echo "Changes by stylish have been made. Please commit them."
      else
        echo "No stylish changes were made."
      fi
      rm pre-stylish.diff post-stylish.diff
      exit
    '';
  };

in
  # This is an environment for running the deps regeneration script.
  pkgs.stdenv.mkDerivation {
    name = "plutus-ghc";
    passthru = { inherit fixStylishHaskell; };
    buildInputs = with pkgs; [ ghc ];
    src = null;
  }
