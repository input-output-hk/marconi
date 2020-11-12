{ pkgs, iohkNix, src, haskell }:
let
  inherit (pkgs) lib;
  cleanSrc = lib.cleanSourceWith {
    filter = lib.cleanSourceFilter;
    inherit src;
    # Otherwise this depends on the name in the parent directory, which reduces caching, and is
    # particularly bad on Hercules, see https://github.com/hercules-ci/support/issues/40
    name = "plutus";
  };
in
pkgs.recurseIntoAttrs {
  shellcheck = pkgs.callPackage iohkNix.tests.shellcheck { src = cleanSrc; };

  stylishHaskell = pkgs.callPackage ./stylish-haskell.nix {
    src = cleanSrc;
    stylish-haskell = haskell.extraPackages.stylish-haskell.components.exes.stylish-haskell;
  };

  purty = pkgs.callPackage ./purty.nix {
    src = cleanSrc;
    purty = haskell.extraPackages.purty.components.exes.purty;
  };

  nixpkgsFmt = pkgs.callPackage ./nixpkgs-fmt.nix {
    src = cleanSrc;
    inherit (pkgs) nixpkgs-fmt;
  };
}
