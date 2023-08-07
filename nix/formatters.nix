# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixformattersnix

{
  fourmolu.enable = true;
  shellcheck.enable = false;
  cabal-fmt.enable = true;
  png-optimization.enable = true;
  nixpkgs-fmt.enable = true;
}
