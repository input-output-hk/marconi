{
  inputs,
  cell,
}: {

  haskellNixProject = 
    let 
      sources = import inputs.nixpkgs { 
        config = inputs.haskell-nix.config;
        overlays = [ inputs.haskell-nix.overlay ];
      };
    in 
      sources.haskell-nix.hackage-project {
        src = ../../../src;
        name = "cabal-install";
        version = "3.6.2.0";
        compiler-nix-name = "ghc8107";
        index-state = "2022-02-22T20:47:03Z";
      };
      
  default = inputs.nixpkgs.stdenv.mkDerivation rec {
    pname = "hello";
    version = "2.10";
    src = inputs.nixpkgs.fetchurl {
      url = "mirror://gnu/hello/${pname}-${version}.tar.gz";
      sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
    };
  };
}