{
  inputs,
  cell,
}: {

  haskellNixProject = 
    let 
      sources = import inputs.nixpkgs-haskell { 
        system = inputs.nixpkgs.system;
        config = inputs.haskell-nix.config;
        overlays = [ inputs.haskell-nix.overlay ];
      };
    in 
      sources.haskell-nix.cabalProject {
        src = inputs.self + ./src;
        name = "cabal-install";
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


