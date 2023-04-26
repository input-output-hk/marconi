{ stdenv, lib, pythonPackages, sphinxcontrib-domaintools, sphinxcontrib-haddock, sphinx-markdown-tables, sphinxemoji, combined-haddock, ... }:
stdenv.mkDerivation {
  name = "plutus-docs";
  src = lib.sourceFilesBySuffices ./. [ ".py" ".rst" ".md" ".hs" ".png" ".svg" ".bib" ".csv" ".css" ".html" "txt" ];
  buildInputs = with pythonPackages; [
    sphinx
    sphinx_rtd_theme
    sphinxcontrib-domaintools
    sphinxcontrib-haddock
    sphinx-markdown-tables
    sphinxcontrib_plantuml
    sphinxcontrib-bibtex
    sphinxemoji
    recommonmark
  ];
  buildPhase = ''
    sphinx-build -n -W . $out
  '';
  dontInstall = true;
}
