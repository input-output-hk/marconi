{ inputs, cell }@block:
{
  sphinx-markdown-tables = import ./sphinx-markdown-tables.nix block;

  sphinx-toolchain = import ./sphinx-toolchain.nix block;

  sphinxcontrib-bibtex = import ./sphinxcontrib-bibtex.nix block;

  sphinxemoji = import ./sphinxemoji.nix block;

  read-the-docs-site = import ./read-the-docs-site.nix block;

  build-readthedocs-site = import ./build-readthedocs-site.nix block;

  serve-readthedocs-site = import ./serve-readthedocs-site.nix block;

  cabal-install = import ./cabal-install.nix block;

  check-the-flake = import ./check-the-flake.nix block;

  cabal-fmt = import ./cabal-fmt.nix block;

  fix-cabal-fmt = import ./fix-cabal-fmt.nix block;

  fix-png-optimization = import ./fix-png-optimization.nix block;

  fix-stylish-haskell = import ./fix-stylish-haskell.nix block;

  ghc = import ./ghc.nix block;

  haskell-language-server-wrapper = import ./haskell-language-server-wrapper.nix block;

  hie-bios = import ./hie-bios.nix block;

  hlint = import ./hlint.nix block;

  nixpkgs-fmt = import ./nixpkgs-fmt.nix block;

  pre-commit-check = import ./pre-commit-check.nix block;

  repo-root = import ./repo-root.nix block;

  stylish-haskell = import ./stylish-haskell.nix block;

  marconi-chain-index = import ./marconi-chain-index.nix block;

  marconi-sidechain = import ./marconi-sidechain.nix block;

  cardano-wallet = import ./cardano-wallet.nix block;

  scriv = import ./scriv.nix block;

  inherit (import ./sphinxcontrib-haddock.nix block)

    sphinxcontrib-domaintools

    sphinxcontrib-haddock

    sphobjinv;

}
