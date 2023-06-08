{ inputs, cell }:

# We use the same fourmolu version provided by the haskell-language-server plugin
# 'hls-fourmolu-plugin'. We *can* use a more up-to-date version of fourmolu, but the formatting is a
# little different between major versions. Because of that, you won't get the same formatting when
# using the `fourmolu` or `fix-fourmolu` commands compared to formatting directly with HLS.
#
# To prevent this issue, we explicitly pin the fourmolu version to the one provided by HLS in
# ../library/haskell-language-server-project.nix. See the note there for a similar explanation.
cell.library.haskell-language-server-project.hsPkgs.fourmolu.components.exes.fourmolu
