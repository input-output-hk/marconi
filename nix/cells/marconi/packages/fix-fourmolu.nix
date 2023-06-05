# TODO(std) DUP

{ inputs, cell }:

cell.library.pkgs.writeShellApplication {

  name = "fix-fourmolu";

  runtimeInputs = [
    cell.library.pkgs.fd
    cell.packages.fourmolu
  ];

  text = ''
    fd \
      --extension hs \
      --exclude 'dist-newstyle/*' \
      --exclude 'dist/*' \
      --exclude '.stack-work/*' \
      --exec bash -c "fourmolu -iq {}"
  '';
}

