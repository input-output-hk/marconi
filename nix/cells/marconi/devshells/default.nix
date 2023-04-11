{ inputs, cell }@block: rec
{
  default = marconi-shell;

  marconi-shell = import ./marconi-shell.nix block;
}
