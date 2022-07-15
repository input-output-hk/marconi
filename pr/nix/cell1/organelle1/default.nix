{ inputs, cell }:
{
  installable1 = import ./installable1.nix inputs;

  broken-installable = import ./broken-installable.nix inputs;
}