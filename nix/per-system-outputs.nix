{ inputs, pkgs, ... }:
{
  packages.testrev = pkgs.writeText "asd" ''
    ${inputs.self.rev}
  '';
}
