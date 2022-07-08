{
  inputs,
  cell,
}: {
  # one `std` is the input, the other one is the cell
  default = inputs.std.std.lib.mkShell {
    name = "Hello Devshell"; # mandatory
    # this is a numtide/devshell, not a nixpkgs.mkShell
    # buildInputs = [
    #   inputs.nixpkgs.hello
    # ];
    #
    # see also: https://github.com/numtide/devshell/pull/6
    packages = [
      inputs.nixpkgs.hello
    ];

    imports = [
      inputs.std.std.devshellProfiles.default
    ];
  };
}
