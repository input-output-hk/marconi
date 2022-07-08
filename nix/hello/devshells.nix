{
  inputs,
  cell,
}: {
  default = inputs.std.lib.mkShell {
    buildInputs = [
      inputs.nixpkgs.hello
    ];
  };
}