{
  # The description attribute is a one-line description shown by nix flake metadata.
  description = "TODO Marconi Project Description";

  # The inputs attribute specifies other flakes that this flake depends on. 
  # These are fetched by Nix and passed as arguments to the outputs function.
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;
  };

  # The outputs attribute is the heart of the flake: it’s a function that 
  # produces an attribute set. The function arguments are the flakes specified 
  # in inputs. The self argument denotes this flake. Its primarily useful for 
  # referring to the source of the flake (as in src = self;) or to other outputs 
  # (e.g. self.defaultPackage.x86_64-linux).
  # 
  # The attributes produced by outputs are arbitrary values, except that there 
  # are some standard outputs such as defaultPackage.${system}
  # Every flake has some metadata, such as self.lastModifiedDate, which is used 
  # to generate a version string like hello-20191015
  outputs = { self, nixpkgs, ... }: {

    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra.iohk.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  # Any subsequent build of this flake will use the version of nixpkgs recorded 
  # in the lock file. If you add new inputs to flake.nix, when you run any 
  # command such as nix build, Nix will automatically add corresponding locks to 
  # flake.lock. However, it won’t replace existing locks. If you want to update 
  # a locked input to the latest version, you need to ask for it.
  # 
  # nix flake lock --update-input nixpkgs
  # nix build 

  # Making sure that everything is in order:
  # nix flake check
}
