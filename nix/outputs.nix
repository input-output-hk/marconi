{ repoRoot, inputs, pkgs, lib, system, ... }:
let
  cabalProject = repoRoot.nix.cabal-project;
in
[
  {
    inherit cabalProject;
  }
  {
    devShells.default = cabalProject.iogx.devShell;
  }
  {
    packages = cabalProject.iogx.packages;
  }
  {
    packages.pre-commit-check = cabalProject.iogx.pre-commit-check;
    packages.read-the-docs-site = cabalProject.iogx.read-the-docs-site;
  }
  {
    checks = cabalProject.iogx.checks;
  }
  {
    apps = cabalProject.iogx.apps;
  }
  {
    hydraJobs.native = cabalProject.iogx.hydraJobs;
    hydraJobs.pre-commit-check = cabalProject.iogx.pre-commit-check;
    hydraJobs.read-the-docs-site = cabalProject.iogx.read-the-docs-site;
    hydraJobs.required = lib.iogx.mkHydraRequiredJob;
  }
  (lib.optionalAttrs (system == "x86_64-linux")
    {
      hydraJobs.mingwW64 = inputs.self.cabalProject.projectCross.mingwW64.iogx.hydraJobs;
    })
]
