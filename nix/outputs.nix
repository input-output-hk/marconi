{ repoRoot, inputs, pkgs, lib, system, ... }:
let
  cabalProject = repoRoot.nix.project;
in
[
  {
    inherit cabalProject;

    devShells.default = cabalProject.iogx.devShell;
    checks = cabalProject.iogx.checks;
    apps = cabalProject.iogx.apps;
    packages = cabalProject.iogx.packages;
  }
  {
    packages.pre-commit-check = cabalProject.iogx.pre-commit-check;
    packages.read-the-docs-site = cabalProject.iogx.read-the-docs-site;
  }
  {
    hydraJobs.native = cabalProject.iogx.hydraJobs;
    hydraJobs.pre-commit-check = cabalProject.iogx.pre-commit-check;
    hydraJobs.read-the-docs-site = cabalProject.iogx.read-the-docs-site;
    hydraJobs.required = lib.iogx.mkHydraRequiredJob { };
  }
  (lib.optionalAttrs (system == "x86_64-linux")
    {
      hydraJobs.mingwW64 = cabalProject.projectCross.mingwW64.iogx.hydraJobs;
    })
]
