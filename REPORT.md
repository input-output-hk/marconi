Here are the findings obtained after creating a new `std`-enabled repository and trying to build the marconi project:

## `std` [The standard nix tool](https://github.com/divnix/std)

- **Pros**
  - Provides a TUI for interacting with the repository artifacts.
    Very useful for projects with deployable components, less useful for the plutus repos, where we are concerned mostly with packaging haskell code.
    Still it doens't hurt to have it.
  - Provides an abstraction layer that simplifies the writing of flake-based nix code, and 
    imposes a disciplined organization of nix files.
    This is where most of its value comes from, as a standard for creating more maintainable nix.
  - It's the new standard that most repos inside IOG are moving toward.
    This gives consistency across repositories, making them easier to reason about.
- **Cons** 
  - More cutting-edge and novel and therefore less stable than just using flakes
  - Steep learning curve, additional cognitive overhead for new maintainers.
- **Conclusion**
  - Integrating `std` is worth the effort. It doesn't cost much to layer it on top of an ordinary nix flake, we can be selective about which features to use, and it enforces a sensible and structured organization of the code.


## Nix `flakes` 
- **Pros** 
  - Better DevX thanks to the ability to install software in one line, e.g.
  `nix install github:input-output-hk/some-repo/some-haskell-component` 
  - Better DevX thanks to the ability to automatically setup binary caches via the`nixConfig` field inside the flake 
  - Better DevX thanks to reduced times to enter the shell due to caching of derivations (to be verified, as the final setup might not trigger a cache hit in practice).
  - A standardized and community-approved approach at organizing nix code. It provides a uniform interface for creating shells and packaging code.
  - Many IOG repos are using flakes or are in the process of doing so.
    This gives consistency across repositories, making them easier to reason about.
- **Cons** 
  - No obious cons, except for the refactoring costs 
- **Conclusion**
  - Moving to a fully flake-based setup would improve DevX in some areas, and would simplify the nix code overall.


## Separate `marconi` repo

- **Pros** 
  - Better discoverability 
  - Ability to depend on separate version of plutus-apps
- **Cons** 
  - Involves a sizeable amount of duplication, which results in heavy maintenance costs.
    We would have the same code in 3 repositories.

