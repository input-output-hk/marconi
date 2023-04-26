# Plutus documentation site

This is a sphinx site.

Run `nix develop` to enter a development shell with `sphinx-build` and `sphinx-autobuild`.

The following commands are also available:

- `build-readthedocs-site`
  Build the docs locally in `_build/index.html`
- `serve-readthedocs-site`
  Start a development server with live reload on `http://localhost:8000`

The doc site from main is built automatically and hosted [here](https://marconi.readthedocs.io).

Additionally, the site is built for all PRs, and a link to a preview can be found in the PR statuses.
