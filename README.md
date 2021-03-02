[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/poseidon-framework/poseidon-hs/CI)](https://github.com/poseidon-framework/poseidon-hs/actions?query=workflow%3ACI)
[![Coverage Status](https://img.shields.io/codecov/c/github/poseidon-framework/poseidon-hs/master.svg)](https://codecov.io/github/poseidon-framework/poseidon-hs?branch=master)
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/poseidon-framework/poseidon-hs?include_prereleases) ![GitHub all releases](https://img.shields.io/github/downloads/poseidon-framework/poseidon-hs/total)](https://github.com/poseidon-framework/poseidon-hs/releases)

# poseidon-hs
A toolset to work with modular genotype databases formatted using Poseidon. The main executable within this package is called `trident`.

Detailed documentation can be found on our [github-page](https://poseidon-framework.github.io/#/trident)

## Development Quickstart

You can install the internal documentation using `stack haddock` and open it subsequently using `stack haddock --open`. This will then open a HTML page with all dependency packages and the `poseidon-hs` library itself. The critical package is the `Poseidon.Package` module which defines the core functions to read and work with module files.

Important packages to look into to understand the architecture of this tool:

* Start with `Poseidon.Package`: It defines the main package format and provides some functions how to access the data inside packages.
* The `Poseidon.Utils` module only provides the definition of an Exception type.
* The modules in `CLI/` define the functionality provided in the command line functions for `trident`.
* The `list` command might be a good place to start understanding what's going on and how to use the `Poseidon.Package` interface.
* The `Poseidon.CmdFStats` module is a bit more involved, mainly due to the Jackknifing, which involves chunking up the genotype data as we run through it and compute F-Statistics in each block, and then summarising them again. This is all achieved in one go via the `Pipes.Group` technology.

## Preparing a new stable release

The Github Actions script in `.github/workflows/release.yml` registeres a new draft release and automatically builds and uploads trident binaries when a new Git tag with the prefix `v*` is pushed. 

```bash
# locally register a new tag (e.g. 0.3.1)
git tag -a v0.3.1 -m "short description text for this release"
# push tag
git push origin v0.3.1
```

In case of a failing build delete the tag and the release draft on Github and then delete the tag locally with

```bash
git tag -d v0.3.1
```

before rerunning the procedure above.

