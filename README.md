[![CI](https://github.com/poseidon-framework/poseidon-hs/actions/workflows/main.yml/badge.svg?branch=master)](https://github.com/poseidon-framework/poseidon-hs/actions/workflows/main.yml)
[![Coverage Status](https://img.shields.io/codecov/c/github/poseidon-framework/poseidon-hs/master.svg)](https://codecov.io/github/poseidon-framework/poseidon-hs?branch=master)
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/poseidon-framework/poseidon-hs?include_prereleases) ![GitHub all releases](https://img.shields.io/github/downloads/poseidon-framework/poseidon-hs/total)](https://github.com/poseidon-framework/poseidon-hs/releases)
[![Install with Bioconda](https://anaconda.org/bioconda/poseidon-trident/badges/version.svg)](https://anaconda.org/bioconda/poseidon-trident) [![Anaconda-Server Badge](https://anaconda.org/bioconda/poseidon-trident/badges/downloads.svg)](https://anaconda.org/bioconda/poseidon-trident)

# poseidon-hs
A toolset to work with modular genotype databases formatted using Poseidon. The main executables within this package are called `trident` and `xerxes`.

**Detailed user documentation can be found on our website for [trident](https://poseidon-framework.github.io/#/trident) and [xerxes](https://poseidon-framework.github.io/#/xerxes).**

***

## For (Haskell) developers

To install the development version of poseidon-hs/trident you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone this repository
3. Execute `stack install` inside the repository to build the tool and copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.
4. To run the tests, execute `stack test` inside the repository to build and run tests.

The technical haddock documentation for the poseidon-hs Haskell library is available [here](https://poseidon-framework.github.io/poseidon-hs/index.html) (you can also render it locally using `stack haddock` and open it subsequently using `stack haddock --open`).

### Preparing a new stable release

The Github Actions script in `.github/workflows/release.yml` registers a new draft release and automatically builds and uploads trident binaries when a new Git tag with the prefix `v*` is pushed. 

```bash
# locally register a new tag (e.g. 0.3.1)
git tag -a v0.3.1 -m "see CHANGELOG.md"
# push tag
git push origin v0.3.1
```

In case of a failing build delete the tag and the release draft on Github and then delete the tag locally with

```bash
git tag -d v0.3.1
```

before rerunning the procedure above.

### Golden tests

To test some of the output of the powerful trident CLI functions, poseidon-hs features a golden test mechanism:

The output of a number of CLI function calls implemented in [`test/PoseidonGoldenTests/GoldenTestsRunCommands.hs`](test/PoseidonGoldenTests/GoldenTestsRunCommands.hs) is stored in a static file storage ([`test/PoseidonGoldenTests/GoldenTestData`](test/PoseidonGoldenTests/GoldenTestData)), together with relevant checksums for these files in [`test/PoseidonGoldenTests/GoldenTestCheckSumFile.txt`](test/PoseidonGoldenTests/GoldenTestCheckSumFile.txt). 

`stack test` then runs -- among other things -- the code in `/test/testDat/poseidonHSGoldenTestData` again and stores the result in a dynamic, temporary directory. It then validates the output by comparing the new and temporary checksums with the old and git-logged checksums in said file `poseidonHSGoldenTestCheckSumFile.txt`. Any deviation between these two sets of checksums will cause the test to fail. The test output in this case will therefore only print the name of the operation or the output file that changed, not what exactly changed within this file. 

To find out what exactly changed and also update the output storage and static checksum file, you can run `poseidon-devtools updateGoldenTests` and compare the old and new static output storage with `git diff`.
