# poseidon-tools
A toolset to work with modular genotype databases

## Developer Quickstart

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack test` inside the repository to build and run tests. This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.
4. Execute `stack install` inside the repository to build the tool and copy the executables to `~/.local/bin` (which you may want to add to your path)
5. Try out the CLI, for example from inside the directory: `poet view -d poet view -d test/testDat/testModules/`
