# poseidon-tools
A toolset to work with modular genotype databases

## Developer Quickstart

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack test` inside the repository to build and run tests. This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.
4. Execute `stack install` inside the repository to build the tool and copy the executables to `~/.local/bin` (which you may want to add to your path)

## Run

Here are some example commands (tested on MPI-SHH infrastructure):

    poet list -d /projects1/poseidon/ancient --packages

    poet list -d /projects1/poseidon/ancient/2020_Brunel_France --groups

    poet list -d /projects1/poseidon/ancient/2020_Brunel_France --individuals

    poet list -d /projects1/poseidon/ancient --raw --groups | grep LBK

    poet fstats -d /projects1/poseidon/modern \
      --stat "F4(<Chimp.REF>,<Altai_published.DG>,Yoruba,French)" \
      --stat "F4(<Chimp.REF>,<Altai_snpAD.DG>,Spanish,French)" \
      --stat "F4(Mbuti,Nganasan,Saami.DG,Finnish)" \
      --stat "F3(French,Spanish,Mbuti)"

