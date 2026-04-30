# V1.0.1.1: new release pipeline

This minor release replaces the pipeline to produce static `xerxes` executables for every release.

Just like `trident`, `xerxes` will now be available in the following pre-compiled versions:

- `xerxes-conda-linux` (for bioconda)
- `xerxes-Linux` (build on Ubuntu 20.04 for X64 architecture)
- `xerxes-macOS-ARM64` (build on macOS 14 for ARM64 architecture)
- `xerxes-macOS-X64` (build on macOS 13 for X64 architecture)
- `xerxes-Windows`

`xerxes` now also depends on the latest poseidon-hs version 1.5.0.1.

# V1.0.1.0: FST fix and better user input checks

This release fixes a bug in the estimation of FST. We have now exactly implemented the estimator presented in Bhatia et al. 2013
(see whitepaper), and confirmed numerical equivalence with estimation by admixtools. 

We have also now checked for degenerate statistics (for example F2(A, A) or F3(A, B, A)) and in those cases now output exactly zero.
Previously, due to bias corrections, results could become negative.

Finally, we have implemented a check for illegal sample sizes. In particular for F3(A, B, C), where C is a haploid outgroup, we now 
throw a helpful error that one should rather use F3vanilla, since F3 in that case is undefined due to its use of the heterozygosity of
C in the bias-correction term (see whitepaper).

# V1.0.0.2: performance-fix due to update of poseidon-hs dependency

This release simply updates the dependency on poseidon-hs, which in turn [fixed a performance leak](https://github.com/poseidon-framework/poseidon-hs/releases/tag/v1.4.0.3) with our entity selection framework.

# V1.0.0.1: specify package-versions in fstats, genotype ploidy and whitepaper

This release updates to the latest version of poseidon-hs (v1.4.0.2), which introduced a major clarification and improvement of the entity-selection language, that also powers the selection language to specify groups in fstats and define the statistics.

In particular, appending a version of a package after the package-name is now possible, both in group selection and statistic definitions. For example, here is a possible fstats configuration file:

```YAML
groupDefs:
  Group1: ["*package1-v1.0.0*", "group2", "<pac1-v2.3.4:group:name>"]
fstats:
- type: F2
  a: ["Group1", "Spanish"]
  b: ["Han", "CEU2"]
```

Second, fstats now uses the information in the column "Genotype_Ploidy" from the Janno file to improve bias-correction. Specifically, haploid samples contribute only one chromosome to the count in the bias-correction formula.

We also now added a whitepaper (available under `docs/`), which details the bias-correction and many other aspects of the mathematical basis for F-Statistics. The whitepaper may expand to other aspects of xerxes in the future.

# V0.3.4.0: Poseidon 2.7.1 and running admixpops in chunks

This release yet again updates the version of poseidon-hs (to v1.2.1.0). This enables `xerxes` to read Poseidon packages of version 2.7.1. We also switched to a newer stack resolver version and updated the GitHub Actions for automatic testing.

One new feature was added: The code for the `admixpops` subcommand was refactored to include a new input flag `--inChunks`. This allows to sample not just on the level of individual SNPs for the construction of artificial ancestry-chimeras, but also in chunks, so longer stretches of SNPs of length `--chunkSize`. Please note that `admixpops` remains experimental with no guarantees of correctness.

# V0.3.2.0: Poseidon 2.7.0 and fstats-block-output

- This release updates the underlying Poseidon library, which makes this version now compatible with newest Poseidon packages of version 2.7.0.
- We also added a new feature to `xerxes fstats` to output statistics per Jackknife-block.
- The output was also prettified using our central log-functionality.
- The GHC compiler which this package depends upon has also been updated to a much newer release than the version before. In particular, this new compiler runs on the newest Mac M2 Macbook-Pros (which the previous ones did not).

# V0.3.0.0: Added a first implementation of admixpops

For this release we integrated the very experimental artificial ancestry generation subcommand `admixpops` into xerxes (formerly a component of a tool called [paagen](https://github.com/nevrome/paagen)). We also added some innovations from trident for logging, error handling and code maintenance.

`admixpops` allows to generate individuals with randomized genotype profiles based on arbitrary input populations. Running it for example with the following `--admixString` would yield an artificial individual `Ind1` with 10% ancestry from the reference population Mbuti, 20% from Han, and 70% from French: `"[Ind1:Pop1](Mbuti=10+Han=20+French=70)"`. The implemented algorithm is (trivial) SNP-wise weighted random-sampling. `admixpops` is therefore only an experimental feature and the current implementation will likely change in the future.

In the process of adding it to xerxes, we also upgraded the latter with some of the (recent) improvements in trident. From a user-perspective the only relevant changes are the new general options `--logMode` and `--errLength`, which give you more control over the logging output and error messages.

# V0.2.1.0: Optimized fstats with allele-frequency lookups

This release internally optimizes the core loop by creating lookup tables for allele frequencies, so that they can be reused for the various statistics. 

Code has been refactored and unit tests introduced.

Logging is partly implemented with Co-Log now, but not fully yet.

# V0.2.0.0: New config file input, ascertainment and adhoc-groups Latest

This is a big release with three new features in Xerxes fstats:

1.) A new input option for F-Statistics using a YAML configuration file, supporting automatic looping through multiple statistics.
2.) The option of entering SNP ascertainment, using allele frequency bounds in a reference population
3.) The option of defining adhoc-groups, for example to exclude outlier individuals from populations without changing the genotype data files.

See documentation on https://poseidon-framework.github.io/#/xerxes
