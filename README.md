[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/poseidon-framework/poseidon-hs/CI)](https://github.com/poseidon-framework/poseidon-hs/actions?query=workflow%3ACI)
[![Coverage Status](https://img.shields.io/codecov/c/github/poseidon-framework/poseidon-hs/master.svg)](https://codecov.io/github/poseidon-framework/poseidon-hs?branch=master)
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/poseidon-framework/poseidon-hs?include_prereleases) ![GitHub all releases](https://img.shields.io/github/downloads/poseidon-framework/poseidon-hs/total)](https://github.com/poseidon-framework/poseidon-hs/releases)

# poseidon-hs
A toolset to work with modular genotype databases formatted using Poseidon. The main executable within this package is called `trident`.

* [Installation Quickstart](#installation-quickstart)
* [Guide for the command line utility](#guide-for-the-command-line-utility)
  + [Poseidon package repositories](#poseidon-package-repositories)
  + [Analysing your own dataset outside of the main repository](#analysing-your-own-dataset-outside-of-the-main-repository)
  + [Package creation and manipulation commands](#package-creation-and-manipulation-commands): [`init`](#init-command), [`fetch`](#fetch-command), [`forge`](#forge-command), [`update`](#update-command)
  + [Inspection commands](#inspection-commands): [`list`](#list-command), [`summarise`](#summarise-command), [`survey`](#survey-command), [`validate`](#validate-command)
  + [Analysis commands](#analysis-commands): [`fstats`](#fstats-command)
  + [Getting help](#getting-help)
- [Developers Guide](#developers-guide)
  * [Development Quickstart](#development-quickstart)
  * [Preparing a new stable release](#preparing-a-new-stable-release)
  * [The Poseidon HTTP Server](#the-poseidon-http-server)

## Installation Quickstart

Binaries for stable release versions can be downloaded [here](https://github.com/poseidon-framework/poseidon-hs/releases).

To install the latest development version you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.
4. If you're a developer and would like to run the tests, execute `stack test` inside the repository to build and run tests.

## Guide for trident

### Poseidon package repositories
Trident generally requires Poseidon datasets to work with. Most trident subcommands therefore have a central parameter, called `--baseDir` or simply `-d` to specify a base directory with Poseidon packages. For example, if all Poseidon packages live inside a repository at `/path/to/poseidon/packages` you would simply say `trident <subcommand> -d /path/to/poseidon/dirs/` and `trident` would automatically search all subdirectories inside of the repository for valid poseidon packages.

We typically recommend arranging a poseidon repository in a hierarchical way. For example:

```
/path/to/poseidon/packages
    /modern
        /2019_poseidon_package1
        /2019_poseidon_package2
    /ancient
        /...
        /...
    /Reference_Genomes
        /...
        /...
    /Archaic_Humans
        /...
        /...
```

You can use this structure to select only the level of packages you're interested in, and you can make use of the fact that `-d` can be given multiple times.

Let's use the `list` command to list all packages in the `modern` and `Reference_Genomes`:

```
trident list -d /path/to/poseidon/packages/modern \
  -d /path/to/poseidon/packages/ReferenceGenomes --packages
```

### Analysing your own dataset outside of the main repository

Being able to specify one or multiple repositories is often not enough, as you may have your own data to co-analyse with the main repository. This is easy to do, as you simply need to provide your own genotype data as yet another poseidon package to be added to you `trident list` command. For example, let's say you have genotype data in `EIGENSTRAT` format:

```
~/my_project/my_project.geno
~/my_project/my_project.snp
~/my_project/my_project.ind
```

then you can make that to a skeleton Poseidon package with the [`init`](#init-command) command. You can also do it manually by simply adding a `POSEIDON.yml` file, with the following content:

```
poseidonVersion: 2.0.1
title: My awesome project
description: Unpublished genetic data from my awesome project
contributor:
  - name: Stephan Schiffels
    email: schiffels@institute.org
lastModified: 2020-10-07
bibFile: sources.bib
genotypeData:
  format: EIGENSTRAT
  genoFile: my_project.geno
  snpFile: my_project.snp
  indFile: my_project.ind
jannoFile: my_project.janno
```

Two remarks: 1) all file paths are considered _relative_ to the directory in which `POSEIDON.yml` resides. Here I assume that you put this file into the same directory as the three genotype files. 2) There are two files referenced by this `POSEIDON.yml` file that aren't yet there: `sources.bib` and `my_project.janno`. That means that this is not a fully valid Poseidon package. However, `trident` will still accept this as long as the genotype files are there and in the right format.

Note that at the time of this writing, `trident` supports `EIGENSTRAT` and `PLINK` as formats.

Once you have set up your own "Poseidon" package (which is really only a skeleton), you can add it to your `trident` analysis, by simply adding your project directory to the command using `-d`:

```
trident list -d /path/to/poseidon/packages/modern \
  -d /path/to/poseidon/packages/ReferenceGenomes
  -d ~/my_project --packages
```

### Package creation and manipulation commands

#### Init command
`init` creates a new, valid poseidon package from genotype data files. It adds a valid `POSEIDON.yml` file, a dummy .janno file for context information and an empty .bib file for literature references.

The command

```
trident init \
  --inFormat genotype_data_format \
  --genoFile path/to/geno_file \
  --snpFile path/to/snp_file \
  --indFile path/to/ind_file \
  -n new_package_name \
  -o path/to/new_package_name
```

requires the format (`--inFormat`) of your input data (either `EIGENSTRAT` or `PLINK`) and the paths to the respective files in `--genoFile`, `--snpFile` and `--indFile`.

|          | EIGENSTRAT | PLINK |
|----------|------------|-------|
| genoFile | .geno      | .bed  |
| snpFile  | .snp       | .bim  |
| indFile  | .ind       | .fam  |

The output package of `init` is created as a new directory `-o`, which should not already exist, and gets the name defined in `-n`.

#### Fetch command
`fetch` allows to download poseidon packages from a remote poseidon server.

It works with 

```
trident fetch -d ... -d ... \
  -f "*package_title_1*, *package_title_2*, *package_title_3*" \
  --fetchFile path/to/forgeFile
```

and the packages you want to download must be listed either in a simple string with comma-separated values (`-f`/`--fetchString`) or in a text file (`--fetchFile`). More about that in the documentation of `forge` below. Each package title has to be wrapped in asterisks: *package_title*. The downloaded packages are added in the first (!) `-d` directory, but downloads are only performed if the respective packages are not already present in an up-to-date version in any of the `-d` dirs.

`fetch` also has the optional arguments `--remote https:://..."` do name an alternative poseidon server. The default points to the [DAG server](https://poseidon-framework.github.io/#/repos). 

To overwrite outdated package versions with `fetch`, the `-u`/`--upgrade` flag has to be set. Note that many file systems do not offer a way to recover overwritten files. So be careful with this switch.

#### Forge command
`forge` creates new poseidon packages by extracting and merging packages, populations and individuals from your poseidon repositories.

`forge` can be used with

```
trident forge -d ... -d ... \
  -f "*package_name*, group_id, <individual_id>" \
  --forgeFile path/to/forgeFile \
  -n new_package_name \
  -o path/to/new_package_name
```

where the entities (packages, groups/populations, individuals/samples) you want in the output package can be denoted either as as simple string with comma-separated values (`-f`/`--forgeString`) or in a text file (`--forgeFile`). Entities have to be marked in a certain way: 

- Each package is surrounded by `*`, so if you want all individuals of `2019_Jeong_InnerEurasia` in the output package you would add `*2019_Jeong_InnerEurasia*` to the list.
- Groups/populations are not specially marked. So to get all individuals of the group `Swiss_Roman_period`, you would simply add `Swiss_Roman_period`.
- Individuals/samples are surrounded by `<` and `>`, so `ALA026` becomes `<ALA026>`.

You can either use `-f` or `--forgeFile` or even combine them. In the file each line is treated as a separate forge string, so the following files will yield identical results:

```
*package_name*, group_id, <individual_id>
```

```
*package_name*, group_id
<individual_id>
```

```
*package_name*
group_id
<individual_id>
```

Just as for `init` the output package of `forge` is created as a new directory `-o` and gets the name defined in `-n`.

#### Update command
`update` adds or updates the [md5 checksums](https://en.wikipedia.org/wiki/Md5sum) in the POSEIDON.yml field `checksums` for one or multiple packages.

It can be called simply with

```
trident update -d ... -d ...
```

:heavy_exclamation_mark: As `update` reads and rewrites POSEIDON.yml files, it may change their inner order, layout or even content (e.g. if they have fields which are not in the [Poseidon package definition](https://github.com/poseidon-framework/poseidon2-schema)). Create a backup of the POSEIDON.yml file before running `update` if you are uncertain.

### Inspection commands

#### List command
`list` lists packages, groups and individuals of the datasets you use.

To list packages, as seen above you run

```
trident list -d ... -d ... --packages
```
which will output some log messages, for example about packages that were skipped because of formatting issues of the `POSEIDON.yml` file. T

Example for the final output:
```
.-----------------------------------------.------------.----------------.
|                  Title                  |    Date    | Nr Individuals |
:=========================================:============:================:
| 2015_1000Genomes_1240K_haploid_pulldown | 2020-08-10 | 2535           |
| 2016_Mallick_SGDP1240K_diploid_pulldown | 2020-08-10 | 280            |
| 2018_BostonDatashare_modern_published   | 2020-08-10 | 2772           |
| ...                                     | ...        |                |
'-----------------------------------------'------------'----------------'
```
so a nicely formatted table of all packages, their last update and the number of individuals in it.

You can also list groups, as defined in the third column of Eigenstrat Ind files (or the first column of a PLINK fam file):

```
trident list -d ... -d ... --groups
```
for which an example output may contain:

```
.--------.-----------------------------------------.----------------.
| Group  |                      Packages           | Nr Individuals |
:========:=========================================:================:
| AA     | 2018_BostonDatashare_modern_published   | 12             |
| ACB.SG | 2015_1000Genomes_1240K_haploid_pulldown | 96             |
| Abazin | 2019_Jeong_InnerEurasia                 | 8              |
| ...    | ...                                     |                |
'--------'-----------------------------------------'----------------'
```

which lists all groups, the packages those groups are in, the total number of individuals in each group.

Note that if you want a less fancy table, for example because you want to load this into Excel, or pipe into another command that cannot deal with the fancy layout, you can use the `--raw` option to output that table as a simple tab-delimited file.

Finally, you can query for individuals, using the `--individual` option:
```
trident list -d ... -d ... --individuals
```

Example output:

```
.-----------------------------------------.------------.------------.
|                 Package                 | Individual | Population |
:=========================================:============:============:
| 2015_1000Genomes_1240K_haploid_pulldown | HG00096.SG | GBR.SG     |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00097.SG | GBR.SG     |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00099.SG | GBR.SG     |
| ...                                     | ...        | ...        |
'-----------------------------------------'-------------------------'
```

which lists all individuals with their package, group and individual name.

#### Summarise command
`summarise` prints some general summary statistics for a given poseidon dataset taken from the .janno files.

You can run it with

```
trident summarise -d ... -d ...
```

which will show you context information like -- among others -- the number of individuals in the dataset, their sex distribution, the mean age of the samples (for ancient data) or the mean coverage on the 1240K SNP array in a table. `summarise` depends on complete .janno files and will silently ignore missing information for some statistics.

You can use the `--raw` option to output the summary table as a simple tab-delimited file.

#### Survey command
`survey` tries to indicate package completeness for poseidon datasets. 

Running

```
trident survey -d ... -d ...
```

will yield a table with one row for each package. Completeness is encoded with the following terms:

- column 1: **Genotype data**
  - `G`: Genotype data is present
  - `.`: Genotype data is missing
- column 2-35: **.janno file columns**
  - `M`: Mandatory column that has to be complete
  - `X`: There are some valid entries in this column
  - `.`: The column only contains `n/a` values and was therefore most likely not filled out
- column 1: **BibTeX file**
  - `B`: BibTeX file is present
  - `.`: BibTeX file is missing

Also here you can use the `--raw` option to output the survey table as a simple tab-delimited file.

#### Validate command
`validate` checks poseidon datasets for structural correctness. 

You can run it with

```
trident validate -d ... -d ...
```

and it will either report a success (`Validation passed ✓`) or failure with specific error messages to simplify fixing the issues. 

`validate` tries to ensure that each package in the dataset adheres to the [schema definition](https://github.com/poseidon-framework/poseidon2-schema) of a poseidon package. Here is a list of what is checked:

- Presence of the necessary files
- Full structural correctness of .bib and .janno file
- Superficial correctness of genotype data files. A full check would be too computationally expensive
- Correspondence of BibTeX keys in .bib and .janno
- Correspondence of individual and group IDs in .janno and genotype data files

### Analysis commands

#### Fstats command

Trident allows you to analyse genotype data across poseidon packages, including your own, as explained above by "hooking" in your own package via a `--baseDir` (or `-d`) parameter. This has the advantage that you can compute arbitrary F-Statistics across groups and individuals distributed in many packages, without the need to explicitly merge the data. Trident also takes care of merging PLINK and EIGENSTRAT data on the fly. It also takes care of different genotype base sets, like Human-Origins vs. 1240K. It also flips alleles automatically across genotype files, and throws an error if the alleles in different packages are incongruent with each other. Trident is also smart enough to select only the packages relevant for the statistics that you need, and then streams through only those genotype data.

Here is an example command for computing several F-Statistics:

```
trident fstats -d ... -d ... \
  --stat "F4(<Chimp.REF>, <Altai_published.DG>, Yoruba, French)" \
  --stat "F4(<Chimp.REF>, <Altai_snpAD.DG>, Spanish, French)" \
  --stat "F4(Mbuti,Nganasan,Saami.DG,Finnish)" \
  --stat "F3(French,Spanish,Mbuti)" \
  --stat "F2(French,Spanish)" \
  --stat "PWM(French,Spanish)"
```

This showcases a couple of points:
* You can compute F2, F3 and F4 statistics, as well as Pairwise-Mismatch-Rates between groups. Note that in F3 statistics, the third population has the outgroup-role (or the target-admixture role depending on how you use it).
* Use the `--stat` option to enter a single statistic. Use it multiple times to compute several statistics in one go
* Use opening and closing brackets to list the groups, separated by comma followed by zero or more spaces.
* Enclose a statistic with double-quotes, to not have bash interpret the brackets wrongly.
* A normal name is interpreted as the name of a group, while a name enclosed by angular brackets, like "<Chimp.REF>" refers to an _individual_. This can be useful if you want to analyse some individuals in a group separately.

You can also load these statistics from a file. Say you have a file named `fstats.txt` with the following content:

```
F4(<Chimp.REF>, <Altai_published.DG>, Yoruba, French)
F4(<Chimp.REF>, <Altai_snpAD.DG>, Spanish, French)
F4(Mbuti,Nganasan,Saami.DG,Finnish)
```

you can then load these statistics using the option `--statFile fstats.txt`. You can also combine statistics read from
a file and statistics read from the command line.

While running the command, you will see a lot of log messages of the form:

```
computing chunk range (1,752566) - (1,12635412), size 5000, values [5.911444428637878e-3,-1.8095540770823502e-3,-1.125257367242664e-2,0.14513440659936425,3.019591456774886e-3,-1.2895210945181934]
computing chunk range (1,12637058) - (1,23477511), size 5000, values [9.680787233954864e-3,8.875422512874053e-4,-1.5542492018047156e-2,0.1510010864324222,3.423485242616963e-3,-1.3555910200669081]
computing chunk range (1,23485934) - (1,36980804), size 5000, values [2.3725885721274857e-3,-2.9289533859294493e-5,-9.839436474279163e-3,0.17268760649484693,2.883453062983087e-3,-1.4139911740647404]
computing chunk range (1,36983827) - (1,49518537), size 5000, values [1.0732414227978656e-2,1.82935508093639e-3,-1.265178671079672e-2,0.1465399856299282,4.448175472444382e-3,-1.408587647156686]
computing chunk range (1,49519125) - (1,61041875), size 5000, values [1.7715712201896328e-3,-5.296485015140395e-4,-1.0758548403470404e-2,0.13780069899614356,3.101218183674832e-3,-1.380892007845735]
```

This shows you the progress of the command. Each logging row here denotes a block of genotype data, for which each statistic is computed, as listed in the end of each line.

The final output of the `fstats` command looks like this:

```
.----------------------------------------------------.-----------------------.-----------------------.---------------------.
|                     Statistic                      |       Estimate        |        StdErr         |       Z score       |
:====================================================:=======================:=======================:=====================:
| F4(<Chimp.REF>,<Altai_published.DG>,Yoruba,French) | 3.158944901394701e-3  | 3.9396628452534067e-4 | 8.018312798519467   |
| F4(<Chimp.REF>,<Altai_snpAD.DG>,Spanish,French)    | 6.224416129499041e-5  | 6.593273670495018e-5  | 0.9440554784421251  |
| F4(Mbuti,Nganasan,Saami.DG,Finnish)                | -8.203181515666918e-3 | 5.722102735664199e-4  | -14.335956368869223 |
| F3(French,Spanish,Mbuti)                           | 0.13473315812634057   | 1.366496126392123e-3  | 98.5975412034781    |
| F2(French,Spanish)                                 | 3.16793648777051e-3   | 3.4084098466298525e-5 | 92.94470531185924   |
| PWM(French,Spanish)                                | -1.19837777631975     | 8.820206514282228e-3  | -135.86731494089872 |
'----------------------------------------------------'-----------------------'-----------------------'---------------------'
```
which lists each statistic, the genome-wide estimate, its standard error and its Z-score.

### Getting help

You can use `trident --help`, `trident list --help` and `trident fstats --help` to get information about each parameter, including some that I haven't covered in this guide.

## Developers Guide

### Development Quickstart

You can install the internal documentation using `stack haddock` and open it subsequently using `stack haddock --open`. This will then open a HTML page with all dependency packages and the `poseidon-hs` library itself. The critical package is the `Poseidon.Package` module which defines the core functions to read and work with module files.

Important packages to look into to understand the architecture of this tool:

* Start with `Poseidon.Package`: It defines the main package format and provides some functions how to access the data inside packages.
* The `Poseidon.Utils` module only provides the definition of an Exception type.
* The modules in `CLI/` define the functionality provided in the command line functions for `trident`.
* The `list` command might be a good place to start understanding what's going on and how to use the `Poseidon.Package` interface.
* The `Poseidon.CmdFStats` module is a bit more involved, mainly due to the Jackknifing, which involves chunking up the genotype data as we run through it and compute F-Statistics in each block, and then summarising them again. This is all achieved in one go via the `Pipes.Group` technology.

### Preparing a new stable release

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

### The Poseidon HTTP Server

The second executable in this package is `poseidon-http-server`, a program that serves poseidon packages via HTTP or HTTPS. 

Basic usage is:

```
poseidon-http-server [--version] (-d|--baseDir DIR) [-p|--port PORT] 
                            [-i|--ignoreGenoFiles] 
                            [--certFile CERTFILE [--chainFile CHAINFILE]
                              --keyFile KEYFILE]
```
Available options:
```
  -h,--help                Show this help text
  --version                Show version
  -d,--baseDir DIR         a base directory to search for Poseidon Packages
                           (could be a Poseidon repository)
  -p,--port PORT           the port on which the server listens (default: 3000)
  -i,--ignoreGenoFiles     whether to ignore the bed and SNP files. Useful for
                           debugging
  --certFile CERTFILE      The cert file of the TLS Certificate used for HTTPS
  --chainFile CHAINFILE    The chain file of the TLS Certificate used for HTTPS.
                           Can be given multiple times
  --keyFile KEYFILE        The key file of the TLS Certificate used for HTTPS
```

Note that if none of `certFile`, `chainFile` and `keyFile` are given, the server listens to HTTP (unsecure) traffic. If at least a `certFile` and a `keyFile` are given, it listens via HTTPS traffic. 

