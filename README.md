# poseidon-tools
A toolset to work with modular genotype databases formatted using Poseidon.

## Installation Quickstart

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. If you're a developer and would like to run teh tests, execute `stack test` inside the repository to build and run tests. This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.
4. Execute `stack install` inside the repository to build the tool and copy the executables to `~/.local/bin` (which you may want to add to your path)

## Development Quickstart

You can install the internal documentation using `stack haddock` and open it subsequently using `stack haddock --open`. This will then open a HTML page with all dependency packages and the `poseidon-hs` library itself. The critical package is the `Poseidon.Package` module which defines the core functions to read and work with module files.

Important packages to look into to understand the architecture of this tool:
* Start with `Poseidon.Package`: It defines the main package format and provides some functions how to access the data inside packages.
* The `Poseidon.Utils` module only provides the definition of an Exception type.
* The two modules `Poseidon.CmdFStats` and `Poseidon.CmdList` define the functionality provided in the two command line functions `list` and `fstats`.
* The `list` command might be a good place to start and understanding what's going on and how to use the `Poseidon.Package` interface.
* The `Poseidon.CmdFStats` module is a bit more involved, mainly due to the Jackknifing, which involves chunking up the genotype data as we run through it and compute F-Statistics in each block, and then summarising them again. This is all achieved in one go via the `Pipes.Group` technology.

## Guide

### Poseidon package repositories
Poet generally requires Poseidon datasets to work with. All poet subcommands therefore have a central parameter, called `--baseDir` or simply `-d` to specify a base directory with Poseidon packages. For example, if all Poseidon packages live inside a repository at `/path/to/poseidon/packages` you would simply say `poet <subcommand> -d /path/to/poseidon/dirs/` and `poet` would automatically search all subdirectories inside of the repository for valid poseidon packages.

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
poet list -d /path/to/poseidon/packages/modern \
  -d /path/to/poseidon/packages/ReferenceGenomes --packages
```

### Analysing your own dataset outside of the main repository

Being able to specify one or multiple repositories is often not enough, as you may have your own data to co-analyse with the main repository. This is easy to do, as you simply need to provide your own genotype data as yet another poseidon package to be added to you `poet list` command. For example, let's say you have genotype data in `EIGENSTRAT` format:

```
~/my_project/my_project.geno
~/my_project/my_project.snp
~/my_project/my_project.ind
```

then you can make that to a skeleton Poseidon package by simply adding a `POSEIDON.yml` file, with the following content:

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

Two remarks: 1) all file paths are considered _relative_ to the directory in which `POSEIDON.yml` resides. Here I assume that you put this file into the same directory as the three genotype files. 2) There are two files referenced by this `POSEIDON.yml` file that aren't yet there: `sources.bib` and `my_project.janno`. That means that this is not a fully valid Poseidon package. However, `poet` will still accept this (it won't even look for these files) as long as the genotype files are there and in the right format.

Note that at the time of this writing, `poet` supports `EIGENSTRAT` and `PLINK` as formats.

Once you have set up your own "Poseidon" package (which is really only a skeleton), you can add it to your `poet` analysis, by simply adding your project directory to the command using `-d`:

```
poet list -d /path/to/poseidon/packages/modern \
  -d /path/to/poseidon/packages/ReferenceGenomes
  -d ~/my_project --packages
```


### List Command
The most basic command is the `list` command. It simply lists packages, groups and individuals of the datasets you use.

To list packages, as seen above you run

```
poet list -d ... -d ... --packages
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
| 2019_Biagini_Spain                      | 2020-08-10 | 120            |
| 2019_Jeong_InnerEurasia                 | 2020-08-10 | 763            |
| 2019_Lamnidis_Fennoscandia              | 2020-08-10 | 1              |
| Archaic_Humans                          | 2020-08-10 | 14             |
| Reference_Genomes                       | 2020-08-10 | 11             |
'-----------------------------------------'------------'----------------'
```
so a nicely formatted table of all packages, their last update and the number of individuals in it.

You can also list groups, as defined in the third column of Eigenstrat Ind files (or the first column of a PLINK fam file):

```
poet list -d ... -d ... --groups
```
for which an example output may contain:

```
.-----------------------------------------------.----------------------------------------------------.----------------.
|                     Group                     |                      Packages                      | Nr Individuals |
:===============================================:====================================================:================:
| AA                                            | 2018_BostonDatashare_modern_published              | 12             |
| ACB.SG                                        | 2015_1000Genomes_1240K_haploid_pulldown            | 96             |
| ASW.SG                                        | 2015_1000Genomes_1240K_haploid_pulldown            | 66             |
| Abazin                                        | 2019_Jeong_InnerEurasia                            | 8              |
| Abazin_outlier                                | 2019_Jeong_InnerEurasia                            | 2              |
| Abkhasian                                     | 2018_BostonDatashare_modern_published              | 9              |
| Adygei                                        | 2018_BostonDatashare_modern_published,2019_Jeong_… | 31             |
| Adygei.DG                                     | 2016_Mallick_SGDP1240K_diploid_pulldown            | 2              |
| Afar.WGA                                      | 2018_BostonDatashare_modern_published              | 5              |
| Ain_Touta.WGA                                 | 2018_BostonDatashare_modern_published              | 3              |
| Albanian                                      | 2018_BostonDatashare_modern_published              | 6              |
| Albanian.DG                                   | 2016_Mallick_SGDP1240K_diploid_pulldown            | 1              |
| Aleut                                         | 2018_BostonDatashare_modern_published              | 7              |
| Aleut.DG                                      | 2016_Mallick_SGDP1240K_diploid_pulldown            | 2              |
| Algerian                                      | 2018_BostonDatashare_modern_published              | 7              |
| Altai_Neanderthal.DG                          | Archaic_Humans                                     | 1              |
| Altai_Neanderthal_published.DG                | Archaic_Humans                                     | 1              |
| Altaian                                       | 2018_BostonDatashare_modern_published,2019_Jeong_… | 24             |
| ...                                           | ...                                                |                |
'-----------------------------------------------'----------------------------------------------------'----------------'
```

which lists all groups, the packages those groups are in, the total number of individuals in each group.

Note that if you want a less fancy table, for example because you want to load this into Excel, or pipe into another command that cannot deal with the fancy layout, you can use the `--raw` option to output that table as a simple tab-delimited file.

Finally, you can query for individuals, using the `--individual` option:
```
poet list -d ... -d ... --individuals
```

Example output:

```
.-----------------------------------------.------------------.----------------------.
|                 Package                 |    Individual    |      Population      |
:=========================================:==================:======================:
| 2015_1000Genomes_1240K_haploid_pulldown | HG00096.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00097.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00099.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00100.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00101.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00102.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00103.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00105.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00106.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00107.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00108.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00109.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00110.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00111.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00112.SG       | GBR.SG               |
| 2015_1000Genomes_1240K_haploid_pulldown | HG00113.SG       | GBR.SG               |
'-----------------------------------------'-----------------------------------------'
```

which lists all individuals with their package, group and individual name.

### Running F-Statistics

Poet allows you to analyse genotype data across poseidon packages, including your own, as explained above by "hooking" in your own package via a `--baseDir` (or `-d`) parameter. This has the advantage that you can compute arbitrary F-Statistics across groups and individuals distributed in many packages, without the need to explicitly merge the data. Poet also takes care of merging PLINK and EIGENSTRAT data on the fly. It also takes care of different genotype base sets, like Human-Origins vs. 1240K. It also flips alleles automatically across genotype files, and throws an error if the alleles in different packages are incongruent with each other. Poet is also smart enough to select only the packages relevant for the statistics that you need, and then streams through only those genotype data.

Here is an example command for computing several F-Statistics:

```
poet fstats -d ... -d ... \
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

### Get more help

You can use `poet --help`, `poet list --help` and `poet fstats --help` to get information about each parameter, including some that I haven't covered in this guide.

