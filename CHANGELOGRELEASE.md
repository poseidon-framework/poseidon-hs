### V 1.2.0.0

...

### V 1.1.11.4

This release fixes a core issue in the implementation of Poseidon v2.7.0, where multiple columns of the .ssf file where not defined correctly as list columns. Poseidon v2.7.0 is in itself deprecated, though, and will be replaced as soon as possible with an updated version. This trident release exists thus mainly to provide a working implementation of 2.7.0 for future reference.

Beyond this change in functionality, this release also includes heavy refactoring in the `survey` subcommand, the golden test infrastructure and the overall version of Haskell poseidon-hs and trident are built with. These changes should not have any user-facing consequences.

### V 1.1.11.0

This release implements the changes necessary to make `trident` capable of handling packages specified for the new Poseidon standard v2.7.0:

- A Poseidon package can now include a .ssf file ("sequencing source file") as specified. `trident` considers it in `validate`, `update`, `survey` and, most importantly, `forge`, where .ssf files are compiled for new packages just as .janno files.
- `trident` now understands and validates the new .janno columns `Country_ISO` and `Library_Names`.
- `trident` now knows the possible value `mixed` for the .janno column `Library_Built`.

The behaviour of `trident` for older package schema versions (v2.5.0 and v2.6.0) should be mostly unchanged. `forge` and `init` now return Poseidon v2.7.0 packages, though.

### V 1.1.10.2

This release bundles a number or minor changes, new minor command line options and some internal refactoring without immediate consequences for `trident`.

#### Changes in command line options

- By default `validate` only tests genotype data by parsing the first 100 SNPs. This limitation is necessary for performance reasons, but can hide issues outside of this tiny subset. We now added an option `--fullGeno` to `validate`, which forces parsing of the entire .bed/.geno file.
- The .fam file of Plink-formatted genotype data is used inconsistently across different popular aDNA software tools to store group/population name information. See more on this issue in our discussion [here](https://github.com/poseidon-framework/poseidon-hs/issues/128). We now added the (global) option `--inPlinkPopName` and `--outPlinkPopName` with the arguments `asFamily` (default), `asPhenotype` and `asBoth` to control the reading and writing of the population name from and to Plink .fam files.
- The `--no-extract` option for faster, package-wise data selection in `forge` was not working properly. We fixed it, renamed it to `--packagewise` and improved its command line help text.

#### Bugfixes

- As described [here](https://github.com/poseidon-framework/poseidon-hs/issues/213), our implementation of .janno file parsing struggled with some encodings of the `No-Break Space` unicode character. We now decided to delete these characters upon reading, following the assumption that they are generally not desired in a .janno file anyway. In this process we also decided **to trim all whitespaces around .janno file fields**.

#### Other changes

- The `-j` option of `list`, which allows to include additional .janno columns in the output with the `--individuals` flag now allows to access arbitrary, additional variables.
- `update` writes messages to the CHANGELOG file now with a prefix `-`, to make it proper markdown.
- The verbose debug-level (with `--logMode VerboseLog`) warnings about missing standard columns in the .janno file were turned off.
- The important "schema version mismatch" error message was made more verbose and clear.
- `trident` failes gracefully now if one or all `-d`/`--baseDir`s do not exist.
- The important "broken lines" error message in the .janno reading process now reminds users to turn on `--logMode VerboseLog` to get more information.

### V 1.1.7.0

This release clarifies a long standing uncertainty how trident treats individual ID duplicates. It adds a new feature to the forge language to specify individuals more precisely and thus resolve duplication conflicts.

trident does **not** allow individuals with identical identifiers, so `Poseidon_ID`s, **within one package**. And we generally also discourage such duplicates across packages in package collections. But there is no reason to enforce this unnecessarily for subcommands where it does not matter. Here are the rules we defined now:

- Generally, so in the subcommands `ìnit`, `fetch`, `genoconvert`, `update`, `list`, `summarise`, and `survey`, trident logs a warning if it observes duplicates in a package collection found in the base dirs. But it proceeds normally then.
- Deviating from this, the special subcommand `validate` stops with an error if it observes duplicates. This behaviour can be changed with the new flag `--ignoreDuplicates`.
- The `forge` subcommand, finally, also ignores duplicates in the base dirs, except (!) this conflict exists within the entities in the `--forgeString`. In this case it stops with an informative error:

```
[Error]   There are duplicated individuals, but forge does not allow that
[Error]   Please specify in your --forgeString or --forgeFile:
[Error]   <Inuk.SG> -> <2010_RasmussenNature:Greenland_Saqqaq.SG:Inuk.SG>
[Error]   <Inuk.SG> -> <2011_RasmussenNature:Greenland_Saqqaq.SG:Inuk.SG>
[Error]   Error in the forge selection: Unresolved duplicated individuals
```

This already shows that the `-f`/`--forgeString` selection language of `forge` (and incidentally also `fetch`) includes a new syntactic element since this release: Individuals can now be described not just with `<individual>`, but also more specifically `<package:group:individual>`. Such defined individuals take precedence over differently defined ones (so: directly with `<individual>` or as a subset of `*package*` or `group`). This allows to resolve duplication issues precisely -- at least in cases where the duplicated individuals differ in source package or primary group.

### V 1.1.6.0

#### Additional columns in .janno files (V 1.1.5.0)

This release changes the way additional columns in .janno files are treated.

So far `trident` fully ignored additional variables, which had the consequence that `trident forge` dropped them without warning. With this new release, additional variables are loaded and carried along in `forge`. For merging different .janno files **A** and **B** the following rules apply regarding additional columns:

- If **A** has an additional column which is not in **B** then empty cells in the rows imported from **B** are filled with `n/a`.
- If **A** and **B** share additional columns with identical column name, then they are treated as semantically identical units and merged accordingly.
- In the resulting .janno file, all additional columns from both **A** and **B** are sorted alphabetically and appended after the normal, specified variables.

The following example illustrates the described behaviour:

**A.janno**

| Poseidon_ID | Group_Name | Genetic_Sex | AdditionalColumn1 | AdditionalColumn2 |
|-------------|------------|-------------|-------------------|-------------------|
| XXX011      | POP1       | M           | A                 | D                 |
| XXX012      | POP2       | F           | B                 | E                 |
| XXX013      | POP1       | M           | C                 | F                 |

**B.janno**

| Poseidon_ID | Group_Name | Genetic_Sex | AdditionalColumn3 | AdditionalColumn2 |
|-------------|------------|-------------|-------------------|-------------------|
| YYY022      | POP5       | F           | G                 | J                 |
| YYY023      | POP5       | F           | H                 | K                 |
| YYY024      | POP5       | M           | I                 | L                 |

**A.janno + B.janno**

| Poseidon_ID | Group_Name | Genetic_Sex | AdditionalColumn1 | AdditionalColumn2 | AdditionalColumn3 |
|-------------|------------|-------------|-------------------|-------------------|-------------------|
| XXX011      | POP1       | M           | A                 | D                 | n/a               |
| XXX012      | POP2       | F           | B                 | E                 | n/a               |
| XXX013      | POP1       | M           | C                 | F                 | n/a               |
| YYY022      | POP5       | F           | n/a               | J                 | G                 |
| YYY023      | POP5       | F           | n/a               | K                 | H                 |
| YYY024      | POP5       | M           | n/a               | L                 | I                 |

#### Minor changes (V 1.1.6.0)

- `--verbose` in `trident validate` was deprecated. The respective output is now logged on the DEBUG level, so can be accessed with `--logMode VerboseLog`
- Trailing slashes in `--outPath` for `init`, `genoconvert` and `forge` are now automatically removed. This prevents a common, confusing error, where a trailing slash would cause `trident` to assume the name of the resulting package is empty.

### V 1.1.4.2

With this release trident becomes able to handle the changes introduced for Poseidon v2.6.0.

- The *contributor* field in the POSEIDON.yml file is optional now and can be left blank.
- The *contributor* field now also can hold an ORCID in a subfield *orcid*. `trident` checks the [structural correctness](https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier) of this identifier.
- `trident` now recognizes the new available entries for the `Capture_Type` variable in the .janno file.

Beyond that:

- Already V 1.1.3.1 closed a loophole in .bib file validation, where .janno files could have arbitrary references if the .bib file was not correctly referenced in the POSEIDON.yml file.
- V 1.1.4.1 added a small validation check for the janno columns *Date_BC_AD_Start*, *Date_BC_AD_Median* and *Date_BC_AD_Stop*: Ages bigger than 2022 now trigger an error, because they are factually impossible and indicate that somebody accidentally entered a BP age.
- V 1.1.4.2 added parsing for Accession IDs. Wrong IDs are ignored (for now), so this is a non-breaking change.

### V 1.1.3.0

This release introduces a major change to the progress indicators in package downloading, reading, forging and converting. It also includes some minor code changes in the poseidon-hs library and the poseidon server executable.

#### Trident 

From a trident user perspective only the change in the progress indicators is relevant. So far we used updating (self-overwriting) counters, which were great for interactive use of trident in modern terminal emulators. They are not suitable for use in scripts, though, because the command line output does not yield well structured log files. We therefore decided to integrate the progress indicators with our general logging infrastructure.

- Loading packages (so the `Initializing packages...` phase) now stays silent by default. With `--logMode VerboseLog` you can list the packages that are currently loading:

```
[Debug]   [10:56:05] Package 20: ./2015_LlorenteScience/POSEIDON.yml
[Debug]   [10:56:05] Package 21: ./2017_KennettNatureCommunications/POSEIDON.yml
[Debug]   [10:56:06] Package 22: ./2016_MartinianoNatureCommunications/POSEIDON.yml
[Debug]   [10:56:06] Package 23: ./2016_BroushakiScience/POSEIDON.yml
[Debug]   [10:56:06] Package 24: ./2017_LindoPNAS/POSEIDON.yml
[Debug]   [10:56:06] Package 25: ./2021_Zegarac_SoutheasternEurope/POSEIDON.yml
```

- `forge` and `genoconvert` now print a log message every 10k SNPs:

```
[Info]    SNPs:    220000    5s
[Info]    SNPs:    230000    5s
[Info]    SNPs:    240000    5s
[Info]    SNPs:    250000    5s
[Info]    SNPs:    260000    6s
[Info]    SNPs:    270000    6s
```

- `fetch` now prints a log message whenever a +5% threshold is reached.

```
[Info]    Package size: 15.3MB
[Info]    MB:      0.8      5.2%
[Info]    MB:      1.6     10.5%
[Info]    MB:      2.4     15.7%
[Info]    MB:      3.2     20.9%
[Info]    MB:      4.0     26.1%
```

#### Server

The server has been updated in the following ways:

* It now uses Co-Log for logging
* A new option `-c` now makes it ignore checksums, which is useful for a fast start of the server if need be
* Zip files are now stored in a separate folder, to keep the (git-backed) repository itself clean
* There is a new API named `/compatibility/<version>` which accepts a client version (from trident) and returns a JSON tuple of Haskell-type (Bool, Maybe String). The first element is simply a Boolean saying if the client version is compatible with the server or not, the second is an optional Warning message the server can return to the client. This will become important in the future.

### V 1.1.1.1

This is a minor release to improve (internal) error handling.

Errors emerging from parsing genotype data are now properly caught and handled, which should improve the output of `trident` in these cases.

We also added a general option `--errLength` to give you more control over very long error messages. These can emerge with broken/unsuitable genotype data and can even exceed a terminal's scrollback buffer size. `--errLength` sets the default error output to 1500 characters, and allows to increase or decrease that number. `--errLength Inf` removes any constraints on error output length.

### V 1.1.0.0

This release summarises a number of smaller bugfixes and interface changes, but also introduces one minor breaking interface change, which makes it necessary to iterate the second major version number component.

- *V 1.0.0.1* fixed a memory leak in `trident genoconvert`.
- *V 1.0.0.2* brought the switch to a new compiler and dependency network version (GHC 8.10.7 and Stackage lts-18.28). This should not have any noticeable consequences for trident.
- *V 1.0.1.0* reintroduced a feature lost in *V 0.27.0*: You can now again list multiple `-f/--forgeString`s and `--forgeFile`s in `trident forge` to structure your input. `trident fetch` now also supports multiple `-f/--fetchString`s and `--fetchFile`s.
- *V 1.0.1.1* allowed `fetch` and `genoconvert` to create missing output directories automatically.
- *V 1.1.0.0* is a breaking change, because it deprecates the short genotype data input options (`-r` + `-g` + `-s` + `-i`) in `init`, `forge` and `genoconvert`. It also improved the input of package and genotype data in `forge` and `genoconvert` by making pointless no-input situations impossible.

### V 1.0.0.0

With this release we change to [PVP versioning](https://pvp.haskell.org). It introduces logging with the [co-log](https://hackage.haskell.org/package/co-log) library.

`trident` now supports different log modes, which can be set with the general argument `--logMode`. This change helps us as developers to structure the information shown on the command line, and thus improves the readability of the output messages. It also gives the user some control over which information they want to see. Consider the following example output for `trident validate -d . --logMode X`:

**NoLog** (hides all messages, only the progress indicator is shown)

```
> 151
```
 
**SimpleLog** (simple output to stderr, similar to the output before the log modes were introduced)

```
Searching POSEIDON.yml files... 
1 found
Checking Poseidon versions... 
Initializing packages... 
> 1 
Some packages were skipped due to issues:
In the package described in ./POSEIDON.yml:
File ./CHANGELOG.md does not exist
Packages loaded: 0
Validation failed
```

**DefaultLog** (default setting, adds severity indicators before each message)

```
[Info]    Searching POSEIDON.yml files... 
[Info]    1 found
[Info]    Checking Poseidon versions... 
[Info]    Initializing packages... 
> 1 
[Warning] Some packages were skipped due to issues:
[Warning] In the package described in ./POSEIDON.yml:
[Warning] File ./CHANGELOG.md does not exist
[Info]    Packages loaded: 0
[Error]   Validation failed
```

**ServerLog** (adds severity indicators and time stamps before each message)

```
[Info]    [21:53:28] Searching POSEIDON.yml files... 
[Info]    [21:53:28] 1 found
[Info]    [21:53:28] Checking Poseidon versions... 
[Info]    [21:53:28] Initializing packages... 
> 1 
[Warning] [21:53:28] Some packages were skipped due to issues:
[Warning] [21:53:28] In the package described in ./POSEIDON.yml:
[Warning] [21:53:28] File ./CHANGELOG.md does not exist
[Info]    [21:53:28] Packages loaded: 0
[Error]   [21:53:28] Validation failed
```

**VerboseLog**, finally, renders the messages just as `ServerLog`, but also shows messages with the severity level `Debug`. The other modes hide these.

This change deprecates the flag `-w/--warnings`, which turned on some more verbose warnings for `trident forge`. To see this information now, you have to set `--logMode VerboseLog`.

### V 0.29.0

This release brings two additions to the interface. They should make it more easy to work with unpackaged genotype files.

`trident genoconvert` gets the option `-o/--outPackagePath`, which allows to redirect the conversion output to any directory. If the input data is read from a POSEIDON package and this option is used, then the POSEIDON.yml file of the source package is not updated.

The second, more significant change is an additional interface option to input unpackaged genotype data. This affects the trident subcommands `init`, `genoconvert` and `forge`. Besides the verbose interface with `-r + -g + -s + -i`, it is now also possible to only give `-p/--genoOne` to fully describe one unpackaged genotype data set. `-p` takes one of the genotype data files (so `.bed`, `.bim` or `.fam` for PLINK or `.geno`, `.snp` or `.ind` for EIGENSTRAT) and determines based on its extension the data format (PLINK/EIGENSTRAT) and the paths to the other files forming the dataset (assuming they have the same name and are in the same directory).

Coming back to the `forge` example below for `V 0.28.0`, we can now for example write:

```
trident forge \
  -d 2017_GonzalesFortesCurrentBiology \
  -p 2017_HaberAJHG/2017_HaberAJHG.bed \
  -p 2018_VeeramahPNAS/2018_VeeramahPNAS.bed \
  -f "<STR241.SG>,<ERS1790729.SG>,Iberia_HG.SG" \
  -o testpackage
```

So we replaced the verbose 

```
-r PLINK -g 2017_HaberAJHG/2017_HaberAJHG.bed -s 2017_HaberAJHG/2017_HaberAJHG.bim -i 2017_HaberAJHG/2017_HaberAJHG.fam
-r PLINK -g 2018_VeeramahPNAS/2018_VeeramahPNAS.bed -i 2018_VeeramahPNAS/2018_VeeramahPNAS.fam -s 2018_VeeramahPNAS/2018_VeeramahPNAS.bim

```

with a much more concise 

```
-p 2017_HaberAJHG/2017_HaberAJHG.bed
-p 2018_VeeramahPNAS/2018_VeeramahPNAS.bed
```

to the same effect.

### V 0.28.0

This release introduces direct genotype data interaction for `trident genoconvert` and `trident forge`. Until now these two CLI subcommands could only be applied to valid Poseidon packages (as created e.g. by `trident init`). We now added a feature that renders the following calls possible:

```
trident genoconvert \
  -d 2015_LlorenteScience \
  -d 2015_FuNature \
  -r PLINK -g 2018_Mittnik_Baltic/Mittnik_Baltic.bed -s 2018_Mittnik_Baltic/Mittnik_Baltic.bim -i 2018_Mittnik_Baltic/Mittnik_Baltic.fam \
  -r PLINK -g 2010_RasmussenNature/2010_RasmussenNature.bed -i 2010_RasmussenNature/2010_RasmussenNature.fam -s 2010_RasmussenNature/2010_RasmussenNature.bim \
  --outFormat EIGENSTRAT
```

This converts the genotype data in two normal Poseidon packages (`2015_LlorenteScience` and `2015_FuNature`), but then ALSO the unpackaged PLINK datasets (`Mittnik_Baltic.bed/bim/fam` and `2010_RasmussenNature.bed/bim/fam`) to the EIGENSTRAT output format. So far `-d` was the only option to select which data to convert. With `-r + -g + -s + -i` we introduced a fully independent interface for interaction with "normal" "unpackaged" genotype data in (binary) PLINK or EIGENSTRAT format. Every call to `genoconvert` or `forge` now requires 0-n instances of `-d` or 0-n instances of `-r + -g + -s + -i`.

```
trident forge \
  -d 2017_GonzalesFortesCurrentBiology \
  -r PLINK -g 2017_HaberAJHG/2017_HaberAJHG.bed -s 2017_HaberAJHG/2017_HaberAJHG.bim -i 2017_HaberAJHG/2017_HaberAJHG.fam \
  -r PLINK -g 2018_VeeramahPNAS/2018_VeeramahPNAS.bed -i 2018_VeeramahPNAS/2018_VeeramahPNAS.fam -s 2018_VeeramahPNAS/2018_VeeramahPNAS.bim \
  -f "<STR241.SG>,<ERS1790729.SG>,Iberia_HG.SG" \
  -o testpackage
```

This compiles a new Poseidon package from the Poseidon package `2017_GonzalesFortesCurrentBiology` AND the unpackaged datasets `2017_HaberAJHG.bed/bim/fam` and `2018_VeeramahPNAS.bed/bim/fam`. The new package will contain individuals and groups from all three input datasets, making use of the powerful DSL we created to subset and merge packages in `trident forge`.

With this addition, trident can now be used independently of Poseidon packages (although it is still recommended to use them for data storage and management). A new option `--onlyGeno` allows to return only the genotype data and thus bypass the Poseidon infrastructure entirely.

See https://poseidon-framework.github.io/#/trident for the full documentation of these functions.
