### V 1.5.4.0

This bigger release adds a number of useful features to `trident`, some of them long requested. The highlights are ordered output for `forge`, a way to preserve key information if `forge` is applied to a singular source package, a new Web-API option to return the content of all available `.janno` columns, and better error messages for common `trident` issues.

#### Ordered `forge` output with `--ordered`

The order of samples in a Poseidon package created with `trident forge` depends on the order in which the relevant source packages are discovered by `trident` (e.g. when it crawls for packages in the `-d` base directories) and then the sample order within these packages. This mechanism did not allow for any convenient way to manually set the output order.

This release now adds a new option `--ordered`, which causes `trident` to output the resulting package with samples ordered according to the selection in `-f` or `--forgeFile`. This works through an alternative, slower sample selection algorithm that loops through the list of entities and checks for each entity which samples it adds or removes respectively from the final selection.

For simple, positive selection, packages, groups and samples are added as expected. Negative selection removes samples from the list again. If an entity is selected twice via positive selection, then its first occurence will be considered for the ordering.

#### Preserve the source package in `forge` with `--preservePyml`

For the specific task of subsetting an existing Poseidon package it can be useful to preserve some fields of the `POSEIDON.yml` file of the singular source package, as well as supplementary information in the `README.md` and the `CHANGELOG.md` file. These are typically discarded by `forge`, but can now be copied over to the output package with the new `--preservePyml` output mode. Naturally this only works with a single source package.

It specifically preserves the following `POSEIDON.yml` fields:

- `description`
- `contributor`
- `packageVersion`
- `lastModified`
- `readmeFile`
- `changelogFile`

Note that this does not include the package `title`, but this can be easily set to be identical to the source with `-n` or `-o` if it is desired. The `poseidonVersion` field is also not copied, because `trident` can only ever produce output packages with the latest Poseidon schema version. `--preservePyml` also copies the `README` and the `CHANGELOG` files to the output package.

One particular application of `--preservePyml` is the reordering of samples in an existing Poseidon package `MyPac` with the new `--ordered` flag. We suggest the following workflow for this application:

1. Generate a `--forgeFile` with the desired order of the samples in `MyPac`. This can be done manually or with any tool. Here is an example, where we employ `qjanno` to generate a `forge` selection so that the samples are ordered alphabetically and descendingly by their `Poseidon_ID`:

```bash
qjanno "SELECT '<'||Poseidon_ID||'>' FROM d(MyPac) ORDER BY Poseidon_ID DESC" --raw --noOutHeader > myOrder.txt
```

2. Use `trident forge` with `--ordered` and `--preservePyml` to create the package with the specified order:

```bash
trident forge -d MyPac --forgeFile myOrder.txt -o MyPac2 --ordered --preservePyml
```

3. Apply `trident rectify` to increment the package version number and document the reordering:

```bash
trident rectify -d MyPac2 --packageVersion Minor --logText "reordered the samples according to ..."
```

`MyPac2` then acts as a stand-in replacement for `MyPac` and only differ in the order of samples (and maybe variables/fields in the `POSEIDON.yml`, `.janno`, `.ssf` or `.bib` files). This workflow is not as convenient as an in-place reordering -- but much safer.

#### Request all `.janno` columns in `list` and the Web-API

`trident list --individuals` allows to acces per-sample information for Poseidon packages on the command line. The `-j` option allows to add arbitrary additional columns from the `.janno` files, here, for example, the `Country` and the `Genetic_Sex` columns:

```
 trident list -d 2010_RasmussenNature --individuals -j "Country" -j "Genetic_Sex"

.------------.---------------------.----------------------.----------------.-----------.-----------.-------------.
| Individual |        Group        |       Package        | PackageVersion | Is Latest |  Country  | Genetic_Sex |
:============:=====================:======================:================:===========:===========:=============:
| Inuk.SG    | Greenland_Saqqaq.SG | 2010_RasmussenNature | 2.1.1          | True      | Greenland | M           |
'------------'---------------------'----------------------'----------------'-----------'-----------'-------------'
```

This release adds a `--fullJanno` flag to request all columns at once, without having to list them individually with many `-j` arguments.

This convenience feature was also added to the Web-API, where it can be triggered with `?additionalJannoColumns=ALL` on the `/individuals` endpoint:

https://server.poseidon-adna.org/individuals?additionalJannoColumns=ALL

#### Better error messages





### V 1.5.0.1

This very minor release only affects the static `trident` executables produced for every release.

It introduces a distinction between pre-built `X64` and `ARM64` executables for macOS, where changes in the main processor architecture have recently rendered old builds invalid for new systems and vice versa.

That means the executable `trident-macOS` will henceforward not longer exist, but instead the executables `trident-macOS-X64` and `trident-macOS-ARM64`.

In the past we have not explicitly documented changes in the compilation pipeline - v1.5.0.0, for example, came with a major overhaul of the pipeline - but in this case a small version bump seems to be in order to announce the split in available artefacts.

### V 1.5.0.0

This is a minor, but technically breaking release. It removes the example contributor *Josiah Carberry* from new packages created by `trident init` and `trident forge` 

Previously every package created by `init` or `forge` included an example entry in the `contributor` field of the `POSEIDON.yml` file:

```yml
- name: Josiah Carberry
  email: carberry@brown.edu
  orcid: 0000-0002-1825-0097
```

This served the purpose of reminding users to actually set a contributor and giving an example how to do so. To simplify scripting with Poseidon packages we now remove this slightly gimmicky default.

To encourage setting the contributor field we instead introduce a reading/validation warning in case the `contributor` field is empty:

```
[Warning] Contributor missing in POSEIDON.yml file of package 2010_RasmussenNature-2.1.1
```

### V 1.4.1.0

This release adds an entirely new subcommand to merge two `.janno` files (`jannocoalesce`) and improves the error messages for broken `.janno` files.

#### Merging `.janno` files with `jannocoalesce`

The need for a tool to combine the information of two `.janno` files arose in the Poseidon ecosystem as we started to conceptualize the Poseidon [Minotaur Archive](https://github.com/poseidon-framework/minotaur-archive). This archive will be populated by paper-wise Poseidon packages for which the genotype data was regenerated through the Minotaur workflow (work in progress). We plan to reprocess various packages that are already in the Poseidon [Community Archive](https://github.com/poseidon-framework/community-archive) and for these packages we want to copy e.g. spatiotemporal information from the already available `.janno` files. `jannocoalesce` is the answer to this specific need, but can also be useful for various other applications.

It generally works by reading a source `.janno` file with `-s|--sourceFile` (or all `.janno` files in a `-d|--baseDir`) and a target `.janno` file with `-t|--targetFile`. It then merges these files by a key column, which can be selected with `--sourceKey` and `--targetKey`. The default for both of these key columns is the `Poseidon_ID`. In case the entries in the key columns slightly and systematically differ, e.g. because the `Poseidon_ID`s in either have a special suffix (for example `_SG`), then the `--stripIdRegex` option allows to strip these with a regular expression to thus match the keys.

`jannocoalesce` generally attempts to fill **all** empty cells in the target `.janno` file with information from the source. `--includeColumns` and `--excludeColumns` allow to select specific columns for which this should be done. In some cases it may be desirable to not just fill empty fields in the target, but overwrite the information already there with the `-f|--force` option. If the target file should be preserved, then the output can be directed to a new output `.janno` file with `-o|--outFile`.

#### Better error messages for broken `.janno` files

`.janno` file validation is a core feature of `trident`. With this release we try to improve the error messages for a two common situations:

1. Broken number fields. This can happen if some text or wrong character ends up in a number field.

So far the error messages for this case have been pretty technical. Here for example if an integer field is filled with `430;`, where the integer number `430` is accidentally written with a trailing `;`:

```
parse error (Failed reading: conversion error: expected Int, got "430;" (incomplete field parse, leftover: [59]))
```

The new error message is more clear:

```
parse error in one column (expected data type: Int, broken value: "430;", problematic characters: ";")
```

2. Inconsistent `Date_*`, `Contamination_*` and `Relation_*` columns. These sets of columns have to be cross-consistent, following a logic that is especially complex for the `Date_*` fields (see [here](https://www.poseidon-adna.org/#/janno_details?id=the-columns-in-detail)).

So far any inconsistency was reported with this generic error message:

```
The Date_* columns are not consistent
```

Now we include far more precise messages, like e.g.:

```
Date_Type is not "C14", but either Date_C14_Uncal_BP or Date_C14_Uncal_BP_Err are not empty.
```

This should simplify tedious `.janno` file debugging in the future.

### V 1.4.0.3

This small release fixes a performance issue related to finding the latest version of all packages. The bug had severe detrimental effects on `forge` and `fetch`, which are now resolved.

We used this opportunity to switch to a new GHC version and new versions of a lot of dependencies for building trident.

### V 1.4.0.2

This release finally fully enables handling multiple Poseidon package versions with trident. It includes a significant overhaul of the selection language in `forge` and `fetch` with major changes in its implementation and, as a consequence, multiple (subtle, but strictly breaking) changes in its semantics.

#### Handling multiple package versions

The trident subcommands `fetch`, `forge`, `genoconvert`, `list`, `rectify`, `survey` and `validate` now by default consider all versions of each Poseidon package in the given base directories. Previously all of them only considered the latest versions. If this old behaviour is desired now, it can be enabled with the flag `--onlyLatest` for the subcommands `genoconvert`, `list`, `rectify`, `survey` and `validate`. `fetch` and `forge` now generally consider all package versions (if we ignore the selection language semantics) and `summarize` continues to consider only the latest ones.

#### Changes to the selection language

In the `forge`- and `fetch`-selection language it is now possible to specify which version of a Poseidon package should be forged/fetched by appending the version number after a minus: e.g. `*2010_RasmussenNature-2.1.1*`. This also works for the more verbose and precise syntax to describe individuals, e.g. `<2010_RasmussenNature-2.1.1:Greenland_Saqqaq.SG:Inuk.SG>`.

While implementing this change, we also reworked the entity selection logic. It now adheres to the following rules:

**Inclusion queries**

* `*Pac1*`: Select all individuals in the latest version of package "Pac1"
* `*Pac1-1.0.1*`: Select all individuals in package "Pac1" with version "1.0.1"
* `Group1`: Select all individuals associated with "Group1" in all latest versions of all packages
* `<Ind1>`: Select the individual named "Ind1", searching in all latest packages.
* `<Pac1:Group1:Ind1>`: Select the individual named "Ind1" associated with "Group1" in the latest version of package "Pac1"
* `<Pac1-1.0.1:Group1:Ind1>`: Select the individual named "Ind1" associated with "Group1" in the package "Pac1" with version "1.0.1"

**Exclusion queries**

* `-*Pac1*`: Remove all individuals in all versions of package "Pac1"
* `-*Pac1-1.0.1*`: Remove only individuals in package "Pac1" with version "1.0.1" (but leave other versions in)
* `-Group1`: Remove all individuals associated with "Group1" in all versions of all packages (not just the latest)
* `-<Ind1>`: Remove all individuals named "Ind1" in all versions of all packages (not just the latest).
* `-<Pac1:Group1:Ind1>`: Remove the individual named "Ind1" associated with "Group1", searching in all versions of package "Pac1"
* `-<Pac1-1.0.1:Group1:Ind1>`: Remove the individual named "Ind1" associated with "Group1", but only if they are in "Pac1" with version "1.0.1"

Missing (or mis-spelled) entities in a selection-set lead to errors now.

If the forge entity list starts with a negative entity, or if the entity list is empty, `forge` will still implicitly assume you want to include all individuals from only the **latest** packages found in the baseDirs.
Likewise, `trident fetch --downloadAll` considers only latest versions now.

The specific individual selection syntax (with `-<Pac1-1.0.1:Group1:Ind1>`) does not perform automatic duplicate resolution any more. If there is another `<Ind1>` somewhere within the selected entities, then this will cause an error that has to be resolved manually by adjusting the selection.

#### Minor additional changes

- The Web API and the `list` subcommand now return an extra, boolean field/column `isLatest` to point out if an entity (individual, group, package) is from the latest package version.
- `list` now also returns column headers with the `--raw` flag. If they are not desired, then they have to be filtered out manually on the command line (e.g. with `trident list ... | tail -n+2`).
- Duplicate individuals in a package collection do not anymore lead to errors. Instead, only a selection for `forge` (and externally also in `xerxes`), if resulting in multiple individuals in the selection, will lead to errors. `validate` will also fail by default, except `--ignoreDuplicates` is set.

### V 1.3.0.4

This is a significant release with a breaking change, multiple new features and a number of minor fixes and improvements.

#### `rectify` replaces `update`

The subcommand `update` was renamed to `rectify` to better express its purpose. The name `update` suggested that this command could effectively migrate packages from one Poseidon version to the next, which was never the case. Structural and semantical changes to the package always have to be performed by the user through other means, so usually by manually editing the respective package files. `rectify` only helps to adjust a number of parameters (mostly in the POSEIDON.yml file) after the changes have been applied: It updates checksums, iterates version numbers, adds contributors and appends logging information to CHANGELOG files. Despite this limitation it is still a valuable tool, especially for the management of large package archives, where structural changes are often applied to many packages at once, all requiring "rectification" in the end.

`rectify` doesn't only introduce a different name, it also features a different interface. While `update` was a catch-all procedure with an opinionated, default behaviour that could partially be adjusted with various flags, `rectify` follows a much more transparent opt-in philosophy. The new interface allows to precisely choose which aspects of a package should be updated.

:warning: Please note that certain changes like incrementing the version number or adding a logText therefore do not happen automatically any more in `rectify`. They have to be requested explicitly!

Here is the new command line documentation of `rectify`:

```
Usage: trident rectify (-d|--baseDir DIR) [--ignorePoseidonVersion]
                       [--poseidonVersion ?.?.?]
                       [--packageVersion VPART [--logText STRING]]
                       [--checksumAll | [--checksumGeno] [--checksumJanno]
                         [--checksumSSF] [--checksumBib]]
                       [--newContributors DSL]

  Adjust POSEIDON.yml files automatically to package changes

Available options:
  -h,--help                Show this help text
  -d,--baseDir DIR         A base directory to search for Poseidon packages.
  --ignorePoseidonVersion  Read packages even if their poseidonVersion is not
                           compatible with trident.
  --poseidonVersion ?.?.?  Poseidon version the packages should be updated to:
                           e.g. "2.5.3".
  --packageVersion VPART   Part of the package version number in the
                           POSEIDON.yml file that should be updated: Major,
                           Minor or Patch (see https://semver.org).
  --logText STRING         Log text for this version in the CHANGELOG file.
  --checksumAll            Update all checksums.
  --checksumGeno           Update genotype data checksums.
  --checksumJanno          Update .janno file checksum.
  --checksumSSF            Update .ssf file checksum
  --checksumBib            Update .bib file checksum.
  --newContributors DSL    Contributors to add to the POSEIDON.yml file in the
                           form "[Firstname Lastname](Email address);...".
```

#### `serve` now provides different package archives and `list` and `fetch` can access them

`trident serve`, so the subcommand behind the server providing the Poseidon Web API, can now host packages from multiple named archives in parallel. This works through a modified `-d` interface on the command line and a new option `?archive=...` in the Web API. The client commands `fetch` and `list` can request information and package download from these different archives with a new option `--archive`. If `--archive` (or `?archive=...` in the http request) are not given, then the server falls back to a default archive (the first in `-d`).

See the Poseidon [public archive](https://www.poseidon-adna.org/#/archive_overview) and [Web API](https://www.poseidon-adna.org/#/web_api) documentation for the concrete consequences of this new feature.

#### `validate` can now check individual files

The `validate` subcommand is no longer confined to validating entire poseidon packages. It can still very much do so with `-d`, where -- just as before -- a number of optional flags can be used to control the exact behaviour. This release, in fact, adds the new options `--ignorePoseidonVersion` and `--ignoreChecksums` here. But `validate` can now also read, parse and thus check individual files: POSEIDON.yml files, genotype data files, .janno files, .ssf files or .bib files. This is tremendously useful for building packages step-by-step, e.g. in automatic pipelines.

:warning: Please note that these individual file checks naturally do not include the cross-file checks (e.g. publication keys across .janno and .bib). These are only available in the full package validation process.

Here is the new command line documentation of `validate`:

```
Usage: trident validate ((-d|--baseDir DIR) [--ignoreGeno] [--fullGeno]
                          [--ignoreDuplicates] [-c|--ignoreChecksums]
                          [--ignorePoseidonVersion] |
                          --pyml FILE | (-p|--genoOne FILE) | --inFormat FORMAT
                          --genoFile FILE --snpFile FILE --indFile FILE |
                          --janno FILE | --ssf FILE | --bib FILE) [--noExitCode]

  Check Poseidon packages or package components for structural correctness

Available options:
  -h,--help                Show this help text
  -d,--baseDir DIR         A base directory to search for Poseidon packages.
  --ignoreGeno             Ignore snp and geno file.
  --fullGeno               Test parsing of all SNPs (by default only the first
                           100 SNPs are probed).
  --ignoreDuplicates       Do not stop on duplicated individual names in the
                           package collection.
  -c,--ignoreChecksums     Whether to ignore checksums. Useful for speedup in
                           debugging.
  --ignorePoseidonVersion  Read packages even if their poseidonVersion is not
                           compatible with trident.
  --pyml FILE              Path to a POSEIDON.yml file.
  -p,--genoOne FILE        One of the input genotype data files. Expects .bed,
                           .bim or .fam for PLINK and .geno, .snp or .ind for
                           EIGENSTRAT. The other files must be in the same
                           directory and must have the same base name.
  --inFormat FORMAT        The format of the input genotype data: EIGENSTRAT or
                           PLINK. Only necessary for data input with --genoFile
                           + --snpFile + --indFile.
  --genoFile FILE          Path to the input geno file.
  --snpFile FILE           Path to the input snp file.
  --indFile FILE           Path to the input ind file.
  --janno FILE             Path to a .janno file.
  --ssf FILE               Path to a .ssf file.
  --bib FILE               Path to a .bib file.
  --noExitCode             Do not produce an explicit exit code.
```

#### Other, minor changes

- Fixed the behaviour of `forge` when combining .bib files. Publication duplicates are now properly removed upon merging and the output is alphabetically sorted.
- Added a global option `--debug`, which is short for `--logMode VerboseLog`.
- Refactored `summarise` to make the resulting counts more accurate. Some variables in the output table have been renamed as well.
- Fixed the behaviour of `chronicle` when updating a chronicle file (with `-u`): The `lastModified` field is now only touched if there is actually a change in the package list.
- Some cleaning of the general `trident` command line documentation: Added meaningful meta variables to all arguments.
- Shortened the default command line output of `fetch` to make it more readable.
- Slightly better error handling for failed http requests in `fetch` and `list --remote`.

### V 1.2.1.0

This release does not include changes for trident end users. 

It adds two new subcommands for (public) archive management, but they are only relevant from a developer's perspective: `chronicle` creates or updates a dedicated .yml file to document version iterations of Poseidon packages in a Git-managed archive, and `timetravel` recovers package iterations based on this file to (re)construct said archive from the source repository.

Just as `serve`, both new subcommands will be omitted in the command line help.

### V 1.2.0.0

This release comes with a massive rewrite of the server-client infrastructure, so the code behind the API to download and list packages from our public data archives.

The server is now implemented as a (hidden) subcommand of trident: `serve`. It returns helpful error messages, if an incompatible version of trident tries to connect to it. And it is now capable of serving multiple (so not just the latest, but also older) versions of one package, which is an important step towards computational reproducibility of Poseidon-based pipelines.

All Server-APIs except for `zip_file` now return a complex JSON datatype with server messages and a payload. The messages contain standard messages like a greeting and in the future perhaps also deprecation warnings. Some APIs also provide information or warnings in the server messages.

All APIs except for `zip_file` also accept an additional parameter `?client_version=X.X.X`, so that the server may in the future respond to old clients that an update is needed in order to understand the API. Our `trident list --remote` functionality already makes use of this.

Here are the individual APIs:

- `https://server.poseidon-adna.org/packages`: returns a list of all packages.
- `https://server.poseidon-adna.org/groups`: returns a list of all groups.
- `https://server.poseidon-adna.org/individuals`: returns a list of all individuals.
- `https://server.poseidon-adna.org/zip_file/<package_name>?package_version=1.0.1`: returns a zip file of the package with the given name and the given version. If no version is given, it returns the latest.

The client subcommands `fetch` and `list` can not yet make full use of this new API in this release, because they lack an interface to request specific package versions. This will be added in a future release. But the output of both subcommands already differs from the previous implementation:

- `fetch` now appends the package version to the directory name when downloading a package. Previously `trident fetch -d . -f "*2010_RasmussenNature*"` yielded a directory named `2010_RasmussenNature` with the package data, but now it creates `2010_RasmussenNature-2.0.1` (or whatever is the latest version of this package in the archive).
- `fetch` does no longer have an option `--upgrade`, since the new behaviour respects different versions to live side by side in different directories. If users whish to remove old versions, they should do so manually.
- `list` lists not just data (package, groups, individuals) for the latest version of a package, but for all versions. The package version became an explicit output column.

As before, `forge` and the other subcommands keep ignoring multiple package versions for now, and only read the latest.

The new server is available at a new URL (https://server.poseidon-adna.org), but the old version at (https://c107-224.cloud.gwdg.de) will also keep running for now. New releases of trident (v1.2.0.0+) will by default use the new server, while older versions must connect to the old one.

### V 1.1.12.0

This release implements the changes necessary for the Poseidon schema v2.7.1. That mostly means that the constraints on several .ssf file columns previously considered mandatory and unique were lifted.

Beyond that a number of type constraints specified already in Poseidon v2.7.0 for the .ssf file were finally implemented in poseidon-hs. A broken file will, thus actually be flagged upon reading if it violates the following requirements:

- .ssf columns that include Accession_IDs have to feature the correct and valid Accession_IDs according to INSDC specification.
- .ssf columns with dates have to be valid dates of the form `YYYY-MM-DD`.
- .ssf columns featuring MD5 hashes require entries with exactly 32 hex-digits.

Both for the .janno and the .ssf file we elevated the log level of the common `broken lines` error from debug to error. This makes these errors more prominent and more easy to resolve.

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
