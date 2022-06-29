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