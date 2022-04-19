### V ...

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