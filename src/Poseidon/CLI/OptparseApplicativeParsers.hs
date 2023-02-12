{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.OptparseApplicativeParsers where

import           Poseidon.CLI.List       (ListEntity (..),
                                          RepoLocationSpec (..))
import           Poseidon.EntitiesList   (EntitiesList, EntityInput (..),
                                          PoseidonEntity, SignedEntitiesList,
                                          SignedEntity, readEntitiesFromString)
import           Poseidon.GenotypeData   (GenoDataSource (..),
                                          GenotypeDataSpec (..),
                                          GenotypeFormatSpec (..),
                                          SNPSetSpec (..))
import           Poseidon.SecondaryTypes (ContributorSpec (..),
                                          VersionComponent (..),
                                          contributorSpecParser,
                                          poseidonVersionParser, runParser)
import           Poseidon.Utils          (LogMode (..))


import           Control.Applicative     ((<|>))
import           Data.Version            (Version)
import qualified Options.Applicative     as OP
import           SequenceFormats.Plink   (PlinkPopNameMode (PlinkPopNameAsBoth, PlinkPopNameAsFamily, PlinkPopNameAsPhenotype))
import           System.FilePath         (dropExtension, takeExtension, (<.>))
import           Text.Read               (readMaybe)

parsePoseidonVersion :: OP.Parser (Maybe Version)
parsePoseidonVersion = OP.option (Just <$> OP.eitherReader readPoseidonVersionString) (
    OP.long "poseidonVersion" <>
    OP.help "Poseidon version the packages should be updated to: \
            \e.g. \"2.5.3\"" <>
    OP.value Nothing <>
    OP.showDefault
    )
    where
        readPoseidonVersionString :: String -> Either String Version
        readPoseidonVersionString s = case runParser poseidonVersionParser () "" s of
            Left p  -> Left (show p)
            Right x -> Right x

parseLogMode :: OP.Parser LogMode
parseLogMode = OP.option (OP.eitherReader readLogMode) (
    OP.long "logMode" <>
    OP.help "How information should be reported: \
            \NoLog, SimpleLog, DefaultLog, ServerLog or VerboseLog" <>
    OP.value DefaultLog <>
    OP.showDefault
    )
    where
        readLogMode :: String -> Either String LogMode
        readLogMode s = case s of
            "NoLog"      -> Right NoLog
            "SimpleLog"  -> Right SimpleLog
            "DefaultLog" -> Right DefaultLog
            "ServerLog"  -> Right ServerLog
            "VerboseLog" -> Right VerboseLog
            _            -> Left "must be NoLog, SimpleLog, DefaultLog, ServerLog or VerboseLog"

data ErrorLength = CharInf | CharCount Int deriving Show

parseErrorLength :: OP.Parser ErrorLength
parseErrorLength = OP.option (OP.eitherReader readErrorLengthString) (
    OP.long "errLength" <>
    OP.help "After how many characters should a potential error message be truncated. \"Inf\" for no truncation." <>
    OP.value (CharCount 1500) <>
    OP.showDefault
    ) where
        readErrorLengthString :: String -> Either String ErrorLength
        readErrorLengthString s = do
            if s == "Inf"
            then Right CharInf
            else case readMaybe s of
                Just n  -> Right $ CharCount n
                Nothing -> Left "must be either \"Inf\" or an integer number"

parseRemoveOld :: OP.Parser Bool
parseRemoveOld = OP.switch (OP.long "removeOld" <> OP.help "Remove the old genotype files when creating the new ones")

parseVersionComponent :: OP.Parser VersionComponent
parseVersionComponent = OP.option (OP.eitherReader readVersionComponent) (
    OP.long "versionComponent" <>
    OP.help "Part of the package version number in the POSEIDON.yml file \
            \that should be updated: \
            \Major, Minor or Patch (see https://semver.org)" <>
    OP.value Patch <>
    OP.showDefault
    )
    where
        readVersionComponent :: String -> Either String VersionComponent
        readVersionComponent s = case s of
            "Major" -> Right Major
            "Minor" -> Right Minor
            "Patch" -> Right Patch
            _       -> Left "must be Major, Minor or Patch"

parseNoChecksumUpdate :: OP.Parser Bool
parseNoChecksumUpdate = OP.switch (
    OP.long "noChecksumUpdate" <>
    OP.help "Should update of checksums in the POSEIDON.yml file be skipped"
    )

parseContributors :: OP.Parser [ContributorSpec]
parseContributors = concat <$> OP.many (OP.option (OP.eitherReader readContributorString) (
    OP.long "newContributors" <>
    OP.help "Contributors to add to the POSEIDON.yml file \
            \ in the form \"[Firstname Lastname](Email address);...\""
    ))
    where
        readContributorString :: String -> Either String [ContributorSpec]
        readContributorString s = case runParser contributorSpecParser () "" s of
            Left p  -> Left (show p)
            Right x -> Right x


parseLog :: OP.Parser String
parseLog = OP.strOption (
    OP.long "logText" <>
    OP.help "Log text for this version jump in the CHANGELOG file" <>
    OP.value "not specified" <>
    OP.showDefault
    )

parseForce :: OP.Parser Bool
parseForce = OP.switch (
    OP.long "force" <>
    OP.help "Normally the POSEIDON.yml files are only changed if the \
            \poseidonVersion is adjusted or any of the checksums change. \
            \With --force a package version update can be triggered even \
            \if this is not the case."
    )

-- this will also parse an empty list, which means "forge everything".
parseForgeEntityInputs :: OP.Parser [EntityInput SignedEntity]
parseForgeEntityInputs = OP.many parseSignedEntityInput
  where
    parseSignedEntityInput = (EntitiesFromFile <$> parseForgeEntitiesFromFile) <|> (EntitiesDirect <$> parseForgeEntitiesDirect)

--this will not parse an empty list, but requires the user to specify `downloadAll`. I made this decision to not change the API
-- for the user, even though internally "downloadAll" is represented as an empty list.
parseFetchEntityInputs :: OP.Parser [EntityInput PoseidonEntity]
parseFetchEntityInputs = parseDownloadAll <|> OP.some parseEntityInput
  where
    parseDownloadAll = OP.flag' [] (
        OP.long "downloadAll" <>
        OP.help "download all packages the server is offering"
        )
    parseEntityInput = (EntitiesFromFile <$> parseFetchEntitiesFromFile) <|> (EntitiesDirect <$> parseFetchEntitiesDirect)


parseIgnorePoseidonVersion :: OP.Parser Bool
parseIgnorePoseidonVersion = OP.switch (
    OP.long "ignorePoseidonVersion" <>
    OP.help "Read packages even if their poseidonVersion is not compatible with the trident version. \
        \The assumption is, that the package is already structurally adjusted to the trident version \
        \and only the version number is lagging behind."
    )

parseForgeEntitiesDirect :: OP.Parser SignedEntitiesList
parseForgeEntitiesDirect = OP.option (OP.eitherReader readSignedEntities) (OP.long "forgeString" <>
    OP.short 'f' <>
    OP.help "List of packages, groups or individual samples to be combined in the output package. \
        \Packages follow the syntax *package_title*, populations/groups are simply group_id and individuals \
        \<individual_id>. You can combine multiple values with comma, so for example: \
        \\"*package_1*, <individual_1>, <individual_2>, group_1\". Duplicates are treated as one entry. \
        \Negative selection is possible by prepending \"-\" to the entity you want to exclude \
        \(e.g. \"*package_1*, -<individual_1>, -group_1\"). \
        \forge will apply excludes and includes in order. If the first entity is negative, then forge \
        \will assume you want to merge all individuals in the packages found in the baseDirs (except the \
        \ones explicitly excluded) before the exclude entities are applied. \
        \An empty forgeString (and no --forgeFile) will therefore merge all available individuals. \
        \If there are individuals in your input packages with equal individual id, but different main group or \
        \source package, they can be specified with the special syntax \"<package:group:individual>\".")
  where
    readSignedEntities s = case readEntitiesFromString s of
        Left e  -> Left (show e)
        Right e -> Right e

parseFetchEntitiesDirect :: OP.Parser EntitiesList
parseFetchEntitiesDirect = OP.option (OP.eitherReader readEntities) (OP.long "fetchString" <>
    OP.short 'f' <>
    OP.help "List of packages to be downloaded from the remote server. \
        \Package names should be wrapped in asterisks: *package_title*. \
        \You can combine multiple values with comma, so for example: \"*package_1*, *package_2*, *package_3*\". \
        \fetchString uses the same parser as forgeString, but does not allow excludes. If groups or individuals are \
        \specified, then packages which include these groups or individuals are included in the download.")
  where
    readEntities s = case readEntitiesFromString s of
        Left e  -> Left (show e)
        Right e -> Right e

parseForgeEntitiesFromFile :: OP.Parser FilePath
parseForgeEntitiesFromFile = OP.strOption (OP.long "forgeFile" <>
    OP.help "A file with a list of packages, groups or individual samples. \
        \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
        \Empty lines are ignored and comments start with \"#\", so everything after \"#\" is ignored \
        \in one line. \
        \Multiple instances of -f and --forgeFile can be given. They will be evaluated according to their \
        \input order on the command line.")

parseFetchEntitiesFromFile :: OP.Parser FilePath
parseFetchEntitiesFromFile = OP.strOption (OP.long "fetchFile" <>
    OP.help "A file with a list of packages. \
        \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
        \-f and --fetchFile can be combined.")

parseIntersect :: OP.Parser Bool
parseIntersect = OP.switch (OP.long "intersect" <>
    OP.help "Whether to output the intersection of the genotype files to be forged. \
        \The default (if this option is not set) is to output the union of all SNPs, with genotypes \
        \defined as missing in those packages which do not have a SNP that is present in another package. \
        \With this option set, the forged dataset will typically have fewer SNPs, but less missingness.")

parseRemoteDummy :: OP.Parser ()
parseRemoteDummy = OP.flag' () (OP.long "remote" <> OP.help "list packages from a remote server instead the local file system")

parseOutGenotypeFormat :: Bool -> OP.Parser GenotypeFormatSpec
parseOutGenotypeFormat withDefault =
  if withDefault
  then OP.option (OP.eitherReader readGenotypeFormat) settingsWithDefault
  else OP.option (OP.eitherReader readGenotypeFormat) settingsWithoutDefault
  where
    settingsWithDefault = OP.long "outFormat" <>
        OP.help "the format of the output genotype data: EIGENSTRAT or PLINK. Default: PLINK" <>
        OP.value GenotypeFormatPlink
    settingsWithoutDefault = OP.long "outFormat" <>
      OP.help "the format of the output genotype data: EIGENSTRAT or PLINK."
    readGenotypeFormat :: String -> Either String GenotypeFormatSpec
    readGenotypeFormat s = case s of
        "EIGENSTRAT" -> Right GenotypeFormatEigenstrat
        "PLINK"      -> Right GenotypeFormatPlink
        _            -> Left "must be EIGENSTRAT or PLINK"

parseGenoDataSources :: OP.Parser [GenoDataSource]
parseGenoDataSources = OP.some parseGenoDataSource

parseGenoDataSource :: OP.Parser GenoDataSource
parseGenoDataSource = (PacBaseDir <$> parseBasePath) <|> (GenoDirect <$> parseInGenotypeDataset)

parseRepoLocation :: OP.Parser RepoLocationSpec
parseRepoLocation = (RepoLocal <$> parseBasePaths) <|> (parseRemoteDummy *> (RepoRemote <$> parseRemoteURL))

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some parseBasePath

parseBasePath :: OP.Parser FilePath
parseBasePath = OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages (could be a Poseidon repository)")

parseInGenotypeDataset :: OP.Parser GenotypeDataSpec
parseInGenotypeDataset = createGeno <$> (parseInGenoOne <|> parseInGenoSep) <*> parseGenotypeSNPSet
    where
        createGeno :: GenoInput -> Maybe SNPSetSpec -> GenotypeDataSpec
        createGeno (a,b,c,d) e = GenotypeDataSpec a b Nothing c Nothing d Nothing e

type GenoInput = (GenotypeFormatSpec, FilePath, FilePath, FilePath)

parseInGenoOne :: OP.Parser GenoInput
parseInGenoOne = OP.option (OP.eitherReader readGenoInput) (
        OP.short 'p' <> OP.long "genoOne" <> OP.help
            "one of the input genotype data files. Expects\
            \ .bed  or .bim or .fam for PLINK and\
            \ .geno or .snp or .ind for EIGENSTRAT.\
            \ The other files must be in the same directory and must have the same base name")
    where
        readGenoInput :: FilePath -> Either String GenoInput
        readGenoInput p = makeGenoInput (dropExtension p) (takeExtension p)
        makeGenoInput path ext
            | ext `elem` [".geno", ".snp", ".ind"] =
                Right (GenotypeFormatEigenstrat, path <.> "geno", path <.> "snp", path <.> "ind")
            | ext `elem` [".bed", ".bim", ".fam"]  =
                Right (GenotypeFormatPlink,      path <.> "bed",  path <.> "bim", path <.> "fam")
            | otherwise = Left $ "unknown file extension: " ++ ext

parseInGenoSep :: OP.Parser GenoInput
parseInGenoSep = (,,,) <$> parseInGenotypeFormat <*> parseInGenoFile <*> parseInSnpFile <*> parseInIndFile

parseInGenotypeFormat :: OP.Parser GenotypeFormatSpec
parseInGenotypeFormat = OP.option (OP.eitherReader readGenotypeFormat) (
    OP.long "inFormat" <>
    OP.help "the format of the input genotype data: EIGENSTRAT or PLINK\
            \ (only necessary for data input with --genoFile + --snpFile + --indFile)")
  where
    readGenotypeFormat :: String -> Either String GenotypeFormatSpec
    readGenotypeFormat s = case s of
        "EIGENSTRAT" -> Right GenotypeFormatEigenstrat
        "PLINK"      -> Right GenotypeFormatPlink
        _            -> Left "must be EIGENSTRAT or PLINK"

parseInGenoFile :: OP.Parser FilePath
parseInGenoFile = OP.strOption (
    OP.long "genoFile" <>
    OP.help "the input geno file path")

parseInSnpFile :: OP.Parser FilePath
parseInSnpFile = OP.strOption (
    OP.long "snpFile" <>
    OP.help "the input snp file path")

parseInIndFile :: OP.Parser FilePath
parseInIndFile = OP.strOption (
    OP.long "indFile" <>
    OP.help "the input ind file path")

parseGenotypeSNPSet :: OP.Parser (Maybe SNPSetSpec)
parseGenotypeSNPSet = OP.option (Just <$> OP.eitherReader readSnpSet) (OP.long "snpSet" <>
    OP.help "the snpSet of the package: 1240K, HumanOrigins or Other. \
            \(only relevant for data input with -p|--genoOne or --genoFile + --snpFile + --indFile, \
            \because the packages in a -d|--baseDir already have this information in their respective \
            \POSEIDON.yml files) Default: Other" <>
    OP.value (Just SNPSetOther))
  where
    readSnpSet :: String -> Either String SNPSetSpec
    readSnpSet s = case s of
        "1240K"        -> Right SNPSet1240K
        "HumanOrigins" -> Right SNPSetHumanOrigins
        "Other"        -> Right SNPSetOther
        _              -> Left "Could not read snpSet. Must be \"1240K\", \
                                \\"HumanOrigins\" or \"Other\""

parseOutPackagePath :: OP.Parser FilePath
parseOutPackagePath = OP.strOption (OP.long "outPackagePath" <>
    OP.short 'o' <>
    OP.help "the output package directory path")

parseMaybeOutPackagePath :: OP.Parser (Maybe FilePath)
parseMaybeOutPackagePath = OP.option (Just <$> OP.str) (
    OP.short 'o' <>
    OP.long "outPackagePath" <>
    OP.help "the output package directory path - this is optional: If no path is provided, \
            \then the output is written to the directories where the input genotype data file \
            \(.bed/.geno) is stored" <>
    OP.value Nothing
    )

parseMaybeOutPackageName :: OP.Parser (Maybe String)
parseMaybeOutPackageName = OP.option (Just <$> OP.str) (
    OP.short 'n' <>
    OP.long "outPackageName" <>
    OP.help "the output package name - this is optional: If no name is provided, \
            \then the package name defaults to the basename of the (mandatory) \
            \--outPackagePath argument" <>
    OP.value Nothing
    )

parseMakeMinimalPackage :: OP.Parser Bool
parseMakeMinimalPackage = OP.switch (OP.long "minimal" <>
    OP.help "should only a minimal output package be created?")

parseOutOnlyGeno :: OP.Parser Bool
parseOutOnlyGeno = OP.switch (OP.long "onlyGeno" <>
    OP.help "should only the resulting genotype data be returned? This means the output will not be a Poseidon package")

parsePackageWise :: OP.Parser Bool
parsePackageWise = OP.switch (OP.long "packagewise" <> OP.help "Skip the within-package selection step in forge. \
    \This will result in \
    \outputting all individuals in the relevant packages, and hence a superset of the requested \
    \individuals/groups. It may result in better performance in cases where one wants to forge entire packages or \
    \almost entire packages. Details: Forge conceptually performs two types of selection: First, it identifies which packages in the supplied base directories are relevant to the requested forge, i.e. whether they are either explicitly listed using *PackageName*, or because they contain selected individuals or groups. Second, within each relevant package, individuals which are not requested are removed. This option skips only the second step, but still performs the first.")

parseMaybeSnpFile :: OP.Parser (Maybe FilePath)
parseMaybeSnpFile = OP.option (Just <$> OP.str) (OP.value Nothing <> OP.long "selectSnps" <>
    OP.help "To extract specific SNPs during this forge operation, provide a Snp file. \
    \Can be either Eigenstrat (file ending must be '.snp') or Plink (file ending must be '.bim'). \
    \When this option is set, the output package will have exactly the SNPs listed in this file. Any SNP not \
    \listed in the file will be excluded. If option '--intersect' is also set, only the SNPs overlapping between the SNP file \
    \and the forged packages are output.")

parseListEntity :: OP.Parser ListEntity
parseListEntity = parseListPackages <|> parseListGroups <|> (parseListIndividualsDummy *> parseListIndividualsExtraCols)
  where
    parseListPackages = OP.flag' ListPackages (OP.long "packages" <> OP.help "list all packages")
    parseListGroups = OP.flag' ListGroups (OP.long "groups" <> OP.help "list all groups, ignoring any group names after the first as specified in the Janno-file")
    parseListIndividualsDummy = OP.flag' () (OP.long "individuals" <> OP.help "list individuals")
    parseListIndividualsExtraCols = ListIndividuals <$> OP.many parseExtraCol
    parseExtraCol = OP.strOption (OP.short 'j' <> OP.long "jannoColumn" <> OP.metavar "JANNO_HEADER" <>
        OP.help "list additional fields from the janno files, using the Janno column heading name, such as \
        \Country, Site, Date_C14_Uncal_BP, Endogenous, ...")

parseRawOutput :: OP.Parser Bool
parseRawOutput = OP.switch (
    OP.long "raw" <>
    OP.help "output table as tsv without header. Useful for piping into grep or awk"
    )

parseIgnoreGeno :: OP.Parser Bool
parseIgnoreGeno = OP.switch (
    OP.long "ignoreGeno" <>
    OP.help "ignore SNP and GenoFile" <>
    OP.hidden
    )

parseNoExitCode :: OP.Parser Bool
parseNoExitCode = OP.switch (
    OP.long "noExitCode" <>
    OP.help "do not produce an explicit exit code" <>
    OP.hidden
    )

parseIgnoreDuplicates :: OP.Parser Bool
parseIgnoreDuplicates = OP.switch (
    OP.long "ignoreDuplicates" <>
    OP.help "do not stop on duplicated individual names in the package collection" <>
    OP.hidden
    )

parseRemoteURL :: OP.Parser String
parseRemoteURL = OP.strOption (
    OP.long "remoteURL" <>
    OP.help "URL of the remote Poseidon server" <>
    OP.value "https://c107-224.cloud.gwdg.de" <>
    OP.showDefault
    )

parseUpgrade :: OP.Parser Bool
parseUpgrade = OP.switch (
    OP.long "upgrade" <>  OP.short 'u' <>
    OP.help "overwrite outdated local package versions"
    )

-- PlinkPopNameAsFamily always is the default
parsePlinkPopMode :: OP.Parser PlinkPopNameMode
parsePlinkPopMode = OP.option (OP.eitherReader readPlinkPopName) (
    OP.long "plinkPopName" <> OP.help "In Plink Format, the *.fam file encodes information about individuals. \
        \But there is no natural rule how to encode the group name. By default, trident assumes that the group name \
        \is encoded in the first column of the fam file, which according to the Plink documentation is the \"Family ID\". \
        \Some other tools, such as Admixtools and Eigensoft prefer the group name to be encoded in the last column instead \
        \(the phenotype according to the Plink documentation). This option lets the user specify this encoding explicitly. \
        \Three options are possible: asFamily (default) | asPhenotype | asBoth. With the latter, the population name is written to \
        \both columns, and read from both columns (differing names are merged if necessary)")
  where
    readPlinkPopName :: String -> Either String PlinkPopNameMode
    readPlinkPopName s = case s of
        "asFamily"    -> Right PlinkPopNameAsFamily
        "asPhenotype" -> Right PlinkPopNameAsPhenotype
        "asBoth"      -> Right PlinkPopNameAsBoth
        _             -> Left "must be asFamily, asPhenotype or asBoth"
