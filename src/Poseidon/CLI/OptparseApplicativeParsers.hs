{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.OptparseApplicativeParsers where

import           Poseidon.CLI.Chronicle     (ChronOperation (..))
import           Poseidon.CLI.Forge         (ForgeOutMode (..))
import           Poseidon.CLI.Jannocoalesce (CoalesceJannoColumnSpec (..),
                                             JannoSourceSpec (..))
import           Poseidon.CLI.List          (ListEntity (..),
                                             RepoLocationSpec (..))
import           Poseidon.CLI.Rectify       (ChecksumsToRectify (..),
                                             PackageVersionUpdate (..))
import           Poseidon.CLI.Validate      (ValidatePlan (..))
import           Poseidon.Contributor       (ContributorSpec (..),
                                             contributorSpecParser)
import           Poseidon.EntityTypes       (EntitiesList, EntityInput (..),
                                             PoseidonEntity, SignedEntitiesList,
                                             SignedEntity,
                                             readEntitiesFromString)
import           Poseidon.GenotypeData      (GenoDataSource (..),
                                             GenotypeDataSpec (..),
                                             GenotypeFileSpec (..),
                                             SNPSetSpec (..))
import           Poseidon.ServerClient      (AddColSpec (..),
                                             ArchiveEndpoint (..))
import           Poseidon.Utils             (ErrorLength (..), LogMode (..),
                                             TestMode (..),
                                             renderPoseidonException,
                                             showParsecErr)
import           Poseidon.Version           (VersionComponent (..),
                                             parseVersion)

import           Control.Applicative        ((<|>))
import qualified Data.ByteString.Char8      as BSC
import           Data.List                  (intercalate)
import           Data.List.Split            (splitOn)
import           Data.Version               (Version)
import qualified Options.Applicative        as OP
import           SequenceFormats.Plink      (PlinkPopNameMode (PlinkPopNameAsBoth, PlinkPopNameAsFamily, PlinkPopNameAsPhenotype))
import           System.FilePath            (splitExtension, splitExtensions,
                                             takeExtension, (<.>))
import qualified Text.Parsec                as P
import           Text.Read                  (readMaybe)

parseChronOperation :: OP.Parser ChronOperation
parseChronOperation = (CreateChron <$> parseChronOutPath) <|> (UpdateChron <$> parseChronUpdatePath)

parseTimetravelSourcePath :: OP.Parser FilePath
parseTimetravelSourcePath = OP.strOption (
    OP.long "srcDir" <>
    OP.short 's' <>
    OP.metavar "DIR" <>
    OP.help "Path to the Git-versioned source directory where the chronFile applies.")

parseTimetravelChronPath :: OP.Parser FilePath
parseTimetravelChronPath = OP.strOption (
    OP.long "chronFile" <>
    OP.short 'c' <>
    OP.metavar "FILE" <>
    OP.help "Path to the chronicle definition file.")

parseChronOutPath :: OP.Parser FilePath
parseChronOutPath = OP.strOption (
    OP.long "outFile" <>
    OP.short 'o' <>
    OP.metavar "FILE" <>
    OP.help "Path to the resulting chronicle definition file.")

parseChronUpdatePath :: OP.Parser FilePath
parseChronUpdatePath = OP.strOption (
    OP.long "updateFile" <>
    OP.short 'u' <>
    OP.metavar "FILE" <>
    OP.help "Path to the chronicle definition file that should be updated. \
            \This file will be overwritten! \
            \But the update procedure does not change the package entries \
            \that are already in the chronicle definition file. \
            \It only adds new entries.")

parseMaybePoseidonVersion :: OP.Parser (Maybe Version)
parseMaybePoseidonVersion = OP.option (Just <$> OP.eitherReader readPoseidonVersionString) (
    OP.long "poseidonVersion" <>
    OP.metavar "?.?.?" <>
    OP.help "Poseidon version the packages should be updated to: \
            \e.g. \"2.5.3\"." <>
    OP.value Nothing
    )
    where
        readPoseidonVersionString :: String -> Either String Version
        readPoseidonVersionString s = case P.runParser parseVersion () "" s of
            Left p  -> Left (showParsecErr p)
            Right x -> Right x

parseDebugMode :: OP.Parser LogMode
parseDebugMode = OP.flag' VerboseLog (
    OP.long "debug" <>
    OP.help "Short for --logMode VerboseLog."
    )

parseLogMode :: OP.Parser LogMode
parseLogMode = OP.option (OP.eitherReader readLogMode) (
    OP.long "logMode" <>
    OP.metavar "MODE" <>
    OP.help "How information should be reported: \
            \NoLog, SimpleLog, DefaultLog, ServerLog or VerboseLog." <>
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

parseTestMode :: OP.Parser TestMode
parseTestMode = OP.option (OP.eitherReader readTestMode) (
    OP.long "testMode" <>
    OP.metavar "MODE" <>
    OP.help "\"Testing\" activates a test mode; relevant only \
            \for developers in very specific edge cases." <>
    OP.value Production <>
    OP.showDefault <>
    OP.internal
    )
    where
        readTestMode :: String -> Either String TestMode
        readTestMode s = case s of
            "Testing"    -> Right Testing
            "Production" -> Right Production
            _            -> Left "must be Testing or Production"

parseErrorLength :: OP.Parser ErrorLength
parseErrorLength = OP.option (OP.eitherReader readErrorLengthString) (
    OP.long "errLength" <>
    OP.metavar "INT" <>
    OP.help "After how many characters should a potential genotype data parsing error \
            \message be truncated. \"Inf\" for no truncation." <>
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
parseRemoveOld = OP.switch (
    OP.long "removeOld" <>
    OP.help "Remove the old genotype files when creating the new ones."
    )

parseChecksumsToRectify :: OP.Parser ChecksumsToRectify
parseChecksumsToRectify = parseChecksumNone <|> parseChecksumAll <|> parseChecksumsDetail
    where
        parseChecksumNone :: OP.Parser ChecksumsToRectify
        parseChecksumNone = pure ChecksumNone
        parseChecksumAll :: OP.Parser ChecksumsToRectify
        parseChecksumAll = ChecksumAll <$
            OP.flag' () (
                OP.long "checksumAll" <>
                OP.help "Update all checksums.")
        parseChecksumsDetail :: OP.Parser ChecksumsToRectify
        parseChecksumsDetail = ChecksumsDetail <$>
            parseChecksumGeno <*>
            parseChecksumJanno <*>
            parseChecksumSSF <*>
            parseChecksumBib
        parseChecksumGeno :: OP.Parser Bool
        parseChecksumGeno = OP.switch (
            OP.long "checksumGeno" <>
            OP.help "Update genotype data checksums.")
        parseChecksumJanno :: OP.Parser Bool
        parseChecksumJanno = OP.switch (
            OP.long "checksumJanno" <>
            OP.help "Update .janno file checksum.")
        parseChecksumSSF :: OP.Parser Bool
        parseChecksumSSF = OP.switch (
            OP.long "checksumSSF" <>
            OP.help "Update .ssf file checksum")
        parseChecksumBib :: OP.Parser Bool
        parseChecksumBib = OP.switch (
            OP.long "checksumBib" <>
            OP.help "Update .bib file checksum.")

parseMaybePackageVersionUpdate :: OP.Parser (Maybe PackageVersionUpdate)
parseMaybePackageVersionUpdate = OP.optional $ PackageVersionUpdate <$> parseVersionComponent <*> parseMaybeLog

parseVersionComponent :: OP.Parser VersionComponent
parseVersionComponent = OP.option (OP.eitherReader readVersionComponent) (
    OP.long "packageVersion" <>
    OP.metavar "VPART" <>
    OP.help "Part of the package version number in the POSEIDON.yml file \
            \that should be updated: \
            \Major, Minor or Patch (see https://semver.org)."
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
    OP.help "Should the update of checksums in the POSEIDON.yml file be skipped?"
    )

parseMaybeContributors :: OP.Parser [ContributorSpec]
parseMaybeContributors = OP.option (OP.eitherReader readContributorString) (
    OP.long "newContributors" <>
    OP.metavar "DSL" <>
    OP.help "Contributors to add to the POSEIDON.yml file \
            \ in the form \"[Firstname Lastname](Email address);...\"." <>
    OP.value []
    )

parseContributors :: OP.Parser [ContributorSpec]
parseContributors = OP.option (OP.eitherReader readContributorString) (
    OP.long "newContributors" <>
    OP.metavar "DSL" <>
    OP.help "Contributors to add to the POSEIDON.yml file \
            \ in the form \"[Firstname Lastname](Email address);...\"."
    )

readContributorString :: String -> Either String [ContributorSpec]
readContributorString s = case P.runParser contributorSpecParser () "" s of
    Left p  -> Left (showParsecErr p)
    Right x -> Right x

parseJannoRemoveEmptyCols :: OP.Parser Bool
parseJannoRemoveEmptyCols = OP.switch (
    OP.long "jannoRemoveEmpty" <>
    OP.help "Reorder the .janno file and remove empty colums. \
            \Remember to pair this option with --checksumJanno to also update the checksum."
    )

parseMaybeLog :: OP.Parser (Maybe String)
parseMaybeLog = OP.option (Just <$> OP.str) (
    OP.long "logText" <>
    OP.metavar "STRING" <>
    OP.help "Log text for this version in the CHANGELOG file." <>
    OP.value Nothing
    )

parseLog :: OP.Parser String
parseLog = OP.strOption (
    OP.long "logText" <>
    OP.metavar "STRING" <>
    OP.help "Log text for this version in the CHANGELOG file." <>
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
        OP.help "Download all packages the server is offering."
        )
    parseEntityInput = (EntitiesFromFile <$> parseFetchEntitiesFromFile) <|> (EntitiesDirect <$> parseFetchEntitiesDirect)


parseIgnorePoseidonVersion :: OP.Parser Bool
parseIgnorePoseidonVersion = OP.switch (
    OP.long "ignorePoseidonVersion" <>
    OP.help "Read packages even if their poseidonVersion is not compatible with trident."
    )

parseForgeEntitiesDirect :: OP.Parser SignedEntitiesList
parseForgeEntitiesDirect = OP.option (OP.eitherReader readSignedEntities) (
    OP.long "forgeString" <>
    OP.short 'f' <>
    OP.metavar "DSL" <>
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
        Left e  -> Left $ renderPoseidonException e
        Right e -> Right e

parseFetchEntitiesDirect :: OP.Parser EntitiesList
parseFetchEntitiesDirect = OP.option (OP.eitherReader readEntities) (
    OP.long "fetchString" <>
    OP.short 'f' <>
    OP.metavar "DSL" <>
    OP.help "List of packages to be downloaded from the remote server. \
        \Package names should be wrapped in asterisks: *package_title*. \
        \You can combine multiple values with comma, so for example: \"*package_1*, *package_2*, *package_3*\". \
        \fetchString uses the same parser as forgeString, but does not allow excludes. If groups or individuals are \
        \specified, then packages which include these groups or individuals are included in the download.")
  where
    readEntities s = case readEntitiesFromString s of
        Left e  -> Left $ renderPoseidonException e
        Right e -> Right e

parseForgeEntitiesFromFile :: OP.Parser FilePath
parseForgeEntitiesFromFile = OP.strOption (
    OP.long "forgeFile" <>
    OP.metavar "FILE" <>
    OP.help "A file with a list of packages, groups or individual samples. \
        \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
        \Empty lines are ignored and comments start with \"#\", so everything after \"#\" is ignored \
        \in one line. \
        \Multiple instances of -f and --forgeFile can be given. They will be evaluated according to their \
        \input order on the command line.")

parseFetchEntitiesFromFile :: OP.Parser FilePath
parseFetchEntitiesFromFile = OP.strOption (
    OP.long "fetchFile" <>
    OP.metavar "FILE" <>
    OP.help "A file with a list of packages. \
        \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
        \-f and --fetchFile can be combined.")

parseIntersect :: OP.Parser Bool
parseIntersect = OP.switch (
    OP.long "intersect" <>
    OP.help "Whether to output the intersection of the genotype files to be forged. \
        \The default (if this option is not set) is to output the union of all SNPs, with genotypes \
        \defined as missing in those packages which do not have a SNP that is present in another package. \
        \With this option set, the forged dataset will typically have fewer SNPs, but less missingness.")

parseRemoteDummy :: OP.Parser ()
parseRemoteDummy = OP.flag' () (
    OP.long "remote" <>
    OP.help "List packages from a remote server instead the local file system.")

parseOutGenotypeFormat :: Bool -> OP.Parser String
parseOutGenotypeFormat withDefault =
  if withDefault
  then OP.strOption settingsWithDefault
  else OP.strOption settingsWithoutDefault
  where
    settingsWithDefault =
        OP.long "outFormat" <>
        OP.metavar "FORMAT" <>
        OP.help "The format of the output genotype data: EIGENSTRAT or PLINK." <>
        OP.value "PLINK" <>
        OP.showDefault
    settingsWithoutDefault =
        OP.long "outFormat" <>
        OP.metavar "FORMAT" <>
        OP.help "the format of the output genotype data: EIGENSTRAT or PLINK."

parseGenoDataSources :: OP.Parser [GenoDataSource]
parseGenoDataSources = OP.some parseGenoDataSource

parseGenoDataSource :: OP.Parser GenoDataSource
parseGenoDataSource = (PacBaseDir <$> parseBasePath) <|> (GenoDirect <$> parseInGenotypeDataset)

parseRepoLocation :: OP.Parser RepoLocationSpec
parseRepoLocation = (RepoLocal <$> parseBasePaths) <|> (parseRemoteDummy *> (RepoRemote <$> parseArchiveEndpoint))

parseArchiveEndpoint :: OP.Parser ArchiveEndpoint
parseArchiveEndpoint = ArchiveEndpoint <$> parseRemoteURL <*> parseMaybeArchiveName

parseValidatePlan :: OP.Parser ValidatePlan
parseValidatePlan =
        (ValPlanBaseDirs <$> parseBasePaths
                         <*> parseIgnoreGeno
                         <*> parseFullGeno
                         <*> parseIgnoreDuplicates
                         <*> parseIgnoreChecksums
                         <*> parseIgnorePoseidonVersion)
    <|> (ValPlanPoseidonYaml <$> parseInPoseidonYamlFile)
    <|> (ValPlanGeno <$> parseInGenoWithoutSNPSet)
    <|> (ValPlanJanno <$> parseInJannoFile)
    <|> (ValPlanSSF <$> parseInSSFile)
    <|> (ValPlanBib <$> parseInBibFile)

parseInPoseidonYamlFile :: OP.Parser FilePath
parseInPoseidonYamlFile = OP.strOption (
    OP.long "pyml" <>
    OP.metavar "FILE" <>
    OP.help "Path to a POSEIDON.yml file.")

parseInJannoFile :: OP.Parser FilePath
parseInJannoFile = OP.strOption (
    OP.long "janno" <>
    OP.metavar "FILE" <>
    OP.help "Path to a .janno file.")

parseInSSFile :: OP.Parser FilePath
parseInSSFile = OP.strOption (
    OP.long "ssf" <>
    OP.metavar "FILE" <>
    OP.help "Path to a .ssf file.")

parseInBibFile :: OP.Parser FilePath
parseInBibFile = OP.strOption (
    OP.long "bib" <>
    OP.metavar "FILE" <>
    OP.help "Path to a .bib file.")

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some parseBasePath

parseBasePath :: OP.Parser FilePath
parseBasePath = OP.strOption (
    OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "A base directory to search for Poseidon packages.")

parseInGenoWithoutSNPSet :: OP.Parser GenotypeDataSpec
parseInGenoWithoutSNPSet = GenotypeDataSpec <$> (parseInGenoOne <|> parseInGenoSep) <*> pure Nothing

parseInGenotypeDataset :: OP.Parser GenotypeDataSpec
parseInGenotypeDataset = GenotypeDataSpec <$> (parseInGenoOne <|> parseInGenoSep) <*> (Just <$> parseGenotypeSNPSet)

parseInGenoOne :: OP.Parser GenotypeFileSpec
parseInGenoOne = OP.option (OP.eitherReader readGenoInput) (
        OP.long "genoOne" <>
        OP.short 'p' <>
        OP.metavar "FILE" <>
        OP.help "One of the input genotype data files. \
                \Expects .bed, .bed.gz, .bim, .bim.gz or .fam for PLINK, \
                \.geno, .geno.gz, .snp, .snp.gz or .ind for EIGENSTRAT, or\
                \.vcf or .vcf.gz for VCF. \
                \In case of EIGENSTRAT and PLINK, the two other files must be in the same directory \
                \and must have the same base name. If a gzipped file is given, it is assumed that the \
                \file pairs (.geno.gz, .snp.gz) or (.bim.gz, .bed.gz) are both zipped, but not the .fam or .ind file. \
                \If a .ind or .fam file is given, it is assumed that none of the file triples is zipped.")
    where
        readGenoInput :: FilePath -> Either String GenotypeFileSpec
        readGenoInput p =
            let (path, extension) = splitExtensionsOptGz p
            in  makeGenoInput path extension
        makeGenoInput path ext
            | ext `elem` [".geno",    ".snp",   ".ind"] =
                Right $ GenotypeEigenstrat (path <.> "geno")    Nothing
                                           (path <.> "snp")     Nothing
                                           (path <.> "ind")     Nothing
            | ext `elem` [".geno.gz", ".snp.gz"       ] =
                Right $ GenotypeEigenstrat (path <.> "geno.gz") Nothing
                                           (path <.> "snp.gz")  Nothing
                                           (path <.> "ind")     Nothing
            | ext `elem` [".bed",     ".bim",   ".fam"] =
                Right $ GenotypePlink      (path <.> "bed")     Nothing
                                           (path <.> "bim")     Nothing
                                           (path <.> "fam")     Nothing
            | ext `elem` [".bed.gz",  ".bim.gz"       ] =
                Right $ GenotypePlink      (path <.> "bed.gz")  Nothing
                                           (path <.> "bim.gz")  Nothing
                                           (path <.> "fam")     Nothing
            | ext `elem` [".vcf", ".vcf.gz"           ] =
                Right $ GenotypeVCF        (path <> ext)         Nothing
            | otherwise = Left $ "unknown file extension: " ++ ext

-- a "smarter" version of `takeExtensions` and `dropExtensions, which splits a filepath at two extensions
-- if the last one is ".gz" but otherwise splits only one.
-- This is important because users may submit files with multiple dots in their name, in which case takeExtensions would return
-- more than we need and the file-ending checks and classifiers would erroneously fail.
splitExtensionsOptGz :: FilePath -> (FilePath, String)
splitExtensionsOptGz fp =
    if takeExtension fp /= ".gz" then -- if the file doesn't end with gz, split at a single ending
        splitExtension fp
    else --if the file ends with ".gz" ...
        let (path, allExtensions) = splitExtensions fp
            extensionsList = drop 1 . splitOn "." $ allExtensions
        in  case extensionsList of
                ["gz"]    -> splitExtension fp -- ... and .gz is the only ending, split there!
                [_, "gz"] -> splitExtensions fp -- ... and there are two endings with gz, use the default splitExtensions function
                _ -> --otherwise split at two endings from the end:
                    let doubleExtension  = ("." ++) . intercalate "." . reverse . take 2 . reverse $ extensionsList
                        extendedPath = path ++ "." ++ (intercalate "." . reverse . drop 2 . reverse $ extensionsList)
                    in (extendedPath, doubleExtension)

parseInGenoSep :: OP.Parser GenotypeFileSpec
parseInGenoSep = parseEigenstrat <|> parsePlink <|> parseVCF
  where
    parseEigenstrat = GenotypeEigenstrat <$>
        parseFileWithEndings "Eigenstrat genotype matrix, optionally gzipped" "genoFile" [".geno", ".geno.gz"] <*>
        pure Nothing <*>
        parseFileWithEndings "Eigenstrat snp positions file, optionally gzipped" "snpFile" [".snp",  ".snp.gz"]  <*>
        pure Nothing <*>
        parseFileWithEndings "Eigenstrat individual file" "indFile" [".ind"] <*>
        pure Nothing
    parsePlink = GenotypePlink <$>
        parseFileWithEndings "Plink genotype matrix, optionally gzipped" "bedFile" [".bed", ".bed.gz"] <*>
        pure Nothing <*>
        parseFileWithEndings "Plink snp positions file, optionally gzipped" "bimFile" [".bim",  ".bim.gz"] <*>
        pure Nothing <*>
        parseFileWithEndings "Plink individual file" "famFile" [".fam"] <*> pure Nothing
    parseVCF = GenotypeVCF <$>
        parseFileWithEndings "VCF (Variant Call Format) file, optionall gzipped" "vcfFile" [".vcf", ".vcf.gz"] <*>
        pure Nothing

parseFileWithEndings :: String -> String -> [String] -> OP.Parser FilePath
parseFileWithEndings help long endings = OP.option (OP.maybeReader fileEndingReader) (
    OP.long long <>
    OP.help (help ++ ". Accepted file endings are " ++ intercalate ", " endings) <>
    OP.metavar "FILE")
  where
    fileEndingReader :: String -> Maybe FilePath
    fileEndingReader p =
        let (_, extension) = splitExtensionsOptGz p
        in if extension `elem` endings then Just p else Nothing

parseGenotypeSNPSet :: OP.Parser SNPSetSpec
parseGenotypeSNPSet = OP.option (OP.eitherReader readSnpSet) (
    OP.long "snpSet" <>
    OP.metavar "SET" <>
    OP.help "The snpSet of the package: 1240K, HumanOrigins or Other. \
            \Only relevant for data input with -p|--genoOne or --genoFile + --snpFile + --indFile, \
            \because the packages in a -d|--baseDir already have this information in their respective \
            \POSEIDON.yml files. (default: Other)" <>
    OP.value SNPSetOther)
  where
    readSnpSet :: String -> Either String SNPSetSpec
    readSnpSet s = case s of
        "1240K"        -> Right SNPSet1240K
        "HumanOrigins" -> Right SNPSetHumanOrigins
        "Other"        -> Right SNPSetOther
        _              -> Left "Could not read snpSet. Must be \"1240K\", \
                                \\"HumanOrigins\" or \"Other\""

parseOutPackagePath :: OP.Parser FilePath
parseOutPackagePath = OP.strOption (
    OP.long "outPackagePath" <>
    OP.short 'o' <>
    OP.metavar "DIR" <>
    OP.help "Path to the output package directory.")

parseMaybeOutPackagePath :: OP.Parser (Maybe FilePath)
parseMaybeOutPackagePath = OP.option (Just <$> OP.str) (
    OP.short 'o' <>
    OP.long "outPackagePath" <>
    OP.metavar "DIR" <>
    OP.help "Path for the converted genotype files to be written to. If a path is provided, only the converted genotype \
        \files are written out, with no change of the original package. If no path is provided, \
        \genotype files will be converted in-place, including a change in the POSEIDON.yml file to yield an updated valid package" <>
    OP.value Nothing <>
    OP.showDefault
    )

parseMaybeOutPackageName :: OP.Parser (Maybe String)
parseMaybeOutPackageName = OP.option (Just <$> OP.str) (
    OP.short 'n' <>
    OP.long "outPackageName" <>
    OP.metavar "STRING" <>
    OP.help "The output package name. This is optional: If no name is provided, \
            \then the package name defaults to the basename of the (mandatory) \
            \--outPackagePath argument." <>
    OP.value Nothing <>
    OP.showDefault
    )

parseForgeOutMode :: OP.Parser ForgeOutMode
parseForgeOutMode =
        parseOutOnlyGenoFlag
    <|> parseMinimalOutputFlag
    <|> parsePreservePymlFlag
    <|> pure NormalOut

parseOutOnlyGenoFlag :: OP.Parser ForgeOutMode
parseOutOnlyGenoFlag = OP.flag' GenoOut onlyGenoOutputDocu
onlyGenoOutputDocu :: OP.Mod OP.FlagFields a
onlyGenoOutputDocu =
    OP.long "onlyGeno" <>
    OP.help "Should only the resulting genotype data be returned? This means the output will not \
            \be a Poseidon package."

parseMinimalOutputFlag :: OP.Parser ForgeOutMode
parseMinimalOutputFlag = OP.flag' MinimalOut minimalOutputDocu
parseMinimalOutputSwitch :: OP.Parser Bool
parseMinimalOutputSwitch = OP.switch minimalOutputDocu
minimalOutputDocu :: OP.Mod OP.FlagFields a
minimalOutputDocu =
    OP.long "minimal" <>
    OP.help "Should the output Poseidon package be reduced to a necessary minimum?"

parsePreservePymlFlag :: OP.Parser ForgeOutMode
parsePreservePymlFlag = OP.flag' PreservePymlOut (
    OP.long "preservePyml" <>
    OP.help "Should the output Poseidon package mimic the input package? \
            \With this option some fields of the source package's POSEIDON.yml file, \
            \its README file and its CHANGELOG file (if available) are copied \
            \to the output package. Only works for a singular source package."
    )

parsePackageWise :: OP.Parser Bool
parsePackageWise = OP.switch (
    OP.long "packagewise" <>
    OP.help "Skip the within-package selection step in forge. This will result in \
            \outputting all individuals in the relevant packages, and hence a superset of the requested \
            \individuals/groups. It may result in better performance in cases where one wants to forge \
            \entire packages or almost entire packages. Details: Forge conceptually performs two types \
            \of selection: First, it identifies which packages in the supplied base directories are \
            \relevant to the requested forge, i.e. whether they are either explicitly listed using \
            \*PackageName*, or because they contain selected individuals or groups. Second, within each \
            \relevant package, individuals which are not requested are removed. This option skips only \
            \the second step, but still performs the first.")

parseMaybeSnpFile :: OP.Parser (Maybe FilePath)
parseMaybeSnpFile = OP.option (Just <$> OP.str) (
    OP.long "selectSnps" <>
    OP.metavar "FILE" <>
    OP.help "To extract specific SNPs during this forge operation, provide a Snp file. \
            \Can be either Eigenstrat (file ending must be '.snp' or '.snp.gz') or Plink (file ending must be '.bim' or '.bim.gz'). \
            \When this option is set, the output package will have exactly the SNPs listed in this file. \
            \Any SNP not listed in the file will be excluded. If option '--intersect' is also set, only \
            \the SNPs overlapping between the SNP file and the forged packages are output." <>
    OP.showDefault <>
    OP.value Nothing)

parseListEntity :: OP.Parser ListEntity
parseListEntity = parseListPackages <|>
                  parseListGroups <|>
                  (parseListIndividualsDummy *> parseListIndividualsExtraCols) <|>
                  (parseListBibliographyDummy *> parseListBibliographyExtraFields)
  where
    parseListPackages = OP.flag' ListPackages (
        OP.long "packages" <>
        OP.help "List all packages."
        )
    parseListGroups = OP.flag' ListGroups (
        OP.long "groups" <>
        OP.help "List all groups, ignoring any group names after the first as specified in the .janno-file.")
    parseListIndividualsDummy = OP.flag' () (
        OP.long "individuals" <>
        OP.help "List all individuals/samples.")
    parseListIndividualsExtraCols = ListIndividuals <$> (parseAllJannoCols <|> (AddColList <$> OP.many parseExtraCol))
    parseAllJannoCols = OP.flag' AddColAll (OP.long "fullJanno" <> OP.help "output all Janno Columns")
    parseExtraCol = OP.strOption (
        OP.short 'j' <>
        OP.long "jannoColumn" <>
        OP.metavar "COLNAME" <>
        OP.help "List additional fields from the janno files, using the .janno column heading name, such as \
        \\"Country\", \"Site\", \"Date_C14_Uncal_BP\", etc... Can be given multiple times")
    parseListBibliographyDummy = OP.flag' () (
        OP.long "bibliography" <> OP.help "output bibliography information for packages")
    parseListBibliographyExtraFields = ListBibliography <$>
        (parseAllBibFields <|> (AddColList <$> OP.many parseExtraBibFields))
    parseAllBibFields = OP.flag' AddColAll (OP.long "fullBib" <> OP.help "output all bibliography fields found in any bibliography item")
    parseExtraBibFields = OP.strOption (
        OP.short 'b' <> OP.long "bibField" <> OP.metavar "BIB-FIELD" <>
        OP.help "List information from the given bibliography field, for example \"abstract\" or \"publisher\". Can \
        \be given multiple times.")

parseRawOutput :: OP.Parser Bool
parseRawOutput = OP.switch (
    OP.long "raw" <>
    OP.help "Return the output table as tab-separated values without header. \
    \This is useful for piping into grep or awk."
    )

parseOnlyLatest :: OP.Parser Bool
parseOnlyLatest = OP.switch (
    OP.long "onlyLatest" <>
    OP.help "Consider only the latest versions of packages, or the groups and individuals \
            \within the latest versions of packages, respectively."
    )

parseIgnoreGeno :: OP.Parser Bool
parseIgnoreGeno = OP.switch (
    OP.long "ignoreGeno" <>
    OP.help "Ignore snp and geno file."
    )

parseFullGeno  :: OP.Parser Bool
parseFullGeno = OP.switch (
    OP.long "fullGeno" <>
    OP.help "Test parsing of all SNPs (by default only the first 100 SNPs are probed)."
    )

parseNoExitCode :: OP.Parser Bool
parseNoExitCode = OP.switch (
    OP.long "noExitCode" <>
    OP.help "Do not produce an explicit exit code."
    )

parseIgnoreDuplicates :: OP.Parser Bool
parseIgnoreDuplicates = OP.switch (
    OP.long "ignoreDuplicates" <>
    OP.help "Do not stop on duplicated individual names in the package collection."
    )

parseRemoteURL :: OP.Parser String
parseRemoteURL = OP.strOption (
    OP.long "remoteURL" <>
    OP.metavar "URL" <>
    OP.help "URL of the remote Poseidon server." <>
    OP.value "https://server.poseidon-adna.org" <>
    OP.showDefault
    )

parseUpgrade :: OP.Parser Bool
parseUpgrade = OP.switch (
    OP.long "upgrade" <>
    OP.short 'u' <>
    OP.help "Overwrite outdated local package versions."
    )

-- PlinkPopNameAsFamily always is the default
parseInputPlinkPopMode :: OP.Parser PlinkPopNameMode
parseInputPlinkPopMode = OP.option (OP.eitherReader readPlinkPopName) (
    OP.long "inPlinkPopName" <>
    OP.metavar "MODE" <>
    OP.help "Where to read the population/group name from the FAM file in Plink-format. \
            \Three options are possible: asFamily (default) | asPhenotype | asBoth." <>
    OP.value PlinkPopNameAsFamily
    )

parseOutputPlinkPopMode :: OP.Parser PlinkPopNameMode
parseOutputPlinkPopMode = OP.option (OP.eitherReader readPlinkPopName) (
    OP.long "outPlinkPopName" <>
    OP.metavar "MODE" <>
    OP.help "Where to write the population/group name into the FAM file in Plink-format. \
            \Three options are possible: asFamily (default) | asPhenotype | asBoth. \
            \See also --inPlinkPopName." <>
    OP.value PlinkPopNameAsFamily)

readPlinkPopName :: String -> Either String PlinkPopNameMode
readPlinkPopName s = case s of
    "asFamily"    -> Right PlinkPopNameAsFamily
    "asPhenotype" -> Right PlinkPopNameAsPhenotype
    "asBoth"      -> Right PlinkPopNameAsBoth
    _             -> Left "must be asFamily, asPhenotype or asBoth"

parseMaybeZipDir :: OP.Parser (Maybe FilePath)
parseMaybeZipDir = OP.option (Just <$> OP.str) (
    OP.long "zipDir" <>
    OP.short 'z' <>
    OP.metavar "DIR" <>
    OP.help "A directory to store .zip files in. If not specified, do not generate .zip files." <>
    OP.value Nothing <>
    OP.showDefault
    )

parsePort :: OP.Parser Int
parsePort = OP.option OP.auto (
    OP.long "port" <>
    OP.short 'p' <>
    OP.metavar "PORT" <>
    OP.help "The port on which the server listens." <>
    OP.value 3000 <>
    OP.showDefault)

parseIgnoreChecksums :: OP.Parser Bool
parseIgnoreChecksums = OP.switch (
    OP.long "ignoreChecksums" <>
    OP.short 'c' <>
    OP.help "Whether to ignore checksums. Useful for speedup in debugging.")

parseMaybeCertFiles :: OP.Parser (Maybe (FilePath, [FilePath], FilePath))
parseMaybeCertFiles = OP.optional parseFiles
  where
    parseFiles = (,,) <$> parseCertFile <*> OP.many parseChainFile <*> parseKeyFile

parseKeyFile :: OP.Parser FilePath
parseKeyFile = OP.strOption (
    OP.long "keyFile" <>
    OP.metavar "FILE" <>
    OP.help "The key file of the TLS Certificate used for HTTPS.")

parseChainFile :: OP.Parser FilePath
parseChainFile = OP.strOption (
    OP.long "chainFile" <>
    OP.metavar "FILE" <>
    OP.help "The chain file of the TLS Certificate used for HTTPS. Can be given multiple times.")

parseCertFile :: OP.Parser FilePath
parseCertFile = OP.strOption (
    OP.long "certFile" <>
    OP.metavar "FILE" <>
    OP.help "The cert file of the TLS Certificate used for HTTPS."
    )

parseArchiveBasePaths :: OP.Parser [(String, FilePath)]
parseArchiveBasePaths = OP.some parseArchiveBasePath
  where
    parseArchiveBasePath :: OP.Parser (String, FilePath)
    parseArchiveBasePath = OP.option (OP.eitherReader parseArchiveNameAndPath) (
        OP.long "baseDir" <>
        OP.short 'd' <>
        OP.metavar "DSL" <>
        OP.help "A base path, prepended by the corresponding archive name under which \
                \packages in this path are being served. Example: arch1=/path/to/basepath. Can \
                \be given multiple times. Multiple paths for the same archive are combined internally. \
                \The very first named archive is considered to be the default archive on the server.")
    parseArchiveNameAndPath :: String -> Either String (String, FilePath)
    parseArchiveNameAndPath str =
        let parts = splitOn "=" str
        in  case parts of
                [name, fp] -> return (name, fp)
                _ -> Left $ "could not parse archive and base directory " ++ str ++ ". Please use format name=path "

parseMaybeArchiveName :: OP.Parser (Maybe String)
parseMaybeArchiveName = OP.option (Just <$> OP.str) (
    OP.long "archive" <>
    OP.metavar "STRING" <>
    OP.help "The name of the Poseidon package archive that should be queried. \
            \If not given, then the query falls back to the default archive of the \
            \server selected with --remoteURL. \
            \See the archive documentation at https://www.poseidon-adna.org/#/archive_overview \
            \for a list of archives currently available from the official Poseidon Web API." <>
    OP.value Nothing <>
    OP.showDefault
    )

parseJannocoalSourceSpec :: OP.Parser JannoSourceSpec
parseJannocoalSourceSpec = parseJannocoalSingleSource <|> (JannoSourceBaseDirs <$> parseBasePaths)
  where
    parseJannocoalSingleSource = OP.option (JannoSourceSingle <$> OP.str) (
        OP.long "sourceFile" <>
        OP.short 's' <>
        OP.metavar "FILE" <>
        OP.help "The source .janno file."
        )

parseJannocoalTargetFile :: OP.Parser FilePath
parseJannocoalTargetFile = OP.strOption (
    OP.long "targetFile" <>
    OP.short 't' <>
    OP.metavar "FILE" <>
    OP.help "The target .janno file to fill."
    )

parseJannocoalOutSpec :: OP.Parser (Maybe FilePath)
parseJannocoalOutSpec = OP.option (Just <$> OP.str) (
    OP.long "outFile" <>
    OP.short 'o' <>
    OP.metavar "FILE" <>
    OP.value Nothing <>
    OP.showDefault <>
    OP.help "An optional file to write the results to. \
            \If not specified, change the target file in place."
    )

parseJannocoalJannoColumns :: OP.Parser CoalesceJannoColumnSpec
parseJannocoalJannoColumns = includeJannoColumns OP.<|> excludeJannoColumns OP.<|> pure AllJannoColumns
    where
        includeJannoColumns = OP.option (IncludeJannoColumns . map BSC.pack . splitOn "," <$> OP.str) (
            OP.long "includeColumns" <>
            OP.help "A comma-separated list of .janno column names to coalesce. \
                    \If not specified, all columns that can be found in the source \
                    \and target will get filled."
            )
        excludeJannoColumns = OP.option (ExcludeJannoColumns . map BSC.pack . splitOn "," <$> OP.str) (
            OP.long "excludeColumns" <>
            OP.help "A comma-separated list of .janno column names NOT to coalesce. \
                    \All columns that can be found in the source and target will get filled, \
                    \except the ones listed here."
            )

parseJannocoalOverride :: OP.Parser Bool
parseJannocoalOverride = OP.switch (
    OP.long "force" <>
    OP.short 'f' <>
    OP.help "With this option, potential non-missing content in target columns gets overridden \
            \with non-missing content in source columns. By default, only missing data gets filled-in."
    )

parseJannocoalSourceKey :: OP.Parser String
parseJannocoalSourceKey = OP.strOption (
    OP.long "sourceKey" <>
    OP.help "The .janno column to use as the source key." <>
    OP.value "Poseidon_ID" <>
    OP.showDefault
    )

parseJannocoalTargetKey :: OP.Parser String
parseJannocoalTargetKey = OP.strOption (
    OP.long "targetKey" <>
    OP.help "The .janno column to use as the target key." <>
    OP.value "Poseidon_ID" <>
    OP.showDefault
    )

parseJannocoalIdStripRegex :: OP.Parser (Maybe String)
parseJannocoalIdStripRegex = OP.option (Just <$> OP.str) (
    OP.long "stripIdRegex" <>
    OP.help "An optional regular expression to identify parts of the IDs to strip \
            \before matching between source and target. Uses POSIX Extended regular expressions." <>
    OP.value Nothing
    )

parseOutputOrdered :: OP.Parser Bool
parseOutputOrdered = OP.switch (
    OP.long "ordered" <>
    OP.help "With this option, the output of forge is ordered according to the entities given."
    )

parseZipOut :: OP.Parser Bool
parseZipOut = OP.switch (
    OP.long "zip" <>
    OP.short 'z' <>
    OP.help "Should the resulting genotype- and snp-files be gzipped?"
    )
