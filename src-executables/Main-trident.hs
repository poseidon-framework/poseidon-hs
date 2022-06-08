{-# LANGUAGE OverloadedStrings #-}

import           Paths_poseidon_hs      (version)
import           Poseidon.GenotypeData  (GenotypeFormatSpec (..), SNPSetSpec(..))
import           Poseidon.CLI.Init      (InitOptions (..), runInit)
import           Poseidon.CLI.List      (ListEntity (..), ListOptions (..),
                                        runList, RepoLocationSpec(..))
import           Poseidon.CLI.Fetch     (FetchOptions (..), runFetch)
import           Poseidon.CLI.Forge     (ForgeOptions (..), runForge)
import           Poseidon.CLI.Genoconvert (GenoconvertOptions (..), runGenoconvert)
import           Poseidon.EntitiesList  (SignedEntitiesList, EntitiesList,
                                        readEntitiesFromString)
import           Poseidon.CLI.Summarise (SummariseOptions(..), runSummarise)
import           Poseidon.CLI.Survey    (SurveyOptions(..), runSurvey)
import           Poseidon.CLI.Update    (runUpdate, UpdateOptions (..))
import           Poseidon.CLI.Validate  (ValidateOptions(..), runValidate)
import           Poseidon.GenotypeData  (GenotypeDataSpec (..)) 
import           Poseidon.Janno         (jannoHeaderString)
import           Poseidon.PoseidonVersion (validPoseidonVersions, showPoseidonVersion)
import           Poseidon.SecondaryTypes (ContributorSpec (..),
                                        VersionComponent (..),
                                        poseidonVersionParser, 
                                        contributorSpecParser,
                                        runParser)
import           Poseidon.Utils         (PoseidonException (..),
                                        renderPoseidonException,
                                        usePoseidonLogger,
                                        LogModus (..), PoseidonLogIO)

import           Colog                  (logError)
import           Control.Applicative    ((<|>))
import           Control.Exception      (catch)
import           Data.List              (intercalate)
import           Data.Version           (Version (..), showVersion)
import qualified Options.Applicative    as OP
import           Options.Applicative.Help.Pretty (string)
import           System.Exit            (exitFailure)
import           System.FilePath        ((<.>), dropExtension, takeExtension)
import           System.IO              (hPutStrLn, stderr)
import qualified Data.Text              as T

data Options = Options { 
    _logModus :: LogModus
  , _subcommand :: Subcommand 
  }

data Subcommand = 
      CmdFstats -- dummy option to provide help message to user
    | CmdInit InitOptions
    | CmdList ListOptions
    | CmdFetch FetchOptions
    | CmdForge ForgeOptions
    | CmdGenoconvert GenoconvertOptions
    | CmdSummarise SummariseOptions
    | CmdSurvey SurveyOptions
    | CmdUpdate UpdateOptions
    | CmdValidate ValidateOptions

main :: IO ()
main = do
    hPutStrLn stderr renderVersion
    hPutStrLn stderr ""
    (Options logModus subcommand) <- OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    catch (usePoseidonLogger logModus $ runCmd logModus subcommand) (handler logModus)
    where
        handler :: LogModus -> PoseidonException -> IO ()
        handler l e = do
            usePoseidonLogger l $ logError $ T.pack $ renderPoseidonException e
            exitFailure

runCmd :: LogModus -> Subcommand -> PoseidonLogIO ()
runCmd l o = case o of
    CmdFstats           -> runFstatsDummy
    CmdInit opts        -> runInit opts
    CmdList opts        -> runList opts
    CmdFetch opts       -> runFetch opts
    CmdForge opts       -> runForge $ opts {_forgeLogModus = l}
    CmdGenoconvert opts -> runGenoconvert opts
    CmdSummarise opts   -> runSummarise opts
    CmdSurvey opts      -> runSurvey opts
    CmdUpdate opts      -> runUpdate opts
    CmdValidate opts    -> runValidate opts
  where
    runFstatsDummy = logError $ T.pack $ fstatsErrorMessage

fstatsErrorMessage :: String
fstatsErrorMessage = "The fstats command has been moved from trident to the analysis tool \
    \xerxes from https://github.com/poseidon-framework/poseidon-analysis-hs"

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> (Options <$> parseLogModus <*> subcommandParser)) (
    OP.briefDesc <>
    OP.progDesc "trident is a management and analysis tool for Poseidon packages. \
                \Report issues here: \
                \https://github.com/poseidon-framework/poseidon-hs/issues"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version number")

parseLogModus :: OP.Parser LogModus
parseLogModus = OP.option (OP.eitherReader readLogModus) (
    OP.long "logModus" <> 
    OP.help "How information should be reported: \
            \NoLog, SimpleLog or TridentDefaultLog" <>
    OP.value TridentDefaultLog <>
    OP.showDefault
    )
    where
        readLogModus :: String -> Either String LogModus
        readLogModus s = case s of
            "NoLog"             -> Right NoLog
            "SimpleLog"         -> Right SimpleLog
            "TridentDefaultLog" -> Right TridentDefaultLog
            _                   -> Left "must be NoLog, SimpleLog or TridentDefaultLog"

renderVersion :: String
renderVersion = 
    "trident v" ++ showVersion version ++ " for poseidon v" ++ 
    intercalate ", v" (map showPoseidonVersion validPoseidonVersions) ++ "\n" ++
    "https://poseidon-framework.github.io" ++ "\n" ++
    ")<(({°> ~ ────E ~ <°}))>(" ++ "\n\n" ++
    "Recent breaking changes:" ++ "\n" ++
    "v0.27.0: The semantics of --forgeString and --forgeFile have been changed. \
    \Removing samples, groups or packages now follows a different logic. Please see the \
    \documentation in trident forge -h to verify that your selection still behaves as you expect."

subcommandParser :: OP.Parser Subcommand
subcommandParser = OP.subparser (
        OP.command "init" initOptInfo <>
        OP.command "fetch" fetchOptInfo <>
        OP.command "forge" forgeOptInfo <>
        OP.command "genoconvert" genoconvertOptInfo <>
        OP.command "update" updateOptInfo <>
        OP.commandGroup "Package creation and manipulation commands:"
    ) <|>
    OP.subparser (
        OP.command "list" listOptInfo <>
        OP.command "summarise" summariseOptInfo <>
        OP.command "summarize" summarizeOptInfo <>
        OP.command "survey" surveyOptInfo <>
        OP.command "validate" validateOptInfo <>
        OP.commandGroup "Inspection commands:"
    ) <|>
    OP.subparser (
        OP.command "fstats" fstatsOptInfo <>
        OP.commandGroup "Former analysis command:"
    )
  where
    fstatsOptInfo = OP.info (pure CmdFstats) (OP.progDesc fstatsErrorMessage) -- dummy for now
    initOptInfo = OP.info (OP.helper <*> (CmdInit <$> initOptParser))
        (OP.progDesc "Create a new Poseidon package from genotype data")
    listOptInfo = OP.info (OP.helper <*> (CmdList <$> listOptParser))
        (OP.progDesc "List packages, groups or individuals from local or remote Poseidon repositories")
    fetchOptInfo = OP.info (OP.helper <*> (CmdFetch <$> fetchOptParser))
        (OP.progDesc "Download data from a remote Poseidon repository")
    forgeOptInfo = OP.info (OP.helper <*> (CmdForge <$> forgeOptParser))
        (OP.progDesc "Select packages, groups or individuals and create a new Poseidon package from them")
    genoconvertOptInfo = OP.info (OP.helper <*> (CmdGenoconvert <$> genoconvertOptParser))
        (OP.progDesc "Convert the genotype data in a Poseidon package to a different file format")
    summariseOptInfo = OP.info (OP.helper <*> (CmdSummarise <$> summariseOptParser))
        (OP.progDesc "Get an overview over the content of one or multiple Poseidon packages")
    summarizeOptInfo = OP.info (OP.helper <*> (CmdSummarise <$> summariseOptParser))
        (OP.progDesc "Synonym for summarise")
    surveyOptInfo = OP.info (OP.helper <*> (CmdSurvey <$> surveyOptParser))
        (OP.progDesc "Survey the degree of context information completeness for Poseidon packages" <>
        OP.footerDoc (Just $ string $
               "Output structure\n"
            ++ "Data coverage proportions - 0: ., <0.25: ░, <0.5: ▒, <1: ▓, 1: █\n"
            ++ ".janno column order - G: Genotype data present, B: Bibliography file present, "
            ++ intercalate ", " (zipWith (\x y -> show x ++ ": " ++ y) ([1..] :: [Int]) jannoHeaderString)
            ))
    updateOptInfo = OP.info (OP.helper <*> (CmdUpdate <$> updateOptParser))
        (OP.progDesc "Update POSEIDON.yml files automatically")
    validateOptInfo = OP.info (OP.helper <*> (CmdValidate <$> validateOptParser))
        (OP.progDesc "Check one or multiple Poseidon packages for structural correctness")

initOptParser :: OP.Parser InitOptions
initOptParser = InitOptions <$> parseInGenotypeDataset
                            <*> parseOutPackagePath
                            <*> parseMaybeOutPackageName
                            <*> parseMakeMinimalPackage

listOptParser :: OP.Parser ListOptions
listOptParser = ListOptions <$> parseRepoLocation
                            <*> parseListEntity
                            <*> parseRawOutput
                            <*> parseIgnoreGeno

fetchOptParser :: OP.Parser FetchOptions
fetchOptParser = FetchOptions <$> parseBasePaths
                              <*> parseFetchEntitiesDirect
                              <*> parseFetchEntitiesFromFile
                              <*> parseRemoteURL
                              <*> parseUpgrade
                              <*> parseDownloadAll

forgeOptParser :: OP.Parser ForgeOptions
forgeOptParser = ForgeOptions <$> parseBasePaths
                              <*> parseInGenotypeDatasets
                              <*> parseForgeEntitySpec
                              <*> parseMaybeSnpFile
                              <*> parseIntersect
                              <*> parseOutGenotypeFormat True
                              <*> parseMakeMinimalPackage
                              <*> parseOutOnlyGeno
                              <*> parseOutPackagePath
                              <*> parseMaybeOutPackageName
                              <*> pure NoLog
                              <*> parseNoExtract

genoconvertOptParser :: OP.Parser GenoconvertOptions
genoconvertOptParser = GenoconvertOptions <$> parseBasePaths
                                          <*> parseInGenotypeDatasets
                                          <*> parseOutGenotypeFormat False
                                          <*> parseOutOnlyGeno
                                          <*> parseMaybeOutPackagePath
                                          <*> parseRemoveOld

parseRemoveOld :: OP.Parser Bool
parseRemoveOld = OP.switch (OP.long "removeOld" <> OP.help "Remove the old genotype files when creating the new ones")

summariseOptParser :: OP.Parser SummariseOptions
summariseOptParser = SummariseOptions <$> parseBasePaths
                                      <*> parseRawOutput

surveyOptParser :: OP.Parser SurveyOptions
surveyOptParser = SurveyOptions <$> parseBasePaths
                                <*> parseRawOutput

updateOptParser :: OP.Parser UpdateOptions
updateOptParser = UpdateOptions <$> parseBasePaths
                                <*> parsePoseidonVersion
                                <*> parseIgnorePoseidonVersion
                                <*> parseVersionComponent
                                <*> parseNoChecksumUpdate
                                <*> parseIgnoreGeno
                                <*> parseContributors
                                <*> parseLog
                                <*> parseForce

validateOptParser :: OP.Parser ValidateOptions
validateOptParser = ValidateOptions <$> parseBasePaths
                                    <*> parseVerbose
                                    <*> parseIgnoreGeno
                                    <*> parseNoExitCode

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

parseForgeEntitySpec :: OP.Parser (Either SignedEntitiesList FilePath)
parseForgeEntitySpec = (Right <$> parseForgeEntitiesFromFile) <|> (Left <$> parseForgeEntitiesDirect)

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
        \will assume you want to merge all individuals in the \
        \packages found in the baseDirs (except the ones explicitly excluded) before the exclude entities are applied. \
        \An empty forgeString will therefore merge all available individuals." <> OP.value [])
  where
    readSignedEntities s = case readEntitiesFromString s of
        Left e -> Left (show e)
        Right e -> Right e

parseFetchEntitiesDirect :: OP.Parser EntitiesList
parseFetchEntitiesDirect = concat <$> OP.many (OP.option (OP.eitherReader readEntities) (OP.long "fetchString" <>
    OP.short 'f' <>
    OP.help "List of packages to be downloaded from the remote server. \
        \Package names should be wrapped in asterisks: *package_title*. \
        \You can combine multiple values with comma, so for example: \"*package_1*, *package_2*, *package_3*\". \
        \fetchString uses the same parser as forgeString, but does not allow excludes. If groups or individuals are \
        \specified, then packages which include these groups or individuals are included in the download."))
  where
    readEntities s = case readEntitiesFromString s of
        Left e -> Left (show e)
        Right e -> Right e

parseForgeEntitiesFromFile :: OP.Parser FilePath
parseForgeEntitiesFromFile = OP.strOption (OP.long "forgeFile" <>
    OP.help "A file with a list of packages, groups or individual samples. \
        \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
        \Empty lines are ignored and comments start with \"#\", so everything after \"#\" is ignored \
        \in one line.")

parseFetchEntitiesFromFile :: OP.Parser [FilePath]
parseFetchEntitiesFromFile = OP.many (OP.strOption (OP.long "fetchFile" <>
    OP.help "A file with a list of packages. \
        \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
        \-f and --fetchFile can be combined."))

parseIntersect :: OP.Parser (Bool)
parseIntersect = OP.switch (OP.long "intersect" <>
    OP.help "Whether to output the intersection of the genotype files to be forged. \
        \The default (if this option is not set) is to output the union of all SNPs, with genotypes \
        \defined as missing in those packages which do not have a SNP that is present in another package. \
        \With this option set, the forged dataset will typically have fewer SNPs, but less missingness.")

parseRepoLocation :: OP.Parser RepoLocationSpec
parseRepoLocation = (RepoLocal <$> parseBasePaths) <|> (parseRemoteDummy *> (RepoRemote <$> parseRemoteURL))

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.many (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages (could be a Poseidon repository)"))

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

parseInGenotypeDatasets :: OP.Parser [GenotypeDataSpec]
parseInGenotypeDatasets = OP.many parseInGenotypeDataset

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
                Right (GenotypeFormatEigenstrat,(path <.> "geno"),(path <.> "snp"),(path <.> "ind"))
            | ext `elem` [".bed", ".bim", ".fam"]  =
                Right (GenotypeFormatPlink,     (path <.> "bed"), (path <.> "bim"),(path <.> "fam"))
            | otherwise = Left $ "unknown file extension: " ++ ext

parseInGenoSep :: OP.Parser GenoInput
parseInGenoSep = (,,,) <$> parseInGenotypeFormat <*> parseInGenoFile <*> parseInSnpFile <*> parseInIndFile

parseInGenotypeFormat :: OP.Parser GenotypeFormatSpec
parseInGenotypeFormat = OP.option (OP.eitherReader readGenotypeFormat) (
    OP.short 'r' <> OP.long "inFormat" <>
    OP.help "the format of the input genotype data: EIGENSTRAT or PLINK")
  where
    readGenotypeFormat :: String -> Either String GenotypeFormatSpec
    readGenotypeFormat s = case s of
        "EIGENSTRAT" -> Right GenotypeFormatEigenstrat
        "PLINK"      -> Right GenotypeFormatPlink
        _            -> Left "must be EIGENSTRAT or PLINK"

parseInGenoFile :: OP.Parser FilePath
parseInGenoFile = OP.strOption (
    OP.short 'g' <> OP.long "genoFile" <>
    OP.help "the input geno file path")

parseInSnpFile :: OP.Parser FilePath
parseInSnpFile = OP.strOption (
    OP.short 's' <> OP.long "snpFile" <>
    OP.help "the input snp file path")

parseInIndFile :: OP.Parser FilePath
parseInIndFile = OP.strOption (
    OP.short 'i' <> OP.long "indFile" <>
    OP.help "the input ind file path")

parseGenotypeSNPSet :: OP.Parser (Maybe SNPSetSpec)
parseGenotypeSNPSet = OP.option (Just <$> OP.eitherReader readSnpSet) (OP.long "snpSet" <>
    OP.help "the snpSet of the new package: 1240K, HumanOrigins or Other. Default: Other" <>
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

parseNoExtract :: OP.Parser Bool
parseNoExtract = OP.switch (OP.long "no-extract" <> OP.help "Skip the selection step in forge. This will result in \
    \outputting all individuals in the relevant packages, and hence a superset of the requested \
    \individuals/groups. It may result in better performance in cases where one wants to forge entire packages or \
    \almost entire packages. \
    \Note that this will also ignore any ordering in the output groups/individuals. With this option active, \
    \individuals from the relevant packages will just be written in the order that they appear in the original packages.")

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

parseVerbose :: OP.Parser Bool
parseVerbose = OP.switch (
    OP.long "verbose" <>
    OP.help "print more output to the command line"
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

parseDownloadAll :: OP.Parser Bool
parseDownloadAll = OP.switch (
    OP.long "downloadAll" <>
    OP.help "download all packages the server is offering"
    )