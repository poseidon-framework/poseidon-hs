{-# LANGUAGE OverloadedStrings #-}

import           Paths_poseidon_hs      (version)
import           Poseidon.CLI.FStats    (FStatSpec (..), FstatsOptions (..),
                                        JackknifeMode (..), fStatSpecParser,
                                        runFstats, runParser)
import           Poseidon.GenotypeData  (GenotypeFormatSpec (..), SNPSetSpec(..))
import           Poseidon.CLI.Init      (InitOptions (..), runInit)
import           Poseidon.CLI.List      (ListEntity (..), ListOptions (..),
                                        runList, RepoLocationSpec(..))
import           Poseidon.CLI.Fetch     (FetchOptions (..), runFetch)
import           Poseidon.CLI.Forge     (ForgeOptions (..), runForge)
import           Poseidon.CLI.Genoconvert (GenoconvertOptions (..), runGenoconvert)
import           Poseidon.EntitiesList  (PoseidonEntity (..),
                                        poseidonEntitiesParser)
import           Poseidon.CLI.Summarise (SummariseOptions(..), runSummarise)
import           Poseidon.CLI.Survey    (SurveyOptions(..), runSurvey)
import           Poseidon.CLI.Checksumupdate (runChecksumupdate, ChecksumupdateOptions (..))
import           Poseidon.CLI.Validate  (ValidateOptions(..), runValidate)
import           Poseidon.Utils         (PoseidonException (..), 
                                        renderPoseidonException)

import           Control.Applicative    ((<|>))
import           Control.Exception      (catch)
import           Data.ByteString.Char8  (pack, splitWith)
import           Data.Version           (showVersion)
import qualified Options.Applicative    as OP
import           SequenceFormats.Utils  (Chrom (..))
import           System.Exit            (exitFailure)
import           System.IO              (hPutStrLn, stderr)
import           Text.Read              (readEither)

data Options = CmdFstats FstatsOptions
    | CmdInit InitOptions
    | CmdList ListOptions
    | CmdFetch FetchOptions
    | CmdForge ForgeOptions
    | CmdGenoconvert GenoconvertOptions
    | CmdSummarise SummariseOptions
    | CmdSurvey SurveyOptions
    | CmdChecksumupdate ChecksumupdateOptions
    | CmdValidate ValidateOptions

main :: IO ()
main = do
    cmdOpts <- OP.customExecParser p optParserInfo
    catch (runCmd cmdOpts) handler
    where
        p = OP.prefs OP.showHelpOnEmpty
        handler :: PoseidonException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderPoseidonException e
            exitFailure

runCmd :: Options -> IO ()
runCmd o = case o of
    CmdFstats opts    -> runFstats opts
    CmdInit opts      -> runInit opts
    CmdList opts      -> runList opts
    CmdFetch opts     -> runFetch opts
    CmdForge opts     -> runForge opts
    CmdGenoconvert opts -> runGenoconvert opts
    CmdSummarise opts -> runSummarise opts
    CmdSurvey opts    -> runSurvey opts
    CmdChecksumupdate opts -> runChecksumupdate opts
    CmdValidate opts  -> runValidate opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "trident is a management and analysis tool for Poseidon packages. \
                \More information: \
                \https://poseidon-framework.github.io. \
                \Report issues: \
                \https://github.com/poseidon-framework/poseidon-hs/issues"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = OP.subparser (
        OP.command "checksumupdate" checksumupdateOptInfo <>
        OP.command "init" initOptInfo <>
        OP.command "fetch" fetchOptInfo <>
        OP.command "forge" forgeOptInfo <>
        OP.command "genoconvert" genoconvertOptInfo <>
        OP.commandGroup "Package creation and manipulation commands:"
    ) <|>
    OP.subparser (
        OP.command "list" listOptInfo <>
        OP.command "summarise" summariseOptInfo <>
        OP.command "survey" surveyOptInfo <>
        OP.command "validate" validateOptInfo <>
        OP.commandGroup "Inspection commands:"
    ) <|>
    OP.subparser (
        OP.command "fstats" fstatsOptInfo <>
        OP.commandGroup "Analysis commands:"
    )
  where
    fstatsOptInfo = OP.info (OP.helper <*> (CmdFstats <$> fstatsOptParser))
        (OP.progDesc "Run fstats on groups and invidiuals within and across Poseidon packages")
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
    surveyOptInfo = OP.info (OP.helper <*> (CmdSurvey <$> surveyOptParser))
        (OP.progDesc "Survey the degree of context information completeness for Poseidon packages")
    checksumupdateOptInfo = OP.info (OP.helper <*> (CmdChecksumupdate <$> checksumupdateOptParser))
        (OP.progDesc "Update checksums in POSEIDON.yml files")
    validateOptInfo = OP.info (OP.helper <*> (CmdValidate <$> validateOptParser))
        (OP.progDesc "Check one or multiple Poseidon packages for structural correctness")

fstatsOptParser :: OP.Parser FstatsOptions
fstatsOptParser = FstatsOptions <$> parseBasePaths
                                <*> parseJackknife
                                <*> parseExcludeChroms
                                <*> OP.many parseStatSpecsDirect
                                <*> parseStatSpecsFromFile
                                <*> parseRawOutput

initOptParser :: OP.Parser InitOptions
initOptParser = InitOptions <$> parseInGenotypeFormat
                            <*> parseGenotypeSNPSet
                            <*> parseInGenoFile
                            <*> parseInSnpFile
                            <*> parseInIndFile
                            <*> parseOutPackagePath
                            <*> parseOutPackageName

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
                              <*> parseForgeEntitiesDirect
                              <*> parseForgeEntitiesFromFile
                              <*> parseIntersect
                              <*> parseOutPackagePath
                              <*> parseOutPackageName
                              <*> parseOutFormat
                              <*> parseShowWarnings

genoconvertOptParser :: OP.Parser GenoconvertOptions
genoconvertOptParser = GenoconvertOptions <$> parseBasePaths
                                          <*> parseOutGenotypeFormat
                                          <*> parseRemoveOld

parseRemoveOld :: OP.Parser Bool
parseRemoveOld = OP.switch (OP.long "removeOld" <> OP.help "Remove the old genotype files when creating the new ones")

summariseOptParser :: OP.Parser SummariseOptions
summariseOptParser = SummariseOptions <$> parseBasePaths
                                      <*> parseRawOutput

surveyOptParser :: OP.Parser SurveyOptions
surveyOptParser = SurveyOptions <$> parseBasePaths
                                <*> parseRawOutput

checksumupdateOptParser :: OP.Parser ChecksumupdateOptions
checksumupdateOptParser = ChecksumupdateOptions <$> parseBasePaths

validateOptParser :: OP.Parser ValidateOptions
validateOptParser = ValidateOptions <$> parseBasePaths
                                    <*> parseVerbose
                                    <*> parseIgnoreGeno

parseJackknife :: OP.Parser JackknifeMode
parseJackknife = OP.option (OP.eitherReader readJackknifeString) (OP.long "jackknife" <> OP.short 'j' <>
    OP.help "Jackknife setting. If given an integer number, this defines the block size in SNPs. \
        \Set to \"CHR\" if you want jackknife blocks defined as entire chromosomes. The default is at 5000 SNPs" <> OP.value (JackknifePerN 5000))
  where
    readJackknifeString :: String -> Either String JackknifeMode
    readJackknifeString s = case s of
        "CHR"  -> Right JackknifePerChromosome
        numStr -> let num = readEither numStr
                  in  case num of
                        Left e  -> Left e
                        Right n -> Right (JackknifePerN n)

parseExcludeChroms :: OP.Parser [Chrom]
parseExcludeChroms = OP.option (map Chrom . splitWith (==',') . pack <$> OP.str) (OP.long "excludeChroms" <> OP.short 'e' <>
    OP.help "List of chromosome names to exclude chromosomes, given as comma-separated \
        \list. Defaults to X, Y, MT, chrX, chrY, chrMT, 23,24,90" <> OP.value [Chrom "X", Chrom "Y", Chrom "MT",
        Chrom "chrX", Chrom "chrY", Chrom "chrMT", Chrom "23", Chrom "24", Chrom "90"])

parseStatSpecsDirect :: OP.Parser FStatSpec
parseStatSpecsDirect = OP.option (OP.eitherReader readStatSpecString) (OP.long "stat" <>
    OP.help "Specify a summary statistic to be computed. Can be given multiple times. \
        \Possible options are: F4(name1, name2, name3, name4), and similarly F3 and F2 stats, \
        \as well as PWM(name1,name2) for pairwise mismatch rates. Group names are by default \
        \matched with group names as indicated in the PLINK or Eigenstrat files in the Poseidon dataset. \
        \You can also specify individual names using the syntax \"<Ind_name>\", so enclosing them \
        \in angular brackets. You can also mix groups and individuals, like in \
        \\"F4(<Ind1>,Group2,Group3,<Ind4>)\". Group or individual names are separated by commas, and a comma \
        \can be followed by any number of spaces, as in some of the examples in this help text.")

parseStatSpecsFromFile :: OP.Parser (Maybe FilePath)
parseStatSpecsFromFile = OP.option (Just <$> OP.str) (OP.long "statFile" <> OP.help "Specify a file with F-Statistics specified \
    \similarly as specified for option --stat. One line per statistics, and no new-line at the end" <> OP.value Nothing)

readStatSpecString :: String -> Either String FStatSpec
readStatSpecString s = case runParser fStatSpecParser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

parseForgeEntitiesDirect :: OP.Parser [PoseidonEntity]
parseForgeEntitiesDirect = concat <$> OP.many (OP.option (OP.eitherReader readPoseidonEntitiesString) (OP.long "forgeString" <>
    OP.short 'f' <>
    OP.help "List of packages, groups or individual samples to be combined in the output package. \
        \Packages follow the syntax *package_title*, populations/groups are simply group_id and individuals \
        \<individual_id>. You can combine multiple values with comma, so for example: \
        \\"*package_1*, <individual_1>, <individual_2>, group_1\""))

parseFetchEntitiesDirect :: OP.Parser [PoseidonEntity]
parseFetchEntitiesDirect = concat <$> OP.many (OP.option (OP.eitherReader readPoseidonEntitiesString) (OP.long "fetchString" <>
    OP.short 'f' <>
    OP.help "List of packages to be downloaded from the remote server. \
        \Package names should be wrapped in asterisks: *package_title*. You can combine multiple values with comma, so for example: \
        \\"*package_1*, *package_2*, *package_3*\""))

parseForgeEntitiesFromFile :: OP.Parser [FilePath]
parseForgeEntitiesFromFile = OP.many (OP.strOption (OP.long "forgeFile" <>
    OP.help "A file with a list of packages, groups or individual samples. \
    \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
    \-f and --forgeFile can be combined."))

parseFetchEntitiesFromFile :: OP.Parser [FilePath]
parseFetchEntitiesFromFile = OP.many (OP.strOption (OP.long "fetchFile" <>
    OP.help "A file with a list of packages. \
    \Works just as -f, but multiple values can also be separated by newline, not just by comma. \
    \-f and --fetchFile can be combined."))

readPoseidonEntitiesString :: String -> Either String [PoseidonEntity]
readPoseidonEntitiesString s = case runParser poseidonEntitiesParser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

parseIntersect :: OP.Parser (Bool)
parseIntersect = OP.switch (OP.long "intersect" <>
    OP.help "Whether to output the intersection of the genotype files to be forged. \
        \The default (if this option is not set) is to output the union of all SNPs, with genotypes \
        \defined as missing in those packages which do not have a SNP that is present in another package. \
        \With this option set, the forged dataset will typically have fewer SNPs, but less missingness.")

parseRepoLocation :: OP.Parser RepoLocationSpec
parseRepoLocation = (RepoLocal <$> parseBasePaths) <|> (parseRemoteDummy *> (RepoRemote <$> parseRemoteURL))

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages (could be a Poseidon repository)"))

parseRemoteDummy :: OP.Parser ()
parseRemoteDummy = OP.flag' () (OP.long "remote" <> OP.help "list packages from a remote server instead the local file system")

parseInGenotypeFormat :: OP.Parser GenotypeFormatSpec
parseInGenotypeFormat = OP.option (OP.eitherReader readGenotypeFormat) (OP.long "inFormat" <>
    OP.help "the format of the input genotype data: EIGENSTRAT or PLINK") 
  where
    readGenotypeFormat :: String -> Either String GenotypeFormatSpec
    readGenotypeFormat s = case s of
        "EIGENSTRAT" -> Right GenotypeFormatEigenstrat
        "PLINK"      -> Right GenotypeFormatPlink
        _            -> Left "must be EIGENSTRAT or PLINK"

parseGenotypeSNPSet :: OP.Parser SNPSetSpec
parseGenotypeSNPSet = OP.option (OP.eitherReader readSnpSet) (OP.long "snpSet" <>
    OP.help "the snpSet of the new package: 1240K, HumanOrigins or Other")
  where
    readSnpSet :: String -> Either String SNPSetSpec
    readSnpSet s = case s of
        "1240K"        -> Right SNPSet1240K
        "HumanOrigins" -> Right SNPSetHumanOrigins
        "Other"        -> Right SNPSetOther
        _              -> Left "Could not read snpSet. Must be \"1240K\", \
                                \\"HumanOrigins\" or \"Other\""


parseOutGenotypeFormat :: OP.Parser GenotypeFormatSpec
parseOutGenotypeFormat = OP.option (OP.eitherReader readGenotypeFormat) (OP.long "outFormat" <>
    OP.help "the format of the output genotype data: EIGENSTRAT or PLINK") 
  where
    readGenotypeFormat :: String -> Either String GenotypeFormatSpec
    readGenotypeFormat s = case s of
        "EIGENSTRAT" -> Right GenotypeFormatEigenstrat
        "PLINK"      -> Right GenotypeFormatPlink
        _            -> Left "must be EIGENSTRAT or PLINK"

parseInGenoFile :: OP.Parser FilePath
parseInGenoFile = OP.strOption (OP.long "genoFile" <>
    OP.help "the input geno file path")

parseInSnpFile :: OP.Parser FilePath
parseInSnpFile = OP.strOption (OP.long "snpFile" <>
    OP.help "the input snp file path")

parseInIndFile :: OP.Parser FilePath
parseInIndFile = OP.strOption (OP.long "indFile" <>
    OP.help "the input ind file path")

parseOutPackagePath :: OP.Parser FilePath
parseOutPackagePath = OP.strOption (OP.long "outPackagePath" <>
    OP.short 'o' <>
    OP.help "the output package directory path")

parseOutPackageName :: OP.Parser FilePath
parseOutPackageName = OP.strOption (OP.long "outPackageName" <>
    OP.short 'n' <>
    OP.help "the output package name")

parseOutFormat :: OP.Parser GenotypeFormatSpec
parseOutFormat = parseEigenstratFormat <|> pure GenotypeFormatPlink
  where
    parseEigenstratFormat = OP.flag' GenotypeFormatEigenstrat (OP.long "eigenstrat" <>
        OP.help "Choose Eigenstrat instead of PLINK (default) as output format.")

parseShowWarnings :: OP.Parser Bool
parseShowWarnings = OP.switch (OP.long "warnings" <> OP.short 'w' <> OP.help "Show all warnings for merging genotype data")

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
    OP.help "ignore SNP and GenoFile for the validation" <>
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