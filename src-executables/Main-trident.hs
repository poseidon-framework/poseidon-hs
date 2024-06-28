{-# LANGUAGE OverloadedStrings #-}

import           Paths_poseidon_hs                       (version)
import           Poseidon.CLI.Chronicle                  (ChronicleOptions (..),
                                                          runChronicle)
import           Poseidon.CLI.Fetch                      (FetchOptions (..),
                                                          runFetch)
import           Poseidon.CLI.Forge                      (ForgeOptions (..),
                                                          runForge)
import           Poseidon.CLI.Genoconvert                (GenoconvertOptions (..),
                                                          runGenoconvert)
import           Poseidon.CLI.Init                       (InitOptions (..),
                                                          runInit)
import           Poseidon.CLI.Jannocoalesce              (JannoCoalesceOptions (..),
                                                          runJannocoalesce)
import           Poseidon.CLI.List                       (ListOptions (..),
                                                          runList)
import           Poseidon.CLI.OptparseApplicativeParsers
import           Poseidon.CLI.Rectify                    (RectifyOptions (..),
                                                          runRectify)
import           Poseidon.CLI.Serve                      (ServeOptions (..),
                                                          runServerMainThread)
import           Poseidon.CLI.Summarise                  (SummariseOptions (..),
                                                          runSummarise)
import           Poseidon.CLI.Survey                     (SurveyOptions (..),
                                                          runSurvey)
import           Poseidon.CLI.Timetravel                 (TimetravelOptions (..),
                                                          runTimetravel)
import           Poseidon.CLI.Validate                   (ValidateOptions (..),
                                                          runValidate)
import           Poseidon.Janno                          (jannoHeaderString)
import           Poseidon.PoseidonVersion                (showPoseidonVersion,
                                                          validPoseidonVersions)
import           Poseidon.Utils                          (ErrorLength (..),
                                                          LogMode (..),
                                                          PlinkPopNameMode (..),
                                                          PoseidonException (..),
                                                          PoseidonIO, TestMode,
                                                          logError,
                                                          renderPoseidonException,
                                                          usePoseidonLogger)


import           Control.Applicative                     ((<|>))
import           Control.Exception                       (catch)
import           Data.List                               (intercalate)
import           Data.Version                            (showVersion)
import qualified Options.Applicative                     as OP
import           Options.Applicative.Help                (pretty)
import           System.Exit                             (exitFailure)
import           System.IO                               (hPutStrLn, stderr)

data Options = Options {
    _logMode    :: LogMode
  , _testMode   :: TestMode
  , _errLength  :: ErrorLength
  , _plinkMode  :: PlinkPopNameMode
  , _subcommand :: Subcommand
  }

data Subcommand =
      CmdInit InitOptions
    | CmdList ListOptions
    | CmdFetch FetchOptions
    | CmdForge ForgeOptions
    | CmdGenoconvert GenoconvertOptions
    | CmdSummarise SummariseOptions
    | CmdSurvey SurveyOptions
    | CmdRectify RectifyOptions
    | CmdValidate ValidateOptions
    | CmdChronicle ChronicleOptions
    | CmdTimetravel TimetravelOptions
    | CmdServe ServeOptions
    | CmdJannoCoalesce JannoCoalesceOptions

main :: IO ()
main = do
    hPutStrLn stderr renderVersion
    hPutStrLn stderr ""
    (Options logMode testMode errLength plinkMode subcommand) <- OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    catch (usePoseidonLogger logMode testMode plinkMode errLength $ runCmd subcommand) (handler logMode testMode plinkMode errLength)
    where
        handler :: LogMode -> TestMode -> PlinkPopNameMode -> ErrorLength -> PoseidonException -> IO ()
        handler l t pm len e = do
            usePoseidonLogger l t pm len $ logError $ renderPoseidonException e
            exitFailure

runCmd :: Subcommand -> PoseidonIO ()
runCmd o = case o of
    -- alphabetic order
    CmdChronicle     opts -> runChronicle opts
    CmdFetch         opts -> runFetch opts
    CmdForge         opts -> runForge opts
    CmdGenoconvert   opts -> runGenoconvert opts
    CmdJannoCoalesce opts -> runJannocoalesce opts
    CmdInit          opts -> runInit opts
    CmdList          opts -> runList opts
    CmdRectify       opts -> runRectify opts
    CmdServe         opts -> runServerMainThread opts
    CmdSummarise     opts -> runSummarise opts
    CmdSurvey        opts -> runSurvey opts
    CmdTimetravel    opts -> runTimetravel opts
    CmdValidate      opts -> runValidate opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (
    OP.helper <*> versionOption <*>
        (Options <$> (parseLogMode <|> parseDebugMode)
                 <*> parseTestMode
                 <*> parseErrorLength
                 <*> parseInputPlinkPopMode
                 <*> subcommandParser)
        ) (
    OP.briefDesc <>
    OP.progDesc "trident is a management and analysis tool for Poseidon packages. \
                \Report issues here: \
                \https://github.com/poseidon-framework/poseidon-hs/issues"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version number")

renderVersion :: String
renderVersion =
    "trident v" ++ showVersion version ++ " for poseidon v" ++
    intercalate ", v" (map showPoseidonVersion validPoseidonVersions) ++ "\n" ++
    "https://poseidon-framework.github.io"

subcommandParser :: OP.Parser Subcommand
subcommandParser = OP.subparser (
        OP.command "init" initOptInfo <>
        OP.command "fetch" fetchOptInfo <>
        OP.command "forge" forgeOptInfo <>
        OP.command "genoconvert" genoconvertOptInfo <>
        OP.command "jannocoalesce" jannocoalesceOptInfo <>
        OP.command "rectify" rectifyOptInfo <>
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
        OP.command "chronicle" chronicleOptInfo <>
        OP.command "timetravel" timetravelOptInfo <>
        OP.command "serve" serveOptInfo <>
        OP.command "summarize" summarizeOptInfo <>
        OP.commandGroup "Hidden commands:" <>
        OP.internal
    )
  where
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
        OP.footerDoc (Just $ pretty $
               "Output structure\n"
            ++ "Data coverage proportions - 0: ., <0.25: ░, <0.5: ▒, <1: ▓, 1: █\n"
            ++ ".janno column order - G: Genotype data present, S: .ssf file present, B: .bib file present, "
            ++ intercalate ", " (zipWith (\x y -> show x ++ ": " ++ y) ([1..] :: [Int]) jannoHeaderString)
            ))
    rectifyOptInfo = OP.info (OP.helper <*> (CmdRectify <$> rectifyOptParser))
        (OP.progDesc "Adjust POSEIDON.yml files automatically to package changes")
    validateOptInfo = OP.info (OP.helper <*> (CmdValidate <$> validateOptParser))
        (OP.progDesc "Check Poseidon packages or package components for structural correctness")
    chronicleOptInfo = OP.info (OP.helper <*> (CmdChronicle <$> chronicleOptParser))
        (OP.progDesc "Create chronicle files for package collections")
    timetravelOptInfo = OP.info (OP.helper <*> (CmdTimetravel <$> timetravelOptParser))
        (OP.progDesc "Construct package directories from chronicle files")
    serveOptInfo    = OP.info (OP.helper <*> (CmdServe <$> serveOptParser))
        (OP.progDesc "Serve Poseidon packages via HTTP or HTTPS")
    jannocoalesceOptInfo = OP.info (OP.helper <*> (CmdJannoCoalesce <$> jannocoalesceOptParser))
        (OP.progDesc "Coalesce information from one or multiple janno files to another one")

initOptParser :: OP.Parser InitOptions
initOptParser = InitOptions <$> parseInGenotypeDataset
                            <*> parseOutPackagePath
                            <*> parseMaybeOutPackageName
                            <*> parseMinimalOutput

listOptParser :: OP.Parser ListOptions
listOptParser = ListOptions <$> parseRepoLocation
                            <*> parseListEntity
                            <*> parseRawOutput
                            <*> parseOnlyLatest

fetchOptParser :: OP.Parser FetchOptions
fetchOptParser = FetchOptions <$> parseBasePaths
                              <*> parseFetchEntityInputs
                              <*> parseArchiveEndpoint

forgeOptParser :: OP.Parser ForgeOptions
forgeOptParser = ForgeOptions <$> parseGenoDataSources
                              <*> parseForgeEntityInputs
                              <*> parseMaybeSnpFile
                              <*> parseIntersect
                              <*> parseOutGenotypeFormat True
                              <*> parseMinimalOutput
                              <*> parseOutOnlyGeno
                              <*> parseOutPackagePath
                              <*> parseMaybeOutPackageName
                              <*> parsePackageWise
                              <*> parseOutputPlinkPopMode
                              <*> parseOutputOrdered

genoconvertOptParser :: OP.Parser GenoconvertOptions
genoconvertOptParser = GenoconvertOptions <$> parseGenoDataSources
                                          <*> parseOutGenotypeFormat False
                                          <*> parseOutOnlyGeno
                                          <*> parseMaybeOutPackagePath
                                          <*> parseRemoveOld
                                          <*> parseOutputPlinkPopMode
                                          <*> parseOnlyLatest

summariseOptParser :: OP.Parser SummariseOptions
summariseOptParser = SummariseOptions <$> parseBasePaths
                                      <*> parseRawOutput

surveyOptParser :: OP.Parser SurveyOptions
surveyOptParser = SurveyOptions <$> parseBasePaths
                                <*> parseRawOutput
                                <*> parseOnlyLatest

rectifyOptParser :: OP.Parser RectifyOptions
rectifyOptParser = RectifyOptions <$> parseBasePaths
                                  <*> parseIgnorePoseidonVersion
                                  <*> parseMaybePoseidonVersion
                                  <*> parseMaybePackageVersionUpdate
                                  <*> parseChecksumsToRectify
                                  <*> parseMaybeContributors
                                  <*> parseOnlyLatest

validateOptParser :: OP.Parser ValidateOptions
validateOptParser = ValidateOptions <$> parseValidatePlan
                                    <*> parseNoExitCode
                                    <*> parseOnlyLatest

chronicleOptParser :: OP.Parser ChronicleOptions
chronicleOptParser = ChronicleOptions <$> parseBasePaths
                                      <*> parseChronOperation

timetravelOptParser :: OP.Parser TimetravelOptions
timetravelOptParser = TimetravelOptions <$> parseBasePaths
                                        <*> parseTimetravelSourcePath
                                        <*> parseTimetravelChronPath

serveOptParser :: OP.Parser ServeOptions
serveOptParser = ServeOptions <$> parseArchiveBasePaths
                              <*> parseMaybeZipDir
                              <*> parsePort
                              <*> parseIgnoreChecksums
                              <*> parseMaybeCertFiles

jannocoalesceOptParser :: OP.Parser JannoCoalesceOptions
jannocoalesceOptParser = JannoCoalesceOptions <$> parseJannocoalSourceSpec
                                              <*> parseJannocoalTargetFile
                                              <*> parseJannocoalOutSpec
                                              <*> parseJannocoalJannoColumns
                                              <*> parseJannocoalOverride
                                              <*> parseJannocoalSourceKey
                                              <*> parseJannocoalTargetKey
                                              <*> parseJannocoalIdStripRegex
