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
import           Poseidon.CLI.List                       (ListOptions (..),
                                                          runList)
import           Poseidon.CLI.OptparseApplicativeParsers
import           Poseidon.CLI.Serve                      (ServeOptions (..),
                                                          runServerMainThread)
import           Poseidon.CLI.Summarise                  (SummariseOptions (..),
                                                          runSummarise)
import           Poseidon.CLI.Survey                     (SurveyOptions (..),
                                                          runSurvey)
import           Poseidon.CLI.Update                     (UpdateOptions (..),
                                                          runUpdate)
import           Poseidon.CLI.Validate                   (ValidateOptions (..),
                                                          runValidate)
import           Poseidon.Janno                          (jannoHeaderString)
import           Poseidon.PoseidonVersion                (showPoseidonVersion,
                                                          validPoseidonVersions)
import           Poseidon.Utils                          (LogMode (..),
                                                          PlinkPopNameMode (..),
                                                          PoseidonException (..),
                                                          PoseidonIO, logError,
                                                          renderPoseidonException,
                                                          usePoseidonLogger)

import           Control.Applicative                     ((<|>))
import           Control.Exception                       (catch)
import           Data.List                               (intercalate)
import           Data.Version                            (showVersion)
import qualified Options.Applicative                     as OP
import           Options.Applicative.Help.Pretty         (string)
import           System.Exit                             (exitFailure)
import           System.IO                               (hPutStrLn, stderr)

data Options = Options {
    _logMode    :: LogMode
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
    | CmdUpdate UpdateOptions
    | CmdValidate ValidateOptions
    | CmdChronicle ChronicleOptions
    | CmdServe ServeOptions

main :: IO ()
main = do
    hPutStrLn stderr renderVersion
    hPutStrLn stderr ""
    (Options logMode errLength plinkMode subcommand) <- OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    catch (usePoseidonLogger logMode plinkMode $ runCmd subcommand) (handler logMode errLength plinkMode)
    where
        handler :: LogMode -> ErrorLength -> PlinkPopNameMode -> PoseidonException -> IO ()
        handler l len pm e = do
            usePoseidonLogger l pm $ logError $ truncateErr len $ renderPoseidonException e
            exitFailure
        truncateErr :: ErrorLength -> String -> String
        truncateErr CharInf         s = s
        truncateErr (CharCount len) s
            | length s > len          = take len s ++ "... (see more with --errLength)"
            | otherwise               = s

runCmd :: Subcommand -> PoseidonIO ()
runCmd o = case o of
    CmdInit opts        -> runInit opts
    CmdList opts        -> runList opts
    CmdFetch opts       -> runFetch opts
    CmdForge opts       -> runForge opts
    CmdGenoconvert opts -> runGenoconvert opts
    CmdSummarise opts   -> runSummarise opts
    CmdSurvey opts      -> runSurvey opts
    CmdUpdate opts      -> runUpdate opts
    CmdValidate opts    -> runValidate opts
    CmdChronicle opts   -> runChronicle opts
    CmdServe opts       -> runServerMainThread opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*>
        (Options <$> parseLogMode <*> parseErrorLength <*> parseInputPlinkPopMode <*> subcommandParser)) (
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
        OP.command "chronicle" chronicleOptInfo <>
        OP.command "serve" serveOptInfo <>
        OP.commandGroup "Poseidon HTTP Server" <> OP.internal
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
        OP.footerDoc (Just $ string $
               "Output structure\n"
            ++ "Data coverage proportions - 0: ., <0.25: ░, <0.5: ▒, <1: ▓, 1: █\n"
            ++ ".janno column order - G: Genotype data present, S: .ssf file present, B: .bib file present, "
            ++ intercalate ", " (zipWith (\x y -> show x ++ ": " ++ y) ([1..] :: [Int]) jannoHeaderString)
            ))
    updateOptInfo = OP.info (OP.helper <*> (CmdUpdate <$> updateOptParser))
        (OP.progDesc "Update POSEIDON.yml files automatically")
    validateOptInfo = OP.info (OP.helper <*> (CmdValidate <$> validateOptParser))
        (OP.progDesc "Check one or multiple Poseidon packages for structural correctness")
    chronicleOptInfo = OP.info (OP.helper <*> (CmdChronicle <$> chronicleOptParser))
        (OP.progDesc "Create chronicle files for package collections")
    serveOptInfo    = OP.info (OP.helper <*> (CmdServe <$> serveOptParser))
        (OP.progDesc "Serve Poseidon packages via HTTP or HTTPS")

initOptParser :: OP.Parser InitOptions
initOptParser = InitOptions <$> parseInGenotypeDataset
                            <*> parseOutPackagePath
                            <*> parseMaybeOutPackageName
                            <*> parseMinimalOutput

listOptParser :: OP.Parser ListOptions
listOptParser = ListOptions <$> parseRepoLocation
                            <*> parseListEntity
                            <*> parseRawOutput

fetchOptParser :: OP.Parser FetchOptions
fetchOptParser = FetchOptions <$> parseBasePaths
                              <*> parseFetchEntityInputs
                              <*> parseRemoteURL

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

genoconvertOptParser :: OP.Parser GenoconvertOptions
genoconvertOptParser = GenoconvertOptions <$> parseGenoDataSources
                                          <*> parseOutGenotypeFormat False
                                          <*> parseOutOnlyGeno
                                          <*> parseMaybeOutPackagePath
                                          <*> parseRemoveOld
                                          <*> parseOutputPlinkPopMode

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
                                    <*> parseIgnoreGeno
                                    <*> parseFullGeno
                                    <*> parseNoExitCode
                                    <*> parseIgnoreDuplicates

chronicleOptParser :: OP.Parser ChronicleOptions
chronicleOptParser = ChronicleOptions <$> parseBasePaths
                                    <*> parseSnapOperation

serveOptParser :: OP.Parser ServeOptions
serveOptParser = ServeOptions <$> parseBasePaths
                                    <*> parseMaybeZipDir
                                    <*> parsePort
                                    <*> parseIgnoreChecksums
                                    <*> parseMaybeCertFiles
