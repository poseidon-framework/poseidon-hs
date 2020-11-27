{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Poseidon.CLI.FStats    (FStatSpec (..), FstatsOptions (..),
                                        JackknifeMode (..), fStatSpecParser,
                                        runFstats, runParser)
import           Poseidon.CLI.List      (ListEntity (..), ListOptions (..),
                                        runList)
import           Poseidon.CLI.Forge     (runForge, ForgeOptions(..),
                                        ForgeEntity(..), forgeEntitiesParser)
import           Poseidon.CLI.Summarise (SummariseOptions(..), runSummarise)
import           Poseidon.CLI.Survey    (SurveyOptions(..), runSurvey)
import           Poseidon.CLI.Validate  (ValidateOptions(..), runValidate)
import           Data.ByteString.Char8 (pack, splitWith)
import qualified Options.Applicative as OP
import           SequenceFormats.Utils (Chrom (..))
import           Text.Read             (readEither)


data Options = CmdFstats FstatsOptions
    | CmdList ListOptions
    | CmdForge ForgeOptions
    | CmdSummarise SummariseOptions
    | CmdSurvey SurveyOptions
    | CmdValidate ValidateOptions

main :: IO ()
main = do
    cmdOpts <- OP.execParser optParserInfo
    case cmdOpts of
        CmdFstats opts    -> runFstats opts
        CmdList opts      -> runList opts
        CmdForge opts     -> runForge opts
        CmdSummarise opts -> runSummarise opts
        CmdSurvey opts    -> runSurvey opts
        CmdValidate opts  -> runValidate opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> optParser) (OP.briefDesc <>
    OP.progDesc "trident is an analysis tool for poseidon databases.")

optParser :: OP.Parser Options
optParser = OP.subparser $
    OP.command "fstats" fstatsOptInfo <>
    OP.command "list" listOptInfo <>
    OP.command "forge" forgeOptInfo <>
    OP.command "summarise" summariseOptInfo <>
    OP.command "survey" surveyOptInfo <>
    OP.command "validate" validateOptInfo

  where
    fstatsOptInfo = OP.info (OP.helper <*> (CmdFstats <$> fstatsOptParser))
        (OP.progDesc "fstat: running fstats")
    listOptInfo = OP.info (OP.helper <*> (CmdList <$> listOptParser))
        (OP.progDesc "list: list packages, groups or individuals available in the specified packages")
    forgeOptInfo = OP.info (OP.helper <*> (CmdForge <$> forgeOptParser))
        (OP.progDesc "forge: forge the specified entities and create a new package")
    summariseOptInfo = OP.info (OP.helper <*> (CmdSummarise <$> summariseOptParser))
        (OP.progDesc "summarise: get an overview over the content of one or multiple packages")
    surveyOptInfo = OP.info (OP.helper <*> (CmdSurvey <$> surveyOptParser))
        (OP.progDesc "survey: survey the degree of completeness of package information")
    validateOptInfo = OP.info (OP.helper <*> (CmdValidate <$> validateOptParser))
        (OP.progDesc "validate: check one or multiple packages for structural correctness")

fstatsOptParser :: OP.Parser FstatsOptions
fstatsOptParser = FstatsOptions <$> parseBasePaths
                                <*> parseJackknife
                                <*> parseExcludeChroms
                                <*> OP.many parseStatSpecsDirect
                                <*> parseStatSpecsFromFile
                                <*> parseRawOutput

listOptParser :: OP.Parser ListOptions
listOptParser = ListOptions <$> parseBasePaths <*> parseListEntity <*> parseRawOutput

forgeOptParser :: OP.Parser ForgeOptions
forgeOptParser = ForgeOptions <$> parseBasePaths
                              <*> parseForgeEntitiesDirect
                              <*> parseOutPackagePath
                              <*> parseOutPackageName

summariseOptParser :: OP.Parser SummariseOptions
summariseOptParser = SummariseOptions <$> parseBasePaths

surveyOptParser :: OP.Parser SurveyOptions
surveyOptParser = SurveyOptions <$> parseBasePaths

validateOptParser :: OP.Parser ValidateOptions
validateOptParser = ValidateOptions <$> parseBasePaths

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

parseForgeEntitiesDirect :: OP.Parser [ForgeEntity]
parseForgeEntitiesDirect = OP.option (OP.eitherReader readForgeEntitiesString) (OP.long "forge" <>
    OP.help "...")

readForgeEntitiesString :: String -> Either String [ForgeEntity]
readForgeEntitiesString s = case runParser forgeEntitiesParser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages"))

parseOutPackagePath :: OP.Parser FilePath
parseOutPackagePath = OP.strOption (OP.long "outPackagePath" <>
    OP.short 'o' <>
    OP.help "the output package directory path")

parseOutPackageName :: OP.Parser FilePath
parseOutPackageName = OP.strOption (OP.long "outPackageName" <>
    OP.short 'n' <>
    OP.help "the output package name")

parseListEntity :: OP.Parser ListEntity
parseListEntity = parseListPackages <|> parseListGroups <|> parseListIndividuals
  where
    parseListPackages = OP.flag' ListPackages (OP.long "packages" <> OP.help "list packages")
    parseListGroups = OP.flag' ListGroups (OP.long "groups" <> OP.help "list groups")
    parseListIndividuals = OP.flag' ListIndividuals (OP.long "individuals" <> OP.help "list individuals")

parseRawOutput :: OP.Parser Bool
parseRawOutput = OP.switch (OP.long "raw" <> OP.short 'r' <> OP.help "output table as tsv without header. Useful for piping into grep or awk.")


