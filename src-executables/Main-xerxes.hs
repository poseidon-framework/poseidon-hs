{-# LANGUAGE OverloadedStrings #-}

import           Poseidon.Analysis.CLI.FStats            (FstatsOptions (..),
                                                          runFstats, runParser)
import           Poseidon.Analysis.CLI.RAS               (FreqSpec (..),
                                                          RASOptions (..),
                                                          runRAS)
import           Poseidon.Analysis.FStatsConfig          (FStatInput (..),
                                                          fStatSpecParser)
import           Poseidon.Analysis.Utils                 (JackknifeMode (..))
import           Poseidon.Generator.CLI.AdmixPops        (AdmixPopsMethodSettings (..),
                                                          AdmixPopsOptions (..),
                                                          runAdmixPops)
import           Poseidon.Generator.Parsers              (readIndWithAdmixtureSetString)
import           Poseidon.Generator.Types                (RequestedInd)

import           Control.Applicative                     ((<|>))
import           Control.Exception                       (catch)
import           Data.ByteString.Char8                   (pack, splitWith)
import           Data.List                               (intercalate)
import           Data.Version                            (showVersion)
import qualified Options.Applicative                     as OP
import           Paths_poseidon_hs                       (version)
import           Poseidon.CLI.OptparseApplicativeParsers
import           Poseidon.PoseidonVersion                (showPoseidonVersion,
                                                          validPoseidonVersions)
import           Poseidon.Utils                          (LogMode (..),
                                                          PlinkPopNameMode (..),
                                                          PoseidonException (..),
                                                          PoseidonIO, TestMode,
                                                          logError,
                                                          renderPoseidonException,
                                                          usePoseidonLogger)
import           SequenceFormats.Utils                   (Chrom (..))
import           System.Exit                             (exitFailure)
import           System.IO                               (hPutStrLn, stderr)
import           Text.Read                               (readEither)

data Options = Options {
    _logMode    :: LogMode
  , _testMode   :: TestMode
  , _errLength  :: ErrorLength
  , _plinkMode  :: PlinkPopNameMode
  , _subcommand :: Subcommand
  }

data Subcommand =
      CmdFstats FstatsOptions
    | CmdRAS RASOptions
    | CmdAdmixPops AdmixPopsOptions

main :: IO ()
main = do
    hPutStrLn stderr renderVersion
    hPutStrLn stderr ""
    (Options logMode testMode errLength plinkMode subcommand) <- OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    catch (usePoseidonLogger logMode testMode plinkMode $ runCmd subcommand) (handler logMode testMode errLength plinkMode)
    where
        handler :: LogMode -> TestMode -> ErrorLength -> PlinkPopNameMode -> PoseidonException -> IO ()
        handler l t len pm e = do
            usePoseidonLogger l t pm $ logError $ truncateErr len $ renderPoseidonException e
            exitFailure
        truncateErr :: ErrorLength -> String -> String
        truncateErr CharInf         s = s
        truncateErr (CharCount len) s
            | length s > len          = take len s ++ "... (see more with --errLength)"
            | otherwise               = s

runCmd :: Subcommand -> PoseidonIO ()
runCmd o = case o of
    CmdFstats opts    -> runFstats opts
    CmdRAS opts       -> runRAS opts
    CmdAdmixPops opts -> runAdmixPops opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*>
        (Options <$> parseLogMode
                 <*> parseTestMode
                 <*> parseErrorLength
                 <*> parseInputPlinkPopMode
                 <*> subcommandParser)
        ) (
    OP.briefDesc <>
    OP.progDesc "xerxes is an analysis tool for Poseidon packages. \
                \Report issues here: \
                \https://github.com/poseidon-framework/poseidon-analysis-hs/issues"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version number")

renderVersion :: String
renderVersion =
    "xerxes v" ++ showVersion version ++ " for poseidon v" ++
    intercalate ", v" (map showPoseidonVersion validPoseidonVersions) ++ "\n" ++
    "https://poseidon-framework.github.io"

subcommandParser :: OP.Parser Subcommand
subcommandParser =
    OP.subparser (
        OP.command "fstats" fstatsOptInfo <>
        OP.command "ras" rasOptInfo <>
        OP.commandGroup "Analysis commands:"
    ) <|>
    OP.subparser (
        OP.command "admixpops" admixPopsOptInfo <>
        OP.commandGroup "Artificial genotype generators:"
    )
  where
    fstatsOptInfo = OP.info (OP.helper <*> (CmdFstats <$> fstatsOptParser))
        (OP.progDesc "Compute f-statistics on groups and invidiuals within and across Poseidon packages")
    rasOptInfo = OP.info (OP.helper <*> (CmdRAS <$> rasOptParser))
        (OP.progDesc "Compute RAS statistics on groups and individuals within and across Poseidon packages")
    admixPopsOptInfo = OP.info (OP.helper <*> (CmdAdmixPops <$> admixPopsOptParser))
        (OP.progDesc "Generate individuals with randomized genotype profiles based on admixture proportions (experimental)")


fstatsOptParser :: OP.Parser FstatsOptions
fstatsOptParser = FstatsOptions <$> parseBasePaths
                                <*> parseJackknife
                                <*> parseExcludeChroms
                                <*> parseFstatInput
                                <*> parseMaxSnps
                                <*> parseNoTransitions
                                <*> parseTableOutFile
                                <*> parseBlockTableFile

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

parseFstatInput :: OP.Parser [FStatInput]
parseFstatInput = OP.some (parseStatSpecsDirect <|> parseYamlInput <|> parseSimpleText)
  where
    parseStatSpecsDirect = OP.option (FStatInputDirect <$> OP.eitherReader readStatSpecString) (OP.long "stat" <>
        OP.help "Specify a summary statistic to be computed. Can be given multiple times. Possible options are: F4(a, \
            \b, c, d), F3(a, b, c), F2(a, b), PWM(a, b), FST(a, b), Het(a) and some more special options \
            \described at https://poseidon-framework.github.io/#/xerxes?id=fstats-command. Valid entities used in the \
            \statistics are group names as specified in the *.fam, *.ind or *.janno failes, individual names using the \
            \syntax \"<Ind_name>\", so enclosing them in angular brackets, and entire packages like \"*Package1*\" using the \
            \Poseidon package title. You can mix entity types, like in \
            \\"F4(<Ind1>,Group2,*Pac*,<Ind4>)\". Group or individual names are separated by commas, and a comma \
            \can be followed by any number of spaces.")
    parseYamlInput = OP.option (FStatInputYaml <$> OP.str) (OP.long "statConfig" <> OP.help "Specify a yaml file for the Fstatistics and group configurations")
    parseSimpleText = OP.option (FStatInputSimpleText <$> OP.str) (OP.long "statFile" <> OP.help "Specify a file with F-Statistics specified \
    \similarly as specified for option --stat. One line per statistics, and no new-line at the end")
    readStatSpecString s = case runParser fStatSpecParser () "" s of
        Left p  -> Left (show p)
        Right x -> Right x

rasOptParser :: OP.Parser RASOptions
rasOptParser = RASOptions <$>
    parseBasePaths <*>
    parseJackknife <*>
    parseExcludeChroms <*>
    parsePopConfigFile <*>
    parseMinFreq <*>
    parseMaxFreq <*>
    parseMaxMissingness <*>
    parseBlockTableFile <*>
    parseF4tableOutFile <*>
    parseMaxSnps <*>
    parseNoTransitions <*>
    parseBedFile

parsePopConfigFile :: OP.Parser FilePath
parsePopConfigFile = OP.option OP.str (OP.long "popConfigFile" <> OP.help "a file containing the population configuration")

parseMaxFreq :: OP.Parser FreqSpec
parseMaxFreq = parseK <|> parseX <|> parseNone
  where
    parseK = OP.option (FreqK <$> OP.auto) (OP.long "maxAC" <>
        OP.help "define a maximal allele-count cutoff for the RAS statistics. ")
    parseX = OP.option (FreqX <$> OP.auto) (OP.long "maxFreq" <>
        OP.help "define a maximal allele-frequency cutoff for the RAS statistics. ")
    parseNone = OP.flag' FreqNone (OP.long "noMaxFreq" <>
        OP.help "switch off the maximum allele frequency filter. This cam help mimic Outgroup-F3")

parseMinFreq :: OP.Parser FreqSpec
parseMinFreq = parseK <|> parseX <|> parseNone
  where
    parseK = OP.option (FreqK <$> OP.auto) (OP.long "minAC" <>
        OP.help "define a minimal allele-count cutoff for the RAS statistics. ")
    parseX = OP.option (FreqX <$> OP.auto) (OP.long "minFreq" <>
        OP.help "define a minimal allele-frequency cutoff for the RAS statistics. ")
    parseNone = OP.flag' FreqNone (OP.long "noMinFreq" <>
        OP.help "switch off the minimum allele frequency filter")

parseMaxMissingness :: OP.Parser Double
parseMaxMissingness = OP.option OP.auto (OP.long "maxMissingness" <> OP.short 'm' <>
    OP.help "define a maximal missingness for the right populations in the RAS statistics." <>
    OP.value 0.1 <> OP.showDefault)

parseTableOutFile :: OP.Parser (Maybe FilePath)
parseTableOutFile = OP.option (Just <$> OP.str) (OP.long "tableOutFile" <> OP.short 'f' <>
    OP.help "a file to which results are written as tab-separated file" <> OP.value Nothing)

parseF4tableOutFile :: OP.Parser (Maybe FilePath)
parseF4tableOutFile = OP.option (Just <$> OP.str) (OP.long "f4TableOutFile" <>
    OP.help "a file to which F4 computations are written as tab-separated file" <> OP.value Nothing)

parseBlockTableFile :: OP.Parser (Maybe FilePath)
parseBlockTableFile = OP.option (Just <$> OP.str) (OP.long "blockTableFile" <>
    OP.help "a file to which the per-Block results are written as tab-separated file" <> OP.value Nothing)

-- parseFstatConfig :: OP.Parser (Maybe FilePath)
-- parseFstatConfig = OP.option (Just <$> OP.str) (OP.long "config" <> OP.help "Config file in Dhall" <> OP.value Nothing)

parseMaxSnps :: OP.Parser (Maybe Int)
parseMaxSnps = OP.option (Just <$> OP.auto) (OP.long "maxSnps" <>
    OP.help "Stop after a maximum nr of snps has been processed. Useful for short test runs" <>
    OP.value Nothing <> OP.hidden)

parseNoTransitions :: OP.Parser Bool
parseNoTransitions = OP.switch (OP.long "noTransitions" <> OP.help "Skip transition SNPs and use only transversions")

parseBedFile :: OP.Parser (Maybe FilePath)
parseBedFile = OP.option (Just <$> OP.str) (OP.long "bedFile" <> OP.help "An optional bed file that gives sites to be \
    \included in the analysis." <> OP.value Nothing)

admixPopsOptParser :: OP.Parser AdmixPopsOptions
admixPopsOptParser = AdmixPopsOptions <$> parseGenoDataSources
                                      <*> parseIndWithAdmixtureSetDirect
                                      <*> parseIndWithAdmixtureSetFromFile
                                      <*> parseAdmixPopsMethodSettings
                                      <*> parseOutGenotypeFormat True
                                      <*> parseOutPackagePath
                                      <*> parseMaybeOutPackageName
                                      <*> parseOutputPlinkPopMode

parseIndWithAdmixtureSetDirect :: OP.Parser [RequestedInd]
parseIndWithAdmixtureSetDirect = OP.option (OP.eitherReader readIndWithAdmixtureSetString) (
    OP.long "admixString" <>
    OP.short 'a' <>
    OP.value [] <>
    OP.help "Artificial individual to generate. Each setup is a string of the form \
            \\"[id:group](population1=10+population2=30+...)\". Multiple setups can be listed separated by ;. \
            \id and group are simple strings. \
            \The population fractions must be simple integers and sum to 100."
    )

parseIndWithAdmixtureSetFromFile :: OP.Parser (Maybe FilePath)
parseIndWithAdmixtureSetFromFile = OP.option (Just <$> OP.str) (OP.long "admixFile" <>
    OP.value Nothing <>
    OP.help "A file with admixStrings. \
            \Works just as -a, but multiple values can be given separated by newline. \
            \-a and --admixFile can be combined."
    )

parseAdmixPopsMethodSettings :: OP.Parser AdmixPopsMethodSettings
parseAdmixPopsMethodSettings = (PerSNP <$> parseMarginalizeMissing) <|> (parseInChunks *> (InChunks <$> parseChunkSize))

parseMarginalizeMissing :: OP.Parser Bool
parseMarginalizeMissing = OP.switch (
    OP.long "marginalizeMissing" <>
    OP.help "ignore missing SNPs in the per-population genotype frequency calculation \
            \(except all individuals have missing information for a given SNP)"
    )

parseInChunks :: OP.Parser ()
parseInChunks = OP.flag' () (
    OP.long "inChunks" <>
    OP.help "construct the artificial individuals by sampling contiguous stretches of SNPs \
            \('chunks') from random individuals in the source populations"
    )

parseChunkSize :: OP.Parser Int
parseChunkSize = OP.option OP.auto (
    OP.long "chunkSize" <>
    OP.value 5000 <>
    OP.help "The number of SNPs in one chunks" <>
    OP.showDefault
    )
