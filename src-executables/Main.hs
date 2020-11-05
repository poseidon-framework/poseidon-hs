{-# LANGUAGE OverloadedStrings #-}

import           Poseidon.CmdFStats    (FStatSpec (..), FstatsOptions (..),
                                        JackknifeMode (..), fStatSpecParser,
                                        runFstats, runParser)
import           Poseidon.CmdList      (ListEntity (..), ListOptions (..),
                                        runList)

import           Data.ByteString.Char8 (pack, splitWith)
import           Options.Applicative   as OP
import           SequenceFormats.Utils (Chrom (..))
import           Text.Read             (readEither)


data Options = CmdList ListOptions
    | CmdFstats FstatsOptions

main :: IO ()
main = do
    cmdOpts <- OP.execParser optParserInfo
    case cmdOpts of
        CmdList opts   -> runList opts
        CmdFstats opts -> runFstats opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> optParser) (OP.briefDesc <>
    OP.progDesc "poet (working title) is an analysis tool for \
        \poseidon databases.")

optParser :: OP.Parser Options
optParser = OP.subparser $
    OP.command "list" listOptInfo <>
    OP.command "fstats" fstatsOptInfo
  where
    listOptInfo = OP.info (OP.helper <*> (CmdList <$> listOptParser))
        (OP.progDesc "list: list packages, groups or individuals available in the specified packages")
    fstatsOptInfo = OP.info (OP.helper <*> (CmdFstats <$> fstatsOptParser))
        (OP.progDesc "fstat: running fstats")

listOptParser :: OP.Parser ListOptions
listOptParser = ListOptions <$> parseBasePaths <*> parseListEntity <*> parseRawOutput

fstatsOptParser :: OP.Parser FstatsOptions
fstatsOptParser = FstatsOptions <$> parseBasePaths
                                <*> parseJackknife
                                <*> parseExcludeChroms
                                <*> OP.some parseStatSpecsDirect
                                <*> parseStatSpecsFromFile
                                <*> parseRawOutput

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
        \Possible options are: F4(name1,name2,name3,name4), and similarly F3 and F2 stats, \
        \as well as PWM(name1,name2) for pairwise mismatch rates. Group names are by default \
        \matched with group names as indicated in the PLINK or Eigenstrat files in the Poseidon dataset. \
        \You can also specify individual names using the syntax \"<Ind_name>\", so enclosing them \
        \in angular brackets. You can also mix groups and individuals, like in \
        \\"F4(<Ind1>,Group2,Group3,<Ind4>)\".")

parseStatSpecsFromFile :: OP.Parser FilePath
parseStatSpecsFromFile = OP.strOption (OP.long "statFile" <> OP.help "Specify a file with F-Statistics specified \
    \similarly as specified for option --stat. One line per statistics")

readStatSpecString :: String -> Either String FStatSpec
readStatSpecString s = case runParser fStatSpecParser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages"))

parseListEntity :: OP.Parser ListEntity
parseListEntity = parseListPackages <|> parseListGroups <|> parseListIndividuals
  where
    parseListPackages = OP.flag' ListPackages (OP.long "packages" <> OP.help "list packages")
    parseListGroups = OP.flag' ListGroups (OP.long "groups" <> OP.help "list groups")
    parseListIndividuals = OP.flag' ListIndividuals (OP.long "individuals" <> OP.help "list individuals")

parseRawOutput :: OP.Parser Bool
parseRawOutput = OP.switch (OP.long "raw" <> OP.short 'r' <> OP.help "output table as tsv without header. Useful for piping into grep or awk.")

