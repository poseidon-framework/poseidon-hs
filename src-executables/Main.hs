{-# LANGUAGE OverloadedStrings #-}

import           Poseidon.FStats       (FStatSpec (..), fStatSpecParser, runParser)
import           Poseidon.Package      (EigenstratIndEntry (..),
                                        PoseidonPackage (..), getIndividuals,
                                        loadPoseidonPackages)

import           Control.Monad         (forM)
import           Data.ByteString.Char8 (pack, splitWith)
import           Data.List             (groupBy, intercalate, nub, sortOn)
import           Options.Applicative   as OP
import           SequenceFormats.Utils (Chrom (..))
import           System.IO             (hPutStrLn, stderr)
import           Text.Layout.Table     (asciiRoundS, column, def, expand,
                                        expandUntil, rowsG, tableString,
                                        titlesH)

data Options = CmdList ListOptions
    | CmdFstats FstatsOptions

data ListOptions = ListOptions
    { _loBaseDirs   :: [FilePath]
    , _loListEntity :: ListEntity
    , _loRawOutput  :: Bool
    }

data ListEntity = ListPackages
    | ListGroups
    | ListIndividuals

data FstatsOptions = FstatsOptions
    { _foBaseDirs           :: [FilePath]
    , _foBootstrapBlockSize :: Maybe Int
    , _foExcludeChroms      :: [Chrom]
    , _foStatSpec           :: [FStatSpec]
    }

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
fstatsOptParser = FstatsOptions <$> parseBasePaths <*> parseBootstrap <*> parseExcludeChroms <*> OP.some parseStatSpec

parseBootstrap :: OP.Parser (Maybe Int)
parseBootstrap = OP.option (Just <$> OP.auto) (OP.long "blocksize" <> OP.short 'b' <>
    OP.help "Bootstrap block size. Leave blank for \
        \chromosome-wise bootstrap. Otherwise give the block size in nr of Snps (e.g. \
        \5000)" <> OP.value Nothing)

parseExcludeChroms :: OP.Parser [Chrom]
parseExcludeChroms = OP.option (map Chrom . splitWith (==',') . pack <$> OP.str) (OP.long "excludeChroms" <> OP.short 'e' <>
    OP.help "List of chromosome names to exclude chromosomes, given as comma-separated \
        \list. Defaults to X, Y, MT, chrX, chrY, chrMT, 23,24,90" <> OP.value [Chrom "X", Chrom "Y", Chrom "MT",
        Chrom "chrX", Chrom "chrY", Chrom "chrMT", Chrom "23", Chrom "24", Chrom "90"])

parseStatSpec :: OP.Parser FStatSpec
parseStatSpec = OP.option (OP.eitherReader readStatSpecString) (OP.long "stat" <>
    OP.help "Specify a summary statistic to be computed. Can be given multiple times.")

readStatSpecString :: String -> Either String FStatSpec
readStatSpecString s = case runParser fStatSpecParser () "" s of
    Left p -> Left (show p)
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
parseRawOutput = OP.switch (OP.long "raw" <> OP.short 'r' <> OP.help "output table as tsv without header. Useful for piping into group")

runList :: ListOptions -> IO ()
runList (ListOptions baseDirs listEntity rawOutput) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " packages found"
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            let tableH = ["Title", "Date", "Nr Individuals"]
            tableB <- forM packages $ \pac -> do
                inds <- getIndividuals pac
                return [posPacTitle pac, showMaybeDate (posPacLastModified pac), show (length inds)]
            return (tableH, tableB)
        ListGroups -> do
            allInds <- fmap concat . forM packages $ \pac -> do
                inds <- getIndividuals pac
                return [[posPacTitle pac, name, pop] | (EigenstratIndEntry name _ pop) <- inds]
            let allIndsSortedByGroup = groupBy (\a b -> a!!2 == b!!2) . sortOn (!!2) $ allInds
                tableB = do
                    indGroup <- allIndsSortedByGroup
                    let packages_ = nub [i!!0 | i <- indGroup]
                    let nrInds = length indGroup
                    return [(indGroup!!0)!!2, intercalate "," packages_, show nrInds]
            let tableH = ["Group", "Packages", "Nr Individuals"]
            return (tableH, tableB)
        ListIndividuals -> do
            tableB <- fmap concat . forM packages $ \pac -> do
                inds <- getIndividuals pac
                return [[posPacTitle pac, name, pop] | (EigenstratIndEntry name _ pop) <- inds]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " individuals.")
            let tableH = ["Package", "Individual", "Population"]
            return (tableH, tableB)

    if rawOutput then
        putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else
        case listEntity of
            ListGroups -> do
                let colSpecs = replicate 3 (column (expandUntil 50) def def def)
                putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
            _ -> do
                let colSpecs = replicate 3 (column expand def def def)
                putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    showMaybeDate (Just d) = show d
    showMaybeDate Nothing  = "n/a"

runFstats :: FstatsOptions -> IO ()
runFstats (FstatsOptions baseDirs _ _ statSpec) = do
    packages <- loadPoseidonPackages baseDirs
    print statSpec
    print packages
