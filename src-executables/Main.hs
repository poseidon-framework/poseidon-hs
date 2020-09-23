{-# LANGUAGE OverloadedStrings #-}

import           Control.Foldl              (list, purely)
import           Control.Monad              (forM, forM_)
import           Control.Monad.Catch        (throwM)
import           Data.ByteString.Char8      (pack, splitWith)
import           Data.List                  (groupBy, intercalate, intersect,
                                             nub, sortOn, transpose)
import           Data.Maybe                 (catMaybes)
import           Lens.Family2               (view)
import           Options.Applicative        as OP
import           Pipes                      (runEffect, (>->))
import           Pipes.Group                (chunksOf, folds, groupsBy)
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (runSafeT)
import           Poseidon.FStats            (BlockData (..), FStatSpec (..),
                                             PopSpec (..), fStatSpecParser,
                                             runParser, statSpecsFold)
import           Poseidon.Package           (EigenstratIndEntry (..),
                                             PoseidonPackage (..),
                                             getIndividuals,
                                             getJointGenotypeData,
                                             loadPoseidonPackages)
import           SequenceFormats.Eigenstrat (EigenstratSnpEntry (..))
import           SequenceFormats.Utils      (Chrom (..))
import           System.IO                  (hPutStrLn, stderr)
import           Text.Layout.Table          (asciiRoundS, column, def, expand,
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
    , _foRawOutput          :: Bool
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
fstatsOptParser = FstatsOptions <$> parseBasePaths <*> parseBootstrap <*> parseExcludeChroms <*> OP.some parseStatSpec <*> parseRawOutput

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
    OP.help "Specify a summary statistic to be computed. Can be given multiple times. Possible options are: F4(name1,name2,name3,name4), and similarly F3 and F2 stats, as well as PWM(name1,name2) for pairwise mismatch rates. Group names are by default matched with group names as indicated in the PLINK or Eigenstrat files in the Poseidon dataset. You can also specify individual names using the syntax \"<Ind_name>\", so enclosing them in angular brackets. You can also mix groups and individuals, like in \"F4(<Ind1>,Group2,Group3,<Ind4>)\".")

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
parseRawOutput = OP.switch (OP.long "raw" <> OP.short 'r' <> OP.help "output table as tsv without header. Useful for piping into group")

runList :: ListOptions -> IO ()
runList (ListOptions baseDirs listEntity rawOutput) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
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
runFstats (FstatsOptions baseDirs bootstrapSize exclusionList statSpecs rawOutput) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    let collectedStats = collectStatSpecGroups statSpecs
    relevantPackages <- findRelevantPackages collectedStats packages
    hPutStrLn stderr $ (show . length $ relevantPackages) ++ " relevant packages for chosen statistics identified:"
    forM_ relevantPackages $ \pac -> hPutStrLn stderr (posPacTitle pac)
    hPutStrLn stderr $ "Computing stats " ++ show statSpecs
    blockData <- runSafeT $ do
        (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData relevantPackages
        let eigenstratProdFiltered = eigenstratProd >-> P.filter chromFilter
            eigenstratProdInChunks = case bootstrapSize of
                Nothing -> chunkEigenstratByChromosome eigenstratProdFiltered
                Just chunkSize -> chunkEigenstratByNrSnps chunkSize eigenstratProdFiltered
        statsFold <- case statSpecsFold eigenstratIndEntries statSpecs of
            Left e  ->  throwM e
            Right f -> return f
        let summaryStatsProd = purely folds statsFold eigenstratProdInChunks
        purely P.fold list (summaryStatsProd >-> P.tee (P.map showBlockLogOutput >-> P.stdoutLn))
    let jackknifeEstimates = [computeJackknife (map blockSiteCount blocks) (map blockVal blocks) | blocks <- transpose blockData]
        colSpecs = replicate 4 (column expand def def def)
        tableH = ["Statistic", "Estimate", "StdErr", "Z score"]
        tableB = do
            (fstat, result) <- zip statSpecs jackknifeEstimates
            return [show fstat, show (fst result), show (snd result), show (fst result / snd result)]
    if   rawOutput
    then do
        putStrLn $ intercalate "\t" tableH
        forM_ tableB $ \row -> putStrLn (intercalate "\t" row)
    else putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    chromFilter (EigenstratSnpEntry chrom _ _ _ _ _, _) = chrom `notElem` exclusionList
    chunkEigenstratByChromosome = view (groupsBy sameChrom)
    sameChrom (EigenstratSnpEntry chrom1 _ _ _ _ _, _) (EigenstratSnpEntry chrom2 _ _ _ _ _, _) =
        chrom1 == chrom2
    chunkEigenstratByNrSnps chunkSize = view (chunksOf chunkSize)
    showBlockLogOutput blocks = "computing chunk range " ++ show (blockStartPos (head blocks)) ++ " - " ++
        show (blockEndPos (head blocks)) ++ ", size " ++ (show . blockSiteCount . head) blocks ++ ", values " ++
        (show . map blockVal) blocks

computeJackknife :: [Int] -> [Double] -> (Double, Double)
computeJackknife weights values =
    let weights'    = map fromIntegral weights
        sumWeights  = sum weights'
        g           = fromIntegral (length weights)
        theta       = sum [mj * val | (mj, val) <- zip weights' values] / sumWeights
        sigmaSquare = sum [mj * (val - theta) ^ (2 :: Int) / (sumWeights - mj) | (mj, val) <- zip weights' values] / g
    in  (theta, sqrt sigmaSquare)

collectStatSpecGroups :: [FStatSpec] -> [PopSpec]
collectStatSpecGroups statSpecs = nub . concat $ do
    stat <- statSpecs
    case stat of
        F4Spec  a b c d -> return [a, b, c, d]
        F3Spec  a b c   -> return [a, b, c]
        F2Spec  a b     -> return [a, b]
        PWMspec a b     -> return [a, b]

findRelevantPackages :: [PopSpec] -> [PoseidonPackage] -> IO [PoseidonPackage]
findRelevantPackages popSpecs packages = do
    let indNamesStats   = [ind   | PopSpecInd   ind   <- popSpecs]
        groupNamesStats = [group | PopSpecGroup group <- popSpecs]
    fmap catMaybes . forM packages $ \pac -> do
        inds <- getIndividuals pac
        let indNamesPac   = [ind   | EigenstratIndEntry ind _ _     <- inds]
            groupNamesPac = [group | EigenstratIndEntry _   _ group <- inds]
        if   length (intersect indNamesPac indNamesStats) > 0 || length (intersect groupNamesPac groupNamesStats) > 0
        then return (Just pac)
        else return Nothing



