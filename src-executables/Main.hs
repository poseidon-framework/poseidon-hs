{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad       (forM, forM_, when)
import           Data.List           (groupBy, intercalate, isInfixOf, nub,
                                      sortOn)
import           Data.Text           (unpack)
import           Data.Version        (showVersion)
import           Options.Applicative as OP
import           Poseidon.Package    (EigenstratIndEntry (..),
                                      PoseidonPackage (..),
                                      filterDuplicatePackages,
                                      findPoseidonPackages, getIndividuals)
import           System.IO           (hPutStrLn, stderr)
import           Text.Layout.Table   (asciiRoundS, column, def, expand,
                                      expandUntil, rowsG, tableString, titlesH)


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
    { _foBaseDirs :: [FilePath]
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
fstatsOptParser = FstatsOptions <$> parseBasePaths

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
    packages <- getPackages $ baseDirs
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
                    let packages = nub [i!!0 | i <- indGroup]
                    let nrInds = length indGroup
                    return [(indGroup!!0)!!2, intercalate "," packages, show nrInds]
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
runFstats (FstatsOptions baseDirs) = do
    packages <- getPackages $ baseDirs
    print packages

getPackages :: [FilePath] -> IO [PoseidonPackage]
getPackages = fmap (filterDuplicatePackages . concat) . mapM findPoseidonPackages
