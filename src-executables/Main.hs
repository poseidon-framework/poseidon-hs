{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad               (forM, when, forM_)
import Data.List (intercalate, isInfixOf)
import           Data.Text                   (unpack)
import           Data.Version                (showVersion)
import           Options.Applicative         as OP
import           Poseidon.Package            (PoseidonPackage (..),
                                              filterDuplicatePackages,
                                              findPoseidonPackages,
                                              getIndividuals, EigenstratIndEntry(..))
import           Text.Layout.Table           (asciiRoundS, column, def, expand,
                                              rowsG, tableString, titlesH)

data Options = CmdView ViewOptions
    | CmdSearch SearchOptions
    | CmdFstats FstatsOptions

data ViewOptions = ViewOptions
    { _voBaseDirs :: [FilePath]
    }

data SearchOptions = SearchOptions
    { _soBaseDirs     :: [FilePath]
    , _soSearchString :: String
    }

data FstatsOptions = FstatsOptions
    { _foBaseDirs :: [FilePath]
    }

main :: IO ()
main = do
    cmdOpts <- OP.execParser optParserInfo
    case cmdOpts of
        CmdView opts   -> runView opts
        CmdSearch opts -> runSearch opts
        CmdFstats opts -> runFstats opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> optParser) (OP.briefDesc <>
    OP.progDesc "poet (working title) is an analysis tool for \
        \poseidon databases.")

optParser :: OP.Parser Options
optParser = OP.subparser $
    OP.command "view" viewOptInfo <>
    OP.command "search" searchOptInfo <>
    OP.command "fstats" fstatsOptInfo
  where
    viewOptInfo = OP.info (OP.helper <*> (CmdView <$> viewOptParser))
        (OP.progDesc "view: show all packages with \
            \nr of samples, or only packages with selected samples and pops")
    searchOptInfo = OP.info (OP.helper <*> (CmdSearch <$> searchOptParser))
        (OP.progDesc "search: showing all \
            \packages and individuals where the individual or pop-name contains \
            \a search string")
    fstatsOptInfo = OP.info (OP.helper <*> (CmdFstats <$> fstatsOptParser))
        (OP.progDesc "fstat: running fstats")

viewOptParser :: OP.Parser ViewOptions
viewOptParser = ViewOptions <$> parseBasePaths

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions <$> parseBasePaths <*> parseSearchString

fstatsOptParser :: OP.Parser FstatsOptions
fstatsOptParser = FstatsOptions <$> parseBasePaths

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages"))

parseSearchString :: OP.Parser String
parseSearchString = OP.strOption (OP.long "searchString" <>
    OP.short 's' <>
    OP.metavar "STR" <>
    OP.help "A search string for individuals or populations")

runView :: ViewOptions -> IO ()
runView (ViewOptions baseDirs) = do
    packages <- getPackages $ baseDirs
    putStrLn $ (show . length $ packages) ++ " packages found:"
    let tableH = ["Title", "Date", "Nr Individuals"]
    tableB <- forM packages $ \pac -> do
        inds <- getIndividuals pac
        return [posPacTitle pac, show (posPacLastModified pac), show (length inds)]
    let colSpecs = replicate 3 (column expand def def def)
    putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

runSearch :: SearchOptions -> IO ()
runSearch (SearchOptions baseDirs searchString) = do
    packages <- getPackages $ baseDirs
    fullTable <- fmap concat . forM packages $ \pac -> do
        inds <- getIndividuals pac
        return [[posPacTitle pac, name, pop] | (EigenstratIndEntry name _ pop) <- inds]
    let tableB = filter (\[pacT, name, pop] -> searchString `isInfixOf` name || searchString `isInfixOf` pop) fullTable
        tableH = ["Package", "Individual", "Population"]
        colSpecs = replicate 3 (column expand def def def)
    putStrLn $ "Searched in " ++ show (length packages) ++ " packages:"
    if length tableB == 0
    then putStrLn "Nothing found"
    else putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

runFstats :: FstatsOptions -> IO ()
runFstats (FstatsOptions baseDirs) = do
    packages <- getPackages $ baseDirs
    print packages

getPackages :: [FilePath] -> IO [PoseidonPackage]
getPackages = fmap (filterDuplicatePackages . concat) . mapM findPoseidonPackages
