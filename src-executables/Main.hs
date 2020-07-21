{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM)
import           Data.Text           (unpack)
import           Data.Version        (showVersion)
import           Options.Applicative as OP
import           Poseidon.Package    (PoseidonPackage (..),
                                      filterDuplicatePackages,
                                      findPoseidonPackages, getIndividuals)
import           Text.Layout.Table   (asciiRoundS, column, def, expand, rowsG,
                                      tableString, titlesH)

data Options = CmdView ViewOptions
    | CmdSearch SearchOptions
    | CmdFstats FstatsOptions

data ViewOptions = ViewOptions
    { _voBaseDirs :: [FilePath]
    }

data SearchOptions = SearchOptions
    { _soBaseDirs :: [FilePath]
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
        (OP.progDesc "search: fshowing all \
            \packages and individuals where the individual or pop-name contains \
            \a search string")
    fstatsOptInfo = OP.info (OP.helper <*> (CmdFstats <$> fstatsOptParser))
        (OP.progDesc "fstat: running fstats")

viewOptParser :: OP.Parser ViewOptions
viewOptParser = ViewOptions <$> parseBasePaths

searchOptParser :: OP.Parser SearchOptions
searchOptParser = SearchOptions <$> parseBasePaths

fstatsOptParser :: OP.Parser FstatsOptions
fstatsOptParser = FstatsOptions <$> parseBasePaths

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages"))

runView :: ViewOptions -> IO ()
runView (ViewOptions baseDirs) = do
    packages <- getPackages $ baseDirs
    putStrLn $ (show . length $ packages) ++ " packages found:"
    let tableH = ["Title", "Date", "Nr Individuals"]
    tableB <- forM packages $ \pac -> do
        inds <- getIndividuals pac
        return [unpack (posPacTitle pac), show (posPacLastModified pac), show (length inds)]
    let colSpecs = replicate 3 (column expand def def def)
    putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

runSearch :: SearchOptions -> IO ()
runSearch (SearchOptions baseDirs) = do
    packages <- getPackages $ baseDirs
    print packages

runFstats :: FstatsOptions -> IO ()
runFstats (FstatsOptions baseDirs) = do
    packages <- getPackages $ baseDirs
    print packages

getPackages :: [FilePath] -> IO [PoseidonPackage]
getPackages = fmap (filterDuplicatePackages . concat) . mapM findPoseidonPackages
