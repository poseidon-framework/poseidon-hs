import           Options.Applicative as OP
import           Poseidon.Package    (filterDuplicatePackages,
                                      findPoseidonPackages, PoseidonPackage(..))

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
    print packages

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