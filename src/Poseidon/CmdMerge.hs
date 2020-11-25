module Poseidon.CmdMerge (runMerge, MergeOptions(..)) where

import           Poseidon.Package       (PoseidonPackage(..),
                                        loadPoseidonPackages,
                                        maybeLoadJannoFiles,
                                        maybeLoadBibTeXFiles,
                                        bibToSimpleMaybeList,
                                        jannoToSimpleMaybeList,
                                        writeJannoFile,
                                        writeBibTeXFile,
                                        PoseidonSample(..),
                                        GenotypeDataSpec(..))
import           Data.Maybe             (catMaybes, isJust)
import           System.IO              (hPutStrLn, stderr)
import           System.FilePath        ((</>))
import           System.Directory       (createDirectory)
import           Control.Monad          (when)
import           Data.List              (nub, sortOn)
import           Text.CSL.Reference     (refId)

-- | A datatype representing command line options for the survey command
data MergeOptions = MergeOptions
    { _jaBaseDirs  :: [FilePath]
    , _outPacPath  :: FilePath
    }

-- | The main function running the janno command
runMerge :: MergeOptions -> IO ()
runMerge (MergeOptions baseDirs outPath) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    -- collect data
    -- JANNO
    jannoFiles <- maybeLoadJannoFiles packages
    let jannoMaybeList = jannoToSimpleMaybeList jannoFiles
    let anyJannoIssues = not $ all isJust jannoMaybeList
    let goodJannoRows = concat $ catMaybes jannoMaybeList
    -- bib
    bibFiles <- maybeLoadBibTeXFiles packages
    let bibMaybeList = bibToSimpleMaybeList bibFiles
    let anyBibIssues = not $ all isJust bibMaybeList
    let goodBibEntries = nub $ sortOn (show . refId) $ concat $ catMaybes bibMaybeList
    -- create new package
    createDirectory outPath
    writeJannoFile (outPath </> "test.janno") goodJannoRows
    writeBibTeXFile (outPath </> "LITERATURE.bib") goodBibEntries
    -- print read issue warning
    when (anyJannoIssues || anyBibIssues) $
        putStrLn "\nThere were issues with incomplete, missing or invalid data. Run trident validate to learn more."
