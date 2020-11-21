module Poseidon.CmdMerge (runMerge, MergeOptions(..)) where

import           Poseidon.Package       (PoseidonPackage(..),
                                        loadPoseidonPackages,
                                        maybeLoadJannoFiles,
                                        maybeLoadBibTeXFiles,
                                        bibToSimpleMaybeList,
                                        jannoToSimpleMaybeList,
                                        writeJannoFile,
                                        PoseidonSample(..),
                                        GenotypeDataSpec(..))
import           Data.Maybe             (catMaybes, isJust)
import           System.IO              (hPutStrLn, stderr)  

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
    -- merge
    writeJannoFile outPath (concat $ catMaybes jannoMaybeList)
    
