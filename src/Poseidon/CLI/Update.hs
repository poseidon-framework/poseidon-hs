module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..),
    ) where

import           Poseidon.Package       (loadPoseidonPackagesIgnoreChecksums,
                                         updateChecksumsInPackage)

import           Data.Yaml              (encodeFile)
import           System.IO              (hPutStrLn, stderr)

data UpdateOptions = UpdateOptions
    { _jaBaseDirs :: [FilePath]
    }

runUpdate :: UpdateOptions -> IO ()
runUpdate (UpdateOptions baseDirs) = do
    putStrLn $ "Loading packages"
    packages <- loadPoseidonPackagesIgnoreChecksums baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    putStrLn $ "Updating checksums in the packages"
    updatedPackages <- mapM updateChecksumsInPackage packages
    putStrLn ""
    --mapM_ (encodeFile (outPath </> "POSEIDON.yml")) updatedPackages


