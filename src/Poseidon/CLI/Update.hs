module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..),
    ) where

import           Poseidon.Package           (PoseidonPackage (..),
                                             readPoseidonPackageCollection,
                                             updateChecksumsInPackage, writePoseidonPackage)

import           System.IO                  (hPutStrLn, stderr)

data UpdateOptions = UpdateOptions
    { _jaBaseDirs :: [FilePath]
    }

runUpdate :: UpdateOptions -> IO ()
runUpdate (UpdateOptions baseDirs) = do
    allPackages <- readPoseidonPackageCollection False baseDirs
    putStrLn "Updating checksums in the packages"
    updatedPackages <- mapM updateChecksumsInPackage allPackages
    putStrLn "Writing modified POSEIDON.yml files"
    mapM_ writePoseidonPackage updatedPackages
