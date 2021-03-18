module Poseidon.CLI.Checksumupdate (
    runChecksumupdate, ChecksumupdateOptions (..),
    ) where

import           Poseidon.Package           (PoseidonPackage (..),
                                             readPoseidonPackageCollection,
                                             updateChecksumsInPackage, 
                                             writePoseidonPackage)

import           System.IO                  (hPutStrLn, stderr)

data ChecksumupdateOptions = ChecksumupdateOptions
    { _jaBaseDirs :: [FilePath]
    }

runChecksumupdate :: ChecksumupdateOptions -> IO ()
runChecksumupdate (ChecksumupdateOptions baseDirs) = do
<<<<<<< HEAD
    allPackages <- readPoseidonPackageCollection True True True baseDirs
=======
    allPackages <- readPoseidonPackageCollection True True baseDirs
>>>>>>> 66736e53846d7b0c604a98683481dd80c1db5140
    hPutStrLn stderr "Calculating checksums"
    updatedPackages <- mapM updateChecksumsInPackage allPackages
    if allPackages == updatedPackages
    then do 
        hPutStrLn stderr "All checksums were already up-to-date"
    else do 
        hPutStrLn stderr "Writing modified POSEIDON.yml files"
        mapM_ writePoseidonPackage updatedPackages
