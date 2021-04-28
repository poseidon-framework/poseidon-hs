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
    allPackages <- readPoseidonPackageCollection True True True False baseDirs
    hPutStrLn stderr "Calculating checksums"
    updatedPackages <- mapM updateChecksumsInPackage allPackages
    if allPackages == updatedPackages
    then do 
        hPutStrLn stderr "All checksums were already up-to-date"
    else do 
        hPutStrLn stderr "Writing modified POSEIDON.yml files"
        mapM_ writePoseidonPackage updatedPackages
