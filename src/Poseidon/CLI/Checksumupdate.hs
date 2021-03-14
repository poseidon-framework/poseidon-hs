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
    allPackages <- readPoseidonPackageCollection True True baseDirs
    hPutStrLn stderr "Updating checksums in the packages"
    updatedPackages <- mapM updateChecksumsInPackage allPackages
    hPutStrLn stderr "Writing modified POSEIDON.yml files"
    mapM_ writePoseidonPackage updatedPackages
