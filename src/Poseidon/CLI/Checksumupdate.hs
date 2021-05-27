module Poseidon.CLI.Checksumupdate (
    runChecksumupdate, ChecksumupdateOptions (..),
    ) where

import           Poseidon.Package           (readPoseidonPackageCollection,
                                             updateChecksumsInPackage,
                                             writePoseidonPackage, 
                                             PackageReadOptions (..), defaultPackageReadOptions)

import           System.IO                  (hPutStrLn, stderr)

data ChecksumupdateOptions = ChecksumupdateOptions
    { _jaBaseDirs :: [FilePath]
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

runChecksumupdate :: ChecksumupdateOptions -> IO ()
runChecksumupdate (ChecksumupdateOptions baseDirs) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    hPutStrLn stderr "Calculating checksums"
    updatedPackages <- mapM updateChecksumsInPackage allPackages
    if allPackages == updatedPackages
    then do 
        hPutStrLn stderr "All checksums were already up-to-date"
    else do 
        hPutStrLn stderr "Writing modified POSEIDON.yml files"
        mapM_ writePoseidonPackage updatedPackages
