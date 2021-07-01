module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..),
    ) where

import           Poseidon.Package           (readPoseidonPackageCollection,
                                             updateChecksumsInPackage,
                                             writePoseidonPackage, 
                                             PackageReadOptions (..), 
                                             defaultPackageReadOptions,
                                             ContributorSpec)

import           Data.Time                  (Day)
import           Data.Version               (Version)
import           System.IO                  (hPutStrLn, stderr)

data UpdateOptions = UpdateOptions
    { _updateBaseDirs :: [FilePath]
    , _updatePoseidonVersion :: Maybe Version
    , _updateVersionUpdate :: Maybe VersionComponent
    , _updateChecksumUpdate :: Bool
    , _updateIgnoreGeno :: Bool
    , _updateDate :: Maybe Day
    , _updateNewContributors :: [ContributorSpec]
    , _updateLog :: String
    }

data VersionComponent = Major | Minor | Patch

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

runUpdate :: UpdateOptions -> IO ()
runUpdate (UpdateOptions baseDirs poseidonVersion versionUpdate checksumupdate ignoreGeno date newContributors log) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    hPutStrLn stderr "Calculating checksums"
    updatedPackages <- mapM updateChecksumsInPackage allPackages
    if allPackages == updatedPackages
    then do 
        hPutStrLn stderr "All checksums were already up-to-date"
    else do 
        hPutStrLn stderr "Writing modified POSEIDON.yml files"
        mapM_ writePoseidonPackage updatedPackages
