{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate where

import           Poseidon.Package  (PoseidonPackage (..),
                                   findAllPoseidonYmlFiles,
                                   readPoseidonPackageCollection,
                                   PackageReadOptions (..), defaultPackageReadOptions)

import           Control.Monad     (unless)
import           Data.List         (foldl')
import           System.Exit       (exitFailure, exitSuccess)
import           System.IO         (hPutStrLn, stdout)


-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _validateBaseDirs     :: [FilePath]
    , _validateVerbose      :: Bool
    , _validateIgnoreGeno   :: Bool
    , _validateNoExitCode   :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = False
    , _readOptGenoCheck        = True
    }

runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs verbose ignoreGeno noExitCode) = do
    posFiles <- concat <$> mapM findAllPoseidonYmlFiles baseDirs
    allPackages <- readPoseidonPackageCollection 
        pacReadOpts {_readOptVerbose = verbose, _readOptIgnoreGeno = ignoreGeno} 
        baseDirs
    let numberOfPOSEIDONymlFiles = length posFiles
        numberOfLoadedPackagesWithDuplicates = foldl' (+) 0 $ map posPacDuplicate allPackages
    if numberOfPOSEIDONymlFiles == numberOfLoadedPackagesWithDuplicates
    then do
        hPutStrLn stdout "Validation passed: OK"
        unless noExitCode exitSuccess
    else do
        hPutStrLn stdout "Validation failed: ERROR"
        unless noExitCode exitFailure
