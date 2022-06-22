{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Poseidon.CLI.Validate where

import           Poseidon.Package  (PoseidonPackage (..),
                                   findAllPoseidonYmlFiles,
                                   readPoseidonPackageCollection,
                                   PackageReadOptions (..), defaultPackageReadOptions)
import           Poseidon.Utils    (PoseidonLogIO)

import           Colog             (logInfo, logError)
import           Control.Monad     (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.List         (foldl')
import           System.Exit       (exitFailure, exitSuccess)


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

runValidate :: ValidateOptions -> PoseidonLogIO ()
runValidate (ValidateOptions baseDirs verbose ignoreGeno noExitCode) = do
    posFiles <- liftIO $ concat <$> mapM findAllPoseidonYmlFiles baseDirs
    allPackages <- readPoseidonPackageCollection 
        pacReadOpts {_readOptVerbose = verbose, _readOptIgnoreGeno = ignoreGeno} 
        baseDirs
    let numberOfPOSEIDONymlFiles = length posFiles
        numberOfLoadedPackagesWithDuplicates = foldl' (+) 0 $ map posPacDuplicate allPackages
    if numberOfPOSEIDONymlFiles == numberOfLoadedPackagesWithDuplicates
    then do
        logInfo "Validation passed"
        unless noExitCode $ liftIO exitSuccess
    else do
        logError "Validation failed"
        unless noExitCode $ liftIO exitFailure
