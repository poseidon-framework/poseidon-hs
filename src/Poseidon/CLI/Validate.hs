{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Validate where

import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         findAllPoseidonYmlFiles,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonIO, logError, logInfo)

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.List     (filterM)
import           Data.List              (foldl')
import           System.Directory       (doesDirectoryExist)
import           System.Exit            (exitFailure, exitSuccess)


-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _validateBaseDirs         :: [FilePath]
    , _validateIgnoreGeno       :: Bool
    , _validateNoExitCode       :: Bool
    , _validateIgnoreDuplicates :: Bool
    }

runValidate :: ValidateOptions -> PoseidonIO ()
runValidate (ValidateOptions baseDirs ignoreGeno noExitCode ignoreDup) = do

    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = False
        , _readOptGenoCheck        = True
        , _readOptIgnoreGeno       = ignoreGeno
        , _readOptStopOnDuplicates = not ignoreDup
        }

    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    goodDirs <- liftIO $ filterM doesDirectoryExist baseDirs
    posFiles <- liftIO $ concat <$> mapM findAllPoseidonYmlFiles goodDirs
    let numberOfPOSEIDONymlFiles = length posFiles
        numberOfLoadedPackagesWithDuplicates = foldl' (+) 0 $ map posPacDuplicate allPackages
    if numberOfPOSEIDONymlFiles == numberOfLoadedPackagesWithDuplicates
    then do
        logInfo "Validation passed"
        unless noExitCode $ liftIO exitSuccess
    else do
        logError "Validation failed"
        unless noExitCode $ liftIO exitFailure
