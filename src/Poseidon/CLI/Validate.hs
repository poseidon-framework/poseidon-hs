{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Validate where

import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         findAllPoseidonYmlFiles,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonIO, logError, logInfo)
import Poseidon.GenotypeData (GenotypeDataSpec)

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.List     (filterM)
import           Data.List              (foldl')
import           System.Directory       (doesDirectoryExist)
import           System.Exit            (exitFailure, exitSuccess)


-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _validatePlan       :: ValidatePlan
    , _validateNoExitCode :: Bool
    }

data ValidatePlan =
      ValPlanBaseDirs { 
          _valPlanBaseDirs         :: [FilePath]
        , _valPlanIgnoreGeno       :: Bool
        , _valPlanFullGeno         :: Bool
        , _valPlanIgnoreDuplicates :: Bool
      }
    | ValPlanPoseidonYaml FilePath
    | ValPlanGeno GenotypeDataSpec
    | ValPlanJanno FilePath
    | ValPlanSSF FilePath
    | ValPlanBib FilePath

runValidate :: ValidateOptions -> PoseidonIO ()
runValidate (ValidateOptions (ValPlanBaseDirs baseDirs ignoreGeno fullGeno ignoreDup) noExitCode) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = False
        , _readOptGenoCheck        = True
        , _readOptIgnoreGeno       = ignoreGeno
        , _readOptFullGeno         = fullGeno
        , _readOptStopOnDuplicates = not ignoreDup
        }
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    goodDirs <- liftIO $ filterM doesDirectoryExist baseDirs
    posFiles <- liftIO $ concat <$> mapM findAllPoseidonYmlFiles goodDirs
    let numberOfPOSEIDONymlFiles = length posFiles
        numberOfLoadedPackagesWithDuplicates = foldl' (+) 0 $ map posPacDuplicate allPackages
    conclude (numberOfPOSEIDONymlFiles == numberOfLoadedPackagesWithDuplicates) noExitCode
runValidate (ValidateOptions (ValPlanPoseidonYaml path) noExitCode) = do
    undefined
runValidate (ValidateOptions (ValPlanGeno geno) noExitCode) = do
    undefined
runValidate (ValidateOptions (ValPlanJanno path) noExitCode) = do
    undefined
runValidate (ValidateOptions (ValPlanSSF path) noExitCode) = do
    undefined
runValidate (ValidateOptions (ValPlanBib path) noExitCode) = do
    undefined

conclude :: Bool -> Bool -> PoseidonIO ()
conclude True noExitCode = do
    logInfo "Validation passed"
    unless noExitCode $ liftIO exitSuccess
conclude False noExitCode = do
    logError "Validation failed"
    unless noExitCode $ liftIO exitFailure
