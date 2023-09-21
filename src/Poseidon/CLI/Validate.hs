{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Validate where

import           Poseidon.BibFile          (readBibTeXFile)
import           Poseidon.GenotypeData     (GenotypeDataSpec (..))
import           Poseidon.Janno            (JannoRows (..), readJannoFile)
import           Poseidon.Package          (PackageReadOptions (..),
                                            PoseidonException (..),
                                            PoseidonYamlStruct (..),
                                            defaultPackageReadOptions,
                                            findAllPoseidonYmlFiles,
                                            makePseudoPackageFromGenotypeData,
                                            readPoseidonPackageCollection,
                                            validateGeno, getJointIndividualInfo)
import           Poseidon.SequencingSource (SeqSourceRows (..),
                                            readSeqSourceFile)
import           Poseidon.Utils            (PoseidonIO, logError, logInfo)

import           Control.Monad             (unless)
import           Control.Monad.Catch       (throwM)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.List        (filterM, forM_)
import qualified Data.ByteString           as B
import           Data.List                 (intercalate, sortOn, groupBy)
import           Data.Yaml                 (decodeEither')
import           System.Directory          (doesDirectoryExist)
import           System.Exit               (exitFailure, exitSuccess)
import Poseidon.EntityTypes (IndividualInfo(..))

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
        , _valPlanIgnoreChecksums  :: Bool
        , _valPlanIgnorePosVersion :: Bool
      }
    | ValPlanPoseidonYaml FilePath
    | ValPlanGeno GenotypeDataSpec
    | ValPlanJanno FilePath
    | ValPlanSSF FilePath
    | ValPlanBib FilePath

runValidate :: ValidateOptions -> PoseidonIO ()
runValidate (ValidateOptions
    (ValPlanBaseDirs baseDirs ignoreGeno fullGeno ignoreDup ignoreChecksums ignorePosVersion)
    noExitCode) = do
    logInfo $ "Validating: " ++ intercalate ", " baseDirs
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = ignoreChecksums
        , _readOptGenoCheck        = True
        , _readOptIgnoreGeno       = ignoreGeno
        , _readOptFullGeno         = fullGeno
        , _readOptIgnorePosVersion = ignorePosVersion
        }
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    goodDirs <- liftIO $ filterM doesDirectoryExist baseDirs
    posFiles <- liftIO $ concat <$> mapM findAllPoseidonYmlFiles goodDirs
    unless ignoreDup $ do
        let allInds = getJointIndividualInfo allPackages
            duplicateGroups =   filter ((>1) . length)
                              . groupBy (\a b -> indInfoName a == indInfoName b)
                              . sortOn indInfoName $ allInds
        unless (null duplicateGroups) $ do
            logError "There are duplicated individuals in this package collection. \
                     \Set --ignoreDuplicates to ignore this issue."
            forM_ duplicateGroups $ \xs -> do
                logError $ "Duplicate individual " ++ show (indInfoName $ head xs)
                forM_ xs $ \x -> do
                    logError $ "  " ++ show x
            throwM . PoseidonCollectionException $ "Detected duplicate individuals."
    let numberOfPOSEIDONymlFiles = length posFiles
        numberOfLoadedPackages   = length allPackages
    logInfo $ show posFiles
    conclude (numberOfPOSEIDONymlFiles == numberOfLoadedPackages) noExitCode
runValidate (ValidateOptions (ValPlanPoseidonYaml path) noExitCode) = do
    logInfo $ "Validating: " ++ path
    bs <- liftIO $ B.readFile path
    yml <- case decodeEither' bs of
        Left err  -> throwM $ PoseidonYamlParseException path err
        Right pac -> return (pac :: PoseidonYamlStruct)
    logInfo $ "Read .yml file of package " ++ _posYamlTitle yml
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanGeno geno) noExitCode) = do
    logInfo $ "Validating: " ++ genoFile geno
    pac <- makePseudoPackageFromGenotypeData geno
    validateGeno pac True
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanJanno path) noExitCode) = do
    logInfo $ "Validating: " ++ path
    (JannoRows entries) <- readJannoFile path
    logInfo $ "All " ++ show (length entries) ++ " entries are valid"
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanSSF path) noExitCode) = do
    logInfo $ "Validating: " ++ path
    (SeqSourceRows entries) <- readSeqSourceFile path
    logInfo $ "All " ++ show (length entries) ++ " entries are valid"
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanBib path) noExitCode) = do
    logInfo $ "Validating: " ++ path
    entries <- liftIO $ readBibTeXFile path
    logInfo $ "All " ++ show (length entries) ++ " entries are valid"
    conclude True noExitCode

conclude :: Bool -> Bool -> PoseidonIO ()
conclude True noExitCode = do
    logInfo "Validation passed"
    unless noExitCode $ liftIO exitSuccess
conclude False noExitCode = do
    logError "Validation failed"
    unless noExitCode $ liftIO exitFailure
