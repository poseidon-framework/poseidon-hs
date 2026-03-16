{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Validate where

import           Poseidon.BibFile          (readBibTeXFile)
import           Poseidon.GenotypeData     (GenotypeDataSpec (..),
                                            GenotypeFileSpec (..))
import           Poseidon.Janno            (JannoRows (..), readJannoFile)
import           Poseidon.Package          (PackageReadOptions (..),
                                            PoseidonException (..),
                                            PoseidonYamlStruct (..),
                                            defaultPackageReadOptions,
                                            getJointIndividualInfo,
                                            makePseudoPackageFromGenotypeData,
                                            readPoseidonPackageCollectionWithSkipIndicator,
                                            validateGeno)
import           Poseidon.SequencingSource (SeqSourceRows (..),
                                            readSeqSourceFile)
import           Poseidon.Utils            (PoseidonIO, logError, logInfo)

import           Control.Monad             (forM_, unless)
import           Control.Monad.Catch       (throwM)
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as Bchs
import           Data.List                 (groupBy, intercalate, sortOn)
import           Data.Yaml                 (decodeEither')
import           Poseidon.EntityTypes      (IndividualInfo (..))
import           Poseidon.PoseidonVersion  (VersionedFile (..))
import           System.Exit               (exitFailure, exitSuccess)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _validatePlan           :: ValidatePlan
    , _validateMandatoryJanno :: [Bchs.ByteString]
    , _validateMandatorySSF   :: [Bchs.ByteString]
    , _validateNoExitCode     :: Bool
    , _validateOnlyLatest     :: Bool
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
    | ValPlanJanno VersionedFile
    | ValPlanSSF VersionedFile
    | ValPlanBib FilePath

runValidate :: ValidateOptions -> PoseidonIO ()
runValidate (ValidateOptions
    (ValPlanBaseDirs baseDirs ignoreGeno fullGeno ignoreDup ignoreChecksums ignorePosVersion)
    mandatoryJannoCols mandatorySSFCols
    noExitCode onlyLatest) = do
    logInfo $ "Validating: " ++ intercalate ", " baseDirs
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = ignoreChecksums
        , _readOptGenoCheck        = True
        , _readOptIgnoreGeno       = ignoreGeno
        , _readOptFullGeno         = fullGeno
        , _readOptIgnorePosVersion = ignorePosVersion
        , _readOptOnlyLatest       = onlyLatest
        , _readOptMandatoryJannoCols = mandatoryJannoCols
        , _readOptMandatorySSFCols   = mandatorySSFCols
        }
    -- load all packages
    (allPackages, packagesSkipped) <- readPoseidonPackageCollectionWithSkipIndicator pacReadOpts baseDirs
    -- stop on duplicates
    unless ignoreDup $ do
        (allInds, _) <- getJointIndividualInfo allPackages
        let duplicateGroups =   filter ((>1) . length)
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
    -- fail the validation if not all POSEIDON.yml files yielded a clean package
    conclude (not packagesSkipped) noExitCode
runValidate (ValidateOptions (ValPlanPoseidonYaml path) _ _ noExitCode _) = do
    logInfo $ "Validating: " ++ path
    bs <- liftIO $ B.readFile path
    yml <- case decodeEither' bs of
        Left err  -> throwM $ PoseidonYamlParseException path err
        Right pac -> return (pac :: PoseidonYamlStruct)
    logInfo $ "Read .yml file of package " ++ _posYamlTitle yml
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanGeno geno) _ _ noExitCode _) = do
    let gFile = case genotypeFileSpec geno of
            GenotypeEigenstrat gf _ _ _ _ _ -> gf
            GenotypePlink      gf _ _ _ _ _ -> gf
            GenotypeVCF        gf _         -> gf
    logInfo $ "Validating: " ++ gFile
    pac <- makePseudoPackageFromGenotypeData geno
    validateGeno pac True
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanJanno (VersionedFile pv path)) mandatoryJannoCols _ noExitCode _) = do
    logInfo $ "Validating: " ++ path
    (JannoRows entries) <- readJannoFile pv mandatoryJannoCols path
    logInfo $ "All " ++ show (length entries) ++ " entries are valid"
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanSSF (VersionedFile pv path)) _ mandatorySSFCols noExitCode _) = do
    logInfo $ "Validating: " ++ path
    (SeqSourceRows entries) <- readSeqSourceFile pv mandatorySSFCols path
    logInfo $ "All " ++ show (length entries) ++ " entries are valid"
    conclude True noExitCode
runValidate (ValidateOptions (ValPlanBib path) _ _ noExitCode _) = do
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
