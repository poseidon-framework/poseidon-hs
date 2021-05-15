{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate where

import           Poseidon.Package  (PoseidonPackage (..),
                                   findAllPoseidonYmlFiles,
                                   getJointGenotypeData,
                                   readPoseidonPackageCollection,
                                   PackageReadOptions (..), defaultPackageReadOptions)

import           Control.Exception (SomeException, catch)
import           Control.Monad     (unless)
import           Data.List         (foldl')
import           Pipes             (runEffect, (>->))
import qualified Pipes.Prelude     as P
import           Pipes.Safe        (runSafeT)
import           System.Exit       (exitFailure, exitSuccess)
import           System.IO         (hPutStrLn, stderr)


-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs    :: [FilePath]
    , _optVerbose    :: Bool
    , _optIgnoreGeno :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = False
    , _readOptGenoCheck        = True
    }

runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs verbose ignoreGeno) = do
    posFiles <- concat <$> mapM findAllPoseidonYmlFiles baseDirs
    allPackages <- readPoseidonPackageCollection 
        pacReadOpts {_readOptVerbose = verbose, _readOptIgnoreGeno = ignoreGeno} 
        baseDirs
    let numberOfPOSEIDONymlFiles = length posFiles
        numberOfLoadedPackagesWithDuplicates = foldl' (+) 0 $ map posPacDuplicate allPackages
    check <- if numberOfPOSEIDONymlFiles == numberOfLoadedPackagesWithDuplicates
             then if ignoreGeno
                  then return True
                  else do checkJointGenotypeData allPackages
             else return False
    if check
    then do
        hPutStrLn stderr "Validation passed ✓"
        exitSuccess
    else do
        hPutStrLn stderr "Validation failed ✗"
        exitFailure

checkJointGenotypeData :: [PoseidonPackage] -> IO Bool
checkJointGenotypeData packageList = do
    hPutStrLn stderr "Checking first 100 SNPs for cross-package consistency..."
    parseFirst100SNPs packageList `catch` handler
    where
        handler :: SomeException -> IO Bool
        handler e = do
            print e
            return False

parseFirst100SNPs :: [PoseidonPackage] -> IO Bool
parseFirst100SNPs packageList = do
    runSafeT $ do
        (_, eigenstratProd) <- getJointGenotypeData False False packageList
        runEffect $ eigenstratProd >-> P.take 100 >-> P.drain
    return True
