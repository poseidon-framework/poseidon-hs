{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate where

import           Poseidon.Package (PoseidonPackage (..),
                                   findAllPoseidonYmlFiles,
                                   getJointGenotypeData,
                                   readPoseidonPackageCollection)

import           Control.Monad    (unless)
import           Data.List        (foldl')
import           Pipes            (runEffect, (>->))
import qualified Pipes.Prelude    as P
import           Pipes.Safe       (runSafeT)
import           System.Exit      (exitFailure, exitSuccess)
import           System.IO        (hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs    :: [FilePath]
    , _optIgnoreGeno :: Bool
    }

runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs ignoreGeno) = do
    posFiles <- concat <$> mapM findAllPoseidonYmlFiles baseDirs
    allPackages <- readPoseidonPackageCollection True False ignoreGeno baseDirs
    let numberOfPOSEIDONymlFiles = length posFiles
        numberOfLoadedPackagesWithDuplicates = foldl' (+) 0 $ map posPacDuplicate allPackages
    hPutStrLn stderr "Checking first 100 SNPs of genotype data"
    runSafeT $ do
        (_, eigenstratProd) <- getJointGenotypeData False False allPackages
        runEffect $ eigenstratProd >-> P.take 100 >-> P.drain
    if numberOfPOSEIDONymlFiles == numberOfLoadedPackagesWithDuplicates
    then do
        hPutStrLn stderr "Validation passed ✓"
        exitSuccess
    else do
        hPutStrLn stderr "Validation failed ✗"
        exitFailure
