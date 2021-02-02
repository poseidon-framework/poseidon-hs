{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate where

import           Poseidon.Package           (findAllPoseidonYmlFiles,
                                             readPoseidonPackageCollection)

import           Control.Monad              (unless)
import           System.Exit                (exitFailure, exitSuccess)
import           System.IO                  (hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs :: [FilePath]
    , _optIgnoreGeno :: Bool
    }

runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs ignoreGeno) = do
    posFiles <- concat <$> mapM findAllPoseidonYmlFiles baseDirs
    allPackages <- readPoseidonPackageCollection False ignoreGeno baseDirs
    -- TODO: This check as implemented does not consider packages that might 
    -- have been removed as duplicates. A more clever solution would be very
    -- welcome
    if length posFiles == length allPackages
    then do
        hPutStrLn stderr "Validation passed ✓"
        exitSuccess 
    else do
        hPutStrLn stderr "Validation failed ✗"
        exitFailure
