{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdValidate (runValidate, ValidateOptions(..)) where

import           Poseidon.Package   (loadPoseidonPackages,
                                    loadJannoFiles,
                                    PoseidonPackage(..))
import           Poseidon.Utils     (printPoseidonJannoException)
import           System.IO          (hPutStrLn, stderr)
import qualified Data.Either        as E

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    -- POSEIDON.yml consistency
    -- ...
    -- Janno file consistency
    let jannoFilePaths = map posPacJannoFile packages
    let jannoFiles = loadJannoFiles jannoFilePaths
    jannoSamples <- fmap concat jannoFiles
    mapM_ printPoseidonJannoException (E.lefts jannoSamples)
    -- Genotype file consistency (if available and without loading them completely!)
    -- ...
    -- Bibtex file consistency
    -- ...
    -- Cross-file consistency
    -- ...
    -- Final report: Error code generation
    -- ...

