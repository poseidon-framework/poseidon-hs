{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdValidate (runValidate, ValidateOptions(..)) where

import           Poseidon.Package   (loadPoseidonPackages,
                                    loadJannoFiles,
                                    PoseidonPackage(..),
                                    loadBibTeXFiles)
import           Poseidon.Utils     (printPoseidonJannoException)

import qualified Data.Either        as E
import           Data.Maybe         (catMaybes)
import           System.IO          (hPutStrLn, stderr)
import           Text.CSL.Exception (renderError)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs) = do
    --
    putStrLn "POSEIDON.yml file consistency:"
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) 
        ++ " Poseidon packages seem to be fine"
    --
    putStrLn "JANNO file consistency:"
    let jannoFilePaths = map posPacJannoFile packages
    let jannoFiles = loadJannoFiles jannoFilePaths
    jannoSamples <- fmap concat jannoFiles
    mapM_ printPoseidonJannoException (E.lefts jannoSamples)
    hPutStrLn stderr $ (show . length $ E.rights jannoSamples) 
        ++ " samples seem to be fine"
    --
    -- Genotype file consistency (if available and without loading them completely!)
    -- ...
    -- 
    putStrLn "BIBTEX file consistency:"
    let bibFilePaths = catMaybes $ map posPacBibFile packages
    let bibFiles = loadBibTeXFiles bibFilePaths
    bibReferences <- fmap (concat . E.rights) bibFiles
    bibExceptions <- fmap E.lefts bibFiles
    mapM_ (putStrLn . renderError) bibExceptions
    hPutStrLn stderr $ (show . length $ bibReferences) 
        ++ " literature references seem to be fine"
    --
    -- Cross-file consistency
    -- ...
    --
    -- Final report: Error code generation
    -- ...
