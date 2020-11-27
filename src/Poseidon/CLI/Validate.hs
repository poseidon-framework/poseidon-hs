{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate (runValidate, ValidateOptions(..)) where

import           Poseidon.Package   (loadPoseidonPackages, maybeLoadJannoFiles, maybeLoadBibTeXFiles)
import           Poseidon.Utils     (PoseidonException(..),
                                    renderPoseidonException)
import           Poseidon.Janno     (PoseidonSample(..))

import qualified Data.Either        as E
import           Data.Maybe         (mapMaybe)
import           Text.CSL.Reference (refId, unLiteral)
import           Data.List          (nub, (\\), intercalate)
import           Data.Text          (unpack)
import           Control.Exception  (throw)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs) = do
    -- POSEIDON.yml
    putStrLn "POSEIDON.yml file consistency:"
    packages <- loadPoseidonPackages baseDirs
    putStrLn $ show (length packages) 
        ++ " Poseidon packages seem to be fine"
    -- JANNO
    putStrLn "JANNO file consistency:"
    jannoFiles <- maybeLoadJannoFiles packages
    let jannoFileExistenceExceptions = E.lefts jannoFiles
    let jannoSamplesRaw = E.rights jannoFiles
    let jannoFileReadingExceptions = E.lefts $ concat jannoSamplesRaw
    let jannoSamples = E.rights $ concat jannoSamplesRaw
    mapM_ (putStrLn . renderPoseidonException) jannoFileExistenceExceptions
    mapM_ (putStrLn . renderPoseidonException) jannoFileReadingExceptions
    putStrLn $ show (length jannoSamples) 
        ++ " samples seem to be fine"
    --
    -- Genotype file consistency (if available and without loading them completely!)
    -- ...
    -- BIBTEX
    putStrLn "BIBTEX file consistency:"
    bibFiles <- maybeLoadBibTeXFiles packages
    let bibFileExceptions = E.lefts bibFiles
    let bibReferences = concat $ E.rights bibFiles
    mapM_ (putStrLn . renderPoseidonException) bibFileExceptions
    putStrLn $ show (length bibReferences) 
        ++ " literature references seem to be fine"
    --
    -- Cross-file consistency
    putStrLn "JANNO-BIBTEX interaction:"
    let literatureInJanno = nub $ mapMaybe posSamPublication jannoSamples
    let literatureInBib = nub $ map (unpack . unLiteral . refId) bibReferences
    let literatureNotInBibButInJanno = literatureInJanno \\ literatureInBib
    if null literatureNotInBibButInJanno
        then putStrLn "All literature in the .janno files has BibTeX entries"
        else putStrLn $ "The following papers lack BibTeX entries: " ++ 
                        intercalate ", " literatureNotInBibButInJanno
    --
    -- Final report: Error code generation
    if not (null jannoFileReadingExceptions && null bibFileExceptions) -- && ...
        then throw PoseidonValidationException
        else putStrLn "==> Validation passed ✓"
