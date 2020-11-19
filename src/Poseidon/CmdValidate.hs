{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdValidate (runValidate, ValidateOptions(..)) where

import           Poseidon.Package   (loadPoseidonPackages,
                                    maybeLoadJannoFiles,
                                    PoseidonPackage(..),
                                    PoseidonSample(..),
                                    maybeLoadBibTeXFiles)
import           Poseidon.Utils     (PoseidonException(..),
                                    renderPoseidonException)

import qualified Data.Either        as E
import           Data.Maybe         (mapMaybe)
import           System.IO          (hPutStrLn, stderr)
import           Text.CSL.Exception (renderError)
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
    let bibFileExistenceExceptions = E.lefts bibFiles
    let bibReferencesRaw = E.rights bibFiles
    let bibFileReadingExceptions = E.lefts bibReferencesRaw
    let bibReferences = concat $ E.rights bibReferencesRaw
    mapM_ (putStrLn . renderPoseidonException) bibFileExistenceExceptions
    mapM_ (putStrLn . renderError) bibFileReadingExceptions
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
    if not (null jannoFileReadingExceptions && null bibFileExistenceExceptions) -- && ...
        then throw PoseidonValidationException
        else putStrLn "==> Validation passed ✓"
