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
import           System.IO          (hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs  :: [FilePath]
    }

u :: String -> String
u x = "\x1b[4m" ++ x ++ "\x1b[0m"

-- | The main function running the janno command
runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs) = do
    -- POSEIDON.yml
    putStrLn $ u "POSEIDON.yml file consistency:"
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " valid POSEIDON.yml files found"
    -- janno
    putStrLn $ u ".janno file consistency:"
    jannoFiles <- maybeLoadJannoFiles packages
    let jannoFileExistenceExceptions = E.lefts jannoFiles
    let jannoSamplesRaw = E.rights jannoFiles
    let jannoFileReadingExceptions = E.lefts $ concat jannoSamplesRaw
    let jannoSamples = E.rights $ concat jannoSamplesRaw
    mapM_ (putStrLn . renderPoseidonException) jannoFileExistenceExceptions
    mapM_ (putStrLn . renderPoseidonException) jannoFileReadingExceptions
    putStrLn $ show (length jannoSamples) 
        ++ " valid samples found"
    -- bib
    putStrLn $ u ".bib file consistency:"
    bibFiles <- maybeLoadBibTeXFiles packages
    let bibFileExceptions = E.lefts bibFiles
    let bibReferences = concat $ E.rights bibFiles
    mapM_ (putStrLn . renderPoseidonException) bibFileExceptions
    putStrLn $ show (length bibReferences) 
        ++ " valid literature references found"
    -- Genotype file consistency (without loading them completely!)
    putStrLn $ u "Genotype data consistency:"
    putStrLn "not tested so far"
    -- Cross-file consistency
    -- janno + bib
    putStrLn $ u ".janno-.bib interaction:"
    let literatureInJanno = nub $ mapMaybe posSamPublication jannoSamples
    let literatureInBib = nub $ map (unpack . unLiteral . refId) bibReferences
    let literatureNotInBibButInJanno = literatureInJanno \\ literatureInBib
    if null literatureNotInBibButInJanno
        then putStrLn "All literature in the .janno files has BibTeX entries"
        else putStrLn $ "The following papers lack BibTeX entries: " ++ 
                        intercalate ", " literatureNotInBibButInJanno
    -- janno - genotype
    putStrLn $ u ".janno-Genotype data interaction:"
    putStrLn "not tested so far"
    -- Final report: Error code generation
    putStrLn ""
    if not (null jannoFileReadingExceptions && null bibFileExceptions) -- && ...
        then throw PoseidonValidationException
        else putStrLn "==> Validation passed ✓"
