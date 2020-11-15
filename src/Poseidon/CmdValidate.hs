{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdValidate (runValidate, ValidateOptions(..)) where

import           Poseidon.Package   (loadPoseidonPackages,
                                    loadJannoFiles,
                                    PoseidonPackage(..),
                                    PoseidonSample(..),
                                    loadBibTeXFiles)
import           Poseidon.Utils     (PoseidonException(..),
                                    renderPoseidonJannoException)

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
    hPutStrLn stderr $ (show . length $ packages) 
        ++ " Poseidon packages seem to be fine"
    -- JANNO
    putStrLn "JANNO file consistency:"
    let jannoFilePaths = map posPacJannoFile packages
    let jannoFiles = loadJannoFiles jannoFilePaths
    jannoSamplesRaw <- fmap concat jannoFiles
    let jannoSamples = E.rights jannoSamplesRaw
    let jannoExceptions = E.lefts jannoSamplesRaw
    mapM_ (putStrLn . renderPoseidonJannoException) jannoExceptions
    hPutStrLn stderr $ (show . length $ jannoSamples) 
        ++ " samples seem to be fine"
    --
    -- Genotype file consistency (if available and without loading them completely!)
    -- ...
    -- BIBTEX
    putStrLn "BIBTEX file consistency:"
    let bibFilePaths = mapMaybe posPacBibFile packages
    let bibFiles = loadBibTeXFiles bibFilePaths
    bibReferences <- fmap (concat . E.rights) bibFiles
    bibExceptions <- fmap E.lefts bibFiles
    mapM_ (putStrLn . renderError) bibExceptions
    hPutStrLn stderr $ (show . length $ bibReferences) 
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
    if not (null jannoExceptions && null bibExceptions) -- || ...
        then throw PoseidonValidationException
        else putStrLn "==> Validation passed ✓"
