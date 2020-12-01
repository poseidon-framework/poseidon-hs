{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate (runValidate, ValidateOptions(..)) where

import           Poseidon.Package   (getIndividuals,
                                     loadPoseidonPackages, 
                                     maybeLoadJannoFiles, 
                                     maybeLoadBibTeXFiles)
import           Poseidon.Utils     (PoseidonException(..),
                                    renderPoseidonException)
import           Poseidon.Janno     (PoseidonSample(..))

import           Control.Monad      (when, unless)
import qualified Data.Either        as E
import           Data.Maybe         (mapMaybe)
import           Text.CSL.Reference (refId, unLiteral)
import           Data.List          (nub, (\\), intercalate)
import           Data.Text          (unpack)
import           Control.Exception  (throw)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..))
import           System.IO          (hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs  :: [FilePath]
      --_ignoreGeno  :: Bool
      --_ignoreJanno :: Bool,
      --_ignoreBib   :: Bool
    }

u :: String -> String
u x = "\x1b[4m" ++ x ++ "\x1b[0m"

-- TODO: Modularize validator!

-- | The main function running the janno command
runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs) = do
    -- POSEIDON.yml
    putStrLn $ u "POSEIDON.yml file consistency:"
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " valid POSEIDON.yml files found"
    -- Genotype file consistency (without loading them completely!)
    putStrLn $ u "Genotype data consistency:"
    indEntries <- mapM getIndividuals packages
    let allIndEntries = concat indEntries
    putStrLn $ show (length allIndEntries)
        ++ " (superficially) valid genotype data entries found"
    -- janno
    putStrLn $ u ".janno file consistency:"
    jannoFiles <- maybeLoadJannoFiles packages
    let jannoFileExistenceExceptions = E.lefts jannoFiles
        jannoSamplesRaw = E.rights jannoFiles
        jannoFileReadingExceptions = E.lefts $ concat jannoSamplesRaw
        jannoSamples = map E.rights jannoSamplesRaw
        allJannoSamples = concat jannoSamples
    mapM_ (putStrLn . renderPoseidonException) jannoFileExistenceExceptions
    mapM_ (putStrLn . renderPoseidonException) jannoFileReadingExceptions
    putStrLn $ show (length allJannoSamples) 
        ++ " valid context data entries found"
    -- bib
    putStrLn $ u ".bib file consistency:"
    bibFiles <- maybeLoadBibTeXFiles packages
    let bibFileExceptions = E.lefts bibFiles
        bibReferences = E.rights bibFiles
        allbibReferences = concat bibReferences
    mapM_ (putStrLn . renderPoseidonException) bibFileExceptions
    putStrLn $ show (length allbibReferences) 
        ++ " valid literature references found"
    -- Cross-file consistency
    -- janno + genotype
    putStrLn $ u ".janno-Genotype data interaction:"
    indEntries <- mapM getIndividuals packages
    let allIndEntries = concat indEntries
    let genoIDs         = [ x | EigenstratIndEntry  x _ _ <- allIndEntries]
        genoSexs        = [ x | EigenstratIndEntry  _ x _ <- allIndEntries]
        genoGroups      = [ x | EigenstratIndEntry  _ _ x <- allIndEntries]
    let jannoIDs        = map posSamIndividualID allJannoSamples
        jannoSexs       = map posSamGeneticSex allJannoSamples
        jannoGroups     = map (head . posSamGroupName) allJannoSamples
    let idMis           = genoIDs /= jannoIDs
        sexMis          = genoSexs /= jannoSexs
        groupMis        = genoGroups /= jannoGroups
        anyJannoGenoMis = idMis || sexMis || groupMis
    when idMis $ putStrLn $ 
        "Individual ID mismatch between genotype data (left) and .janno files (right):\n" ++
        renderMismatch genoIDs jannoIDs
    when sexMis $ putStrLn $ 
        "Individual Sex mismatch between genotype data (left) and .janno files (right):\n" ++
        renderMismatch (map show genoSexs) (map show jannoSexs)
    when groupMis $ putStrLn $ 
        "Individual GroupID mismatch between genotype data (left) and .janno files (right):\n" ++
        renderMismatch genoGroups jannoGroups
    unless anyJannoGenoMis $ putStrLn "All main IDs in the .janno files match the genotype data"
    -- janno + bib
    putStrLn $ u ".janno-.bib interaction:"
    let literatureInJanno = nub $ mapMaybe posSamPublication allJannoSamples
        literatureInBib = nub $ map (unpack . unLiteral . refId) allbibReferences
        literatureNotInBibButInJanno = literatureInJanno \\ literatureInBib
    if null literatureNotInBibButInJanno
    then putStrLn "All literature in the .janno files has BibTeX entries"
    else putStrLn $ "The following papers lack BibTeX entries: " ++
        intercalate ", " literatureNotInBibButInJanno
    -- Final report: Error code generation
    putStrLn ""
    if not (null jannoFileReadingExceptions)
        || not (null bibFileExceptions) 
        || anyJannoGenoMis
    then throw PoseidonValidationException
    else putStrLn "==> Validation passed ✓"


renderMismatch :: [String] -> [String] -> String
renderMismatch a b = 
    let misMatchList = map (\ (x, y) -> "(" ++ x ++ " = " ++ y ++ ")")
                       (filter (\ (x, y) -> x /= y) $ zipWithPadding "?" "?" a b)
    in if length misMatchList > 10 
       then intercalate "\n" (take 10 misMatchList) ++ "\n..."
       else intercalate "\n" misMatchList

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)