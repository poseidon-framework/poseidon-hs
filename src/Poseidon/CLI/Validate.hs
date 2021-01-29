{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate where

import           Poseidon.BibFile           (loadBibTeXFile)
import           Poseidon.Janno             (PoseidonSample (..), loadJannoFile)
import           Poseidon.Package           (PoseidonPackage (..),
                                             getIndividuals,
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..),
                                             renderPoseidonException)

import           Control.Exception          (try)
import           Control.Exception          (throw)
import           Control.Monad              (forM, unless, when)
import qualified Data.Either                as E
import           Data.List                  (intercalate, nub, (\\))
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (unpack)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..))
import           System.IO                  (hPutStrLn, stderr)
import           Text.CSL.Reference         (refId, unLiteral)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs :: [FilePath]
    }

u :: String -> String
u x = "\x1b[4m" ++ x ++ "\x1b[0m"

-- TODO: Modularize validator!

-- | The main function running the janno command
runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs) = do
    -- POSEIDON.yml
    putStrLn $ u "POSEIDON.yml file consistency:"
    allPackages <- readPoseidonPackageCollection False baseDirs
    -- Genotype file consistency (without loading them completely!)
    putStrLn $ u "Genotype data consistency:"
    indEntries <- mapM getIndividuals allPackages
    let allIndEntries = concat indEntries
    putStrLn $ show (length allIndEntries)
        ++ " (superficially) valid genotype data entries found"
    -- janno
    putStrLn $ u ".janno file consistency:"
    eitherJannoFiles <- sequence [(try . loadJannoFile) fn | Just fn <- map posPacJannoFile allPackages]
    let jannoExceptions = E.lefts eitherJannoFiles
        jannoSamplesRaw = E.rights eitherJannoFiles
        jannoFileReadingExceptions = E.lefts $ concat jannoSamplesRaw
        jannoSamples = map E.rights jannoSamplesRaw
        allJannoSamples = concat jannoSamples
    mapM_ (putStrLn . renderPoseidonException) jannoExceptions
    mapM_ (putStrLn . renderPoseidonException) jannoFileReadingExceptions
    putStrLn $ show (length allJannoSamples)
        ++ " valid context data entries found"
    -- bib
    putStrLn $ u ".bib file consistency:"
    eitherBibFiles <- sequence [(try . loadBibTeXFile) fn | Just fn <- map posPacBibFile allPackages]
    let bibFileExceptions = E.lefts eitherBibFiles
        bibReferences = E.rights eitherBibFiles
        allbibReferences = concat bibReferences
    mapM_ (putStrLn . renderPoseidonException) bibFileExceptions
    putStrLn $ show (length allbibReferences)
        ++ " valid literature references found"
    -- Cross-file consistency
    -- janno + genotype
    putStrLn $ u ".janno-Genotype data interaction:"
    indEntries <- mapM getIndividuals allPackages
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
    if not (null jannoFileReadingExceptions) -- + any genotype errors?
    then putStrLn "There are already issues with the .janno files or the genotype data"
    else do
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
    then throw (PoseidonValidationException "")
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
