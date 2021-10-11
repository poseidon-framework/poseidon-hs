{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Survey where

import           Poseidon.BibFile      (BibTeX)
import           Poseidon.GenotypeData (GenotypeDataSpec (..))
import           Poseidon.Janno        (JannoRow (..))
import           Poseidon.Package      (PackageReadOptions (..),
                                        PoseidonPackage (..),
                                        defaultPackageReadOptions,
                                        readPoseidonPackageCollection)

import           Control.Monad         (forM)
import           Data.List             (intercalate, zip4)
import           Data.Ratio            (Ratio, (%))
import           Data.Maybe            (isJust)
import           System.Directory      (doesFileExist)
import           System.FilePath       ((</>))
import           Text.Layout.Table     (asciiRoundS, column, def, expandUntil,
                                        rowsG, tableString, titlesH)
-- | A datatype representing command line options for the survey command
data SurveyOptions = SurveyOptions
    { _surveyBaseDirs :: [FilePath]
    , _surveyRawOutput :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

-- | The main function running the janno command
runSurvey :: SurveyOptions -> IO ()
runSurvey (SurveyOptions baseDirs rawOutput) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    -- collect information
    let packageNames = map posPacTitle allPackages
    -- geno
    let genotypeDataTuples = [(posPacBaseDir pac, posPacGenotypeData pac) | pac <- allPackages]
    genoFilesExist <- sequence [doesFileExist (d </> genoFile gd) | (d, gd) <- genotypeDataTuples]
    snpFilesExist <- sequence [doesFileExist (d </> snpFile gd) | (d, gd) <- genotypeDataTuples]
    indFilesExist <- sequence [doesFileExist (d </> indFile gd) | (d, gd) <- genotypeDataTuples]
    let genoTypeDataExists = map (\(a,b,c) -> a && b && c) $ zip3 genoFilesExist snpFilesExist indFilesExist
    -- janno
    let jannos = map posPacJanno allPackages
    -- bib
    let bibs = map posPacBib allPackages
    -- print information
    (tableH, tableB) <- do
        let tableH = ["Package", "Survey"]
        tableB <- forM (zip4 packageNames genoTypeDataExists jannos bibs) $ \pac -> do
            return [extractFirst pac, renderPackageWithCompleteness pac]
        return (tableH, tableB)
    let colSpecs = replicate 2 (column (expandUntil 60) def def def)
    if rawOutput
    then putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

extractFirst :: (a, b, c, d) -> a
extractFirst (a,_,_,_) = a

renderPackageWithCompleteness :: (String, Bool, [JannoRow], BibTeX) -> String
renderPackageWithCompleteness (_,genoTypeDataExists,janno,bib) =
       (if genoTypeDataExists then "G" else ".")
    ++ "-"
    ++ renderJannoCompleteness janno
    ++ "-"
    ++ (if not (null bib) then "B" else ".")

renderJannoCompleteness :: [JannoRow] -> String
renderJannoCompleteness jS =
    "M"
    ++ getColString jCollectionID jS
    ++ getColString jSourceTissue jS
    ++ getColString jCountry jS
    ++ getColString jLocation jS
    ++ getColString jSite jS
    ++ getColString jLatitude jS
    ++ getColString jLongitude jS
    ++ getColString jDateC14Labnr jS
    ++ getColString jDateC14UncalBP jS
    ++ getColString jDateC14UncalBPErr jS
    ++ getColString jDateBCADMedian jS
    ++ getColString jDateBCADStart jS
    ++ getColString jDateBCADStop jS
    ++ getColString jDateType jS
    ++ getColString jNrLibraries jS
    ++ getColString jDataType jS
    ++ getColString jGenotypePloidy jS
    ++ "M"
    ++ "M"
    ++ getColString jNrAutosomalSNPs jS
    ++ getColString jCoverage1240K jS
    ++ getColString jMTHaplogroup jS
    ++ getColString jYHaplogroup jS
    ++ getColString jEndogenous jS
    ++ getColString jUDG jS
    ++ getColString jDamage jS
    ++ getColString jNuclearContam jS
    ++ getColString jNuclearContamErr jS
    ++ getColString jMTContam jS
    ++ getColString jMTContamErr jS
    ++ getColString jGeneticSourceAccessionIDs jS
    ++ getColString jDataPreparationPipelineURL jS
    ++ getColString jPrimaryContact jS
    ++ getColString jPublication jS
    ++ getColString jComments jS
    ++ getColString jKeywords jS
    where
        nrRows = length jS
        getColString :: (JannoRow -> Maybe a) -> [JannoRow] -> String
        getColString column_ jannoRows =
             let nrFilledValues = length $ filter (isJust . column_) jannoRows
             in prop2String $ nrFilledValues % nrRows
        prop2String :: Ratio Int -> String
        prop2String r
            | r == 0    = "."
            | r < 0.25  = "░"
            | r < 0.5   = "▒"
            | r < 1     = "▓"
            | r == 1    = "█"
            | otherwise = "?"
