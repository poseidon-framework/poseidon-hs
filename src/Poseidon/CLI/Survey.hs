{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Survey where

import           Poseidon.GenotypeData (GenotypeDataSpec (..))
import           Poseidon.Janno        (JannoRow (..))
import           Poseidon.Package      (PoseidonPackage (..),
                                        readPoseidonPackageCollection,
                                        PackageReadOptions (..), defaultPackageReadOptions)
import           Poseidon.BibFile      (BibTeX)

import           Control.Monad         (forM)
import           Data.List             (zip4, intercalate)
import           Data.Maybe            (isNothing)
import           System.Directory      (doesFileExist)
import           System.FilePath.Posix ((</>))
import           Text.Layout.Table     (asciiRoundS, column, def,
                                        rowsG, tableString, titlesH, expandUntil)
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
    -- -- bib
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
    ++ allNothing jCollectionID jS
    ++ allNothing jSourceTissue jS
    ++ allNothing jCountry jS
    ++ allNothing jLocation jS
    ++ allNothing jSite jS
    ++ allNothing jLatitude jS
    ++ allNothing jLongitude jS
    ++ allNothing jDateC14Labnr jS
    ++ allNothing jDateC14UncalBP jS
    ++ allNothing jDateC14UncalBPErr jS
    ++ allNothing jDateBCADMedian jS
    ++ allNothing jDateBCADStart jS
    ++ allNothing jDateBCADStop jS
    ++ allNothing jDateType jS
    ++ allNothing jNrLibraries jS
    ++ allNothing jDataType jS
    ++ allNothing jGenotypePloidy jS
    ++ "M"
    ++ "M"
    ++ allNothing jNrAutosomalSNPs jS
    ++ allNothing jCoverage1240K jS
    ++ allNothing jMTHaplogroup jS
    ++ allNothing jYHaplogroup jS
    ++ allNothing jEndogenous jS
    ++ allNothing jUDG jS
    ++ allNothing jDamage jS
    ++ allNothing jNuclearContam jS
    ++ allNothing jNuclearContamErr jS
    ++ allNothing jMTContam jS
    ++ allNothing jMTContamErr jS
    ++ allNothing jGeneticSourceAccessionIDs jS
    ++ allNothing jDataPreparationPipelineURL jS
    ++ allNothing jPrimaryContact jS
    ++ allNothing jPublication jS
    ++ allNothing jComments jS
    ++ allNothing jKeywords jS

allNothing :: (JannoRow -> Maybe a) -> [JannoRow] -> String
allNothing column_ jannoRows =
    if all (isNothing . column_) jannoRows
        then "."
        else "X"
