{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Survey where

import           Poseidon.BibFile          (BibTeX)
import           Poseidon.GenotypeData     (GenotypeDataSpec (..))
import           Poseidon.Janno            (JannoRow (..), JannoRows (..))
import           Poseidon.Package          (PackageReadOptions (..),
                                            PoseidonPackage (..),
                                            defaultPackageReadOptions,
                                            readPoseidonPackageCollection)
import           Poseidon.Utils            (PoseidonIO, logInfo)

import           Control.Monad             (forM)
import           Control.Monad.IO.Class    (liftIO)
import           Data.List                 (intercalate, unfoldr, zip5)
import           Data.Maybe                (isJust)
import           Data.Ratio                (Ratio, (%))
import           Poseidon.SequencingSource (SeqSourceRows (..))
import           System.Directory          (doesFileExist)
import           System.FilePath           ((</>))
import           Text.Layout.Table         (asciiRoundS, column, def,
                                            expandUntil, rowsG, tableString,
                                            titlesH)

-- | A datatype representing command line options for the survey command
data SurveyOptions = SurveyOptions
    { _surveyBaseDirs  :: [FilePath]
    , _surveyRawOutput :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

-- | The main function running the janno command
runSurvey :: SurveyOptions -> PoseidonIO ()
runSurvey (SurveyOptions baseDirs rawOutput) = do

    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    -- collect information
    let packageNames = map posPacTitle allPackages
    -- geno
    let genotypeDataTuples = [(posPacBaseDir pac, posPacGenotypeData pac) | pac <- allPackages]
    genoFilesExist <- liftIO $ sequence [doesFileExist (d </> genoFile gd) | (d, gd) <- genotypeDataTuples]
    snpFilesExist  <- liftIO $ sequence [doesFileExist (d </> snpFile gd) | (d, gd) <- genotypeDataTuples]
    indFilesExist  <- liftIO $ sequence [doesFileExist (d </> indFile gd) | (d, gd) <- genotypeDataTuples]
    let genoTypeDataExists = map (\(a,b,c) -> a && b && c) $ zip3 genoFilesExist snpFilesExist indFilesExist
    -- janno
    let jannos = map posPacJanno allPackages
    -- ssf
    let ssfs = map posPacSeqSource allPackages
    -- bib
    let bibs = map posPacBib allPackages
    -- print information
    (tableH, tableB) <- do
        let tableH = ["Package", "Survey"]
        tableB <- forM (zip5 packageNames genoTypeDataExists jannos ssfs bibs) $ \(p, g, j, s, b) -> do
            return [p, renderPackageWithCompleteness p g j s b]
        return (tableH, tableB)
    let colSpecs = replicate 2 (column (expandUntil 60) def def def)
    if rawOutput
    then liftIO $ putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else liftIO $ putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
    -- print help
    logInfo "see trident survey -h for a list of column names"

renderPackageWithCompleteness :: String -> Bool -> JannoRows -> SeqSourceRows -> BibTeX -> String
renderPackageWithCompleteness _ genoTypeDataExists janno (SeqSourceRows seqSource) bib =
       (if genoTypeDataExists then "G" else ".")
    ++ (if not (null seqSource) then "S" else ".")
    ++ (if not (null bib) then "B" else ".")
    ++ "|"
    ++ insertEveryN 5 '|' (renderJannoCompleteness janno)
    where
        -- https://stackoverflow.com/questions/12659562/insert-specific-element-y-after-every-n-elements-in-a-list
        insertEveryN :: Int -> a -> [a] -> [a]
        insertEveryN n y xs = intercalate [y] . groups n $ xs
            where groups n_ xs_ = takeWhile (not . null) . unfoldr (Just . splitAt n_) $ xs_

-- this has to be in the same order as jannoHeader in the janno module
renderJannoCompleteness :: JannoRows -> String
renderJannoCompleteness jS =
      '█'
    : '█'
    : '█'
    : getColChar jS jAlternativeIDs
    : getColChar jS jRelationTo
    : getColChar jS jRelationDegree
    : getColChar jS jRelationType
    : getColChar jS jRelationNote
    : getColChar jS jCollectionID
    : getColChar jS jCountry
    : getColChar jS jCountryISO
    : getColChar jS jLocation
    : getColChar jS jSite
    : getColChar jS jLatitude
    : getColChar jS jLongitude
    : getColChar jS jDateType
    : getColChar jS jDateC14Labnr
    : getColChar jS jDateC14UncalBP
    : getColChar jS jDateC14UncalBPErr
    : getColChar jS jDateBCADStart
    : getColChar jS jDateBCADMedian
    : getColChar jS jDateBCADStop
    : getColChar jS jDateNote
    : getColChar jS jMTHaplogroup
    : getColChar jS jYHaplogroup
    : getColChar jS jSourceTissue
    : getColChar jS jNrLibraries
    : getColChar jS jLibraryNames
    : getColChar jS jCaptureType
    : getColChar jS jUDG
    : getColChar jS jLibraryBuilt
    : getColChar jS jGenotypePloidy
    : getColChar jS jDataPreparationPipelineURL
    : getColChar jS jEndogenous
    : getColChar jS jNrSNPs
    : getColChar jS jCoverageOnTargets
    : getColChar jS jDamage
    : getColChar jS jContamination
    : getColChar jS jContaminationErr
    : getColChar jS jContaminationMeas
    : getColChar jS jContaminationNote
    : getColChar jS jGeneticSourceAccessionIDs
    : getColChar jS jPrimaryContact
    : getColChar jS jPublication
    : getColChar jS jComments
    : getColChar jS jKeywords
    : ""
    where
        getColChar :: JannoRows -> (JannoRow -> Maybe a) -> Char
        getColChar (JannoRows rows) column_ =
            let nrRows = length rows
                nrFilledValues = length $ filter (isJust . column_) rows
            in prop2Char $ nrFilledValues % nrRows
        prop2Char :: Ratio Int -> Char
        prop2Char r
            | r == 0    = '.'
            | r < 0.25  = '░'
            | r < 0.5   = '▒'
            | r < 1     = '▓'
            | r == 1    = '█'
            | otherwise = '?'
