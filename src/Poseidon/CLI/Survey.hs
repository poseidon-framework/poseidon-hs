{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Survey where

import           Poseidon.BibFile      (bibToSimpleMaybeList)
import           Poseidon.GenotypeData (GenotypeDataSpec (..))
import           Poseidon.Janno        (PoseidonSample (..))
import           Poseidon.Package      (PoseidonPackage (..),
                                        readPoseidonPackageCollection)


import           Control.Monad         (forM, when)
import           Data.List             (zip4, intercalate)
import           Data.Maybe            (isJust, isNothing)
import           System.Directory      (doesFileExist)
import           System.FilePath.Posix ((</>))
import           System.IO             (hPutStrLn, stderr)
import           Text.Layout.Table     (asciiRoundS, column, def, expand,
                                        rowsG, tableString, titlesH, expandUntil)
-- | A datatype representing command line options for the survey command
data SurveyOptions = SurveyOptions
    { _jaBaseDirs :: [FilePath]
    , _optRawOutput :: Bool
    }

-- | The main function running the janno command
runSurvey :: SurveyOptions -> IO ()
runSurvey (SurveyOptions baseDirs rawOutput) = do
    allPackages <- readPoseidonPackageCollection True baseDirs
    -- collect information
    let packageNames = map posPacTitle allPackages
    -- geno
    let genotypeDataTuples = [(posPacBaseDir pac, posPacGenotypeData pac) | pac <- allPackages]
    genoFilesExist <- sequence [doesFileExist (d </> genoFile gd) | (d, gd) <- genotypeDataTuples]
    snpFilesExist <- sequence [doesFileExist (d </> snpFile gd) | (d, gd) <- genotypeDataTuples]
    indFilesExist <- sequence [doesFileExist (d </> indFile gd) | (d, gd) <- genotypeDataTuples]
    let genoTypeDataExists = map (\(a,b,c) -> a && b && c) $ zip3 genoFilesExist snpFilesExist indFilesExist
    return ()
    -- JANNO
    -- TODO Fix Janno and bib file loading: What happens if they are missing? So far we had exceptions for that, but
    -- I changed that behaviour... sorry
    -- jannoFiles <- maybereadJannoFiles allPackages
    -- let jannoMaybeList = jannoToSimpleMaybeList jannoFiles
    -- let anyJannoIssues = not $ all isJust jannoMaybeList
    -- -- bib
    -- bibFiles <- maybeLoadBibTeXFiles allPackages
    -- let bibMaybeList = bibToSimpleMaybeList bibFiles
    -- let bibAreAlright = map isJust bibMaybeList
    -- let anyBibIssues = not $ all isJust bibMaybeList
    -- print information
    -- (tableH, tableB) <- do
    --     let tableH = ["Package", "Survey"]
    --     tableB <- forM (zip4 packageNames genoTypeDataExists jannoMaybeList bibAreAlright) $ \pac -> do
    --         return [extractFirst pac, renderPackageWithCompleteness pac]
    --     return (tableH, tableB)
    -- let colSpecs = replicate 2 (column (expandUntil 60) def def def)
    -- if rawOutput
    -- then putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    -- else putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
    -- -- print read issue warning
    -- when (anyJannoIssues || anyBibIssues) $
    --     hPutStrLn stderr "\nThere were issues with incomplete, missing or invalid data. Run trident validate to learn more."

extractFirst :: (a, b, c, d) -> a
extractFirst (a,_,_,_) = a

renderPackageWithCompleteness :: (String,Bool,Maybe [PoseidonSample],Bool) -> String
renderPackageWithCompleteness (_,genoTypeDataExists,jannoSamples,bibIsAlright) =
       (if genoTypeDataExists then "G" else ".")
    ++ "-"
    ++ maybe (replicate 35 '.') renderJannoCompleteness jannoSamples
    ++ "-"
    ++ (if bibIsAlright then "B" else ".")

renderJannoCompleteness :: [PoseidonSample] -> String
renderJannoCompleteness jS =
    "M"
    ++ allNothing posSamCollectionID jS
    ++ allNothing posSamSourceTissue jS
    ++ allNothing posSamCountry jS
    ++ allNothing posSamLocation jS
    ++ allNothing posSamSite jS
    ++ allNothing posSamLatitude jS
    ++ allNothing posSamLongitude jS
    ++ allNothing posSamDateC14Labnr jS
    ++ allNothing posSamDateC14UncalBP jS
    ++ allNothing posSamDateC14UncalBPErr jS
    ++ allNothing posSamDateBCADMedian jS
    ++ allNothing posSamDateBCADStart jS
    ++ allNothing posSamDateBCADStop jS
    ++ allNothing posSamDateType jS
    ++ allNothing posSamNrLibraries jS
    ++ allNothing posSamDataType jS
    ++ allNothing posSamGenotypePloidy jS
    ++ "M"
    ++ "M"
    ++ allNothing posSamNrAutosomalSNPs jS
    ++ allNothing posSamCoverage1240K jS
    ++ allNothing posSamMTHaplogroup jS
    ++ allNothing posSamYHaplogroup jS
    ++ allNothing posSamEndogenous jS
    ++ allNothing posSamUDG jS
    ++ allNothing posSamDamage jS
    ++ allNothing posSamNuclearContam jS
    ++ allNothing posSamNuclearContamErr jS
    ++ allNothing posSamMTContam jS
    ++ allNothing posSamMTContamErr jS
    ++ allNothing posSamPrimaryContact jS
    ++ allNothing posSamPublication jS
    ++ allNothing posSamComments jS
    ++ allNothing posSamKeywords jS

allNothing :: (PoseidonSample -> Maybe a) -> [PoseidonSample] -> String
allNothing column jannoSamples =
    if all (isNothing . column) jannoSamples
        then "."
        else "X"
