{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdSurvey (runSurvey, SurveyOptions(..)) where

import           Poseidon.Package       (PoseidonPackage(..),
                                        loadPoseidonPackages,
                                        loadJannoFiles,
                                        PoseidonSample(..),
                                        GenotypeDataSpec(..))
import qualified Data.Either            as E
import           Data.Maybe             (isNothing)
import           System.Directory       (doesFileExist)
import           Data.List              (zip4)
import           System.IO              (hPutStrLn, stderr)  

-- | A datatype representing command line options for the survey command
data SurveyOptions = SurveyOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runSurvey :: SurveyOptions -> IO ()
runSurvey (SurveyOptions baseDirs) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    -- collect information
    let packageNames = map posPacTitle packages
    -- geno
    let genotypeData = map posPacGenotypeData packages
    genoFilesExist <- mapM (doesFileExist . genoFile) genotypeData 
    snpFilesExist <- mapM (doesFileExist . snpFile) genotypeData 
    indFilesExist <- mapM (doesFileExist . indFile) genotypeData 
    let genoTypeDataExists = map (\(a,b,c) -> a && b && c) $ zip3 genoFilesExist snpFilesExist indFilesExist
    -- janno
    let jannoFilePaths = map posPacJannoFile packages
    jannoSamples <- loadJannoFiles jannoFilePaths
    -- bib
    let bibLinkInDescription = map posPacBibFile packages
    bibExists <- mapM (maybe (return False) doesFileExist) bibLinkInDescription
    -- print information
    mapM_ 
        (putStrLn . renderPackageWithCompleteness) 
        (zip4 packageNames genoTypeDataExists (map E.rights jannoSamples) bibExists)

renderPackageWithCompleteness :: (String,Bool,[PoseidonSample],Bool) -> String
renderPackageWithCompleteness (packageName,genoTypeDataExists,jannoSamples,bibExists) =
    take 40 (packageName ++ repeat ' ') 
    ++ " "
    ++ if genoTypeDataExists then "G" else "."
    ++ "-"
    ++ renderJannoCompleteness jannoSamples
    ++ "-"
    ++ if bibExists then "B" else "."

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