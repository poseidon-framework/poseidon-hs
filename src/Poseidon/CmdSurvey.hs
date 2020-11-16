{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdSurvey (runSurvey, SurveyOptions(..)) where

import           Poseidon.Package   (PoseidonPackage(..),
                                    loadPoseidonPackages,
                                    loadJannoFiles,
                                    PoseidonSample(..))
import qualified Data.Either        as E
import           Data.Maybe         (isNothing)

-- | A datatype representing command line options for the survey command
data SurveyOptions = SurveyOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runSurvey :: SurveyOptions -> IO ()
runSurvey (SurveyOptions baseDirs) = do
    packages <- loadPoseidonPackages baseDirs
    -- putStrLn $ show (length packages) ++ " Poseidon packages found"
    let jannoFilePaths = map posPacJannoFile packages
    let packageNames = map posPacTitle packages
    jannoSamples <- loadJannoFiles jannoFilePaths
    -- putStrLn $ replicate 46 ' ' ++ ['a' .. 'z'] ++ ['A' .. 'I']
    mapM_ (putStrLn . renderPackageWithCompleteness) (zip packageNames (map E.rights jannoSamples))

renderPackageWithCompleteness :: (String,[PoseidonSample]) -> String
renderPackageWithCompleteness (packageName,jannoSamples) =
    take 45 (packageName ++ repeat ' ') ++ " " ++ renderPackageCompleteness jannoSamples

renderPackageCompleteness :: [PoseidonSample] -> String
renderPackageCompleteness jS =
    "M" ++
    allNothing posSamCollectionID jS ++
    allNothing posSamSourceTissue jS ++
    allNothing posSamCountry jS ++
    allNothing posSamLocation jS ++
    allNothing posSamSite jS ++
    allNothing posSamLatitude jS ++
    allNothing posSamLongitude jS ++
    allNothing posSamDateC14Labnr jS ++
    allNothing posSamDateC14UncalBP jS ++
    allNothing posSamDateC14UncalBPErr jS ++
    allNothing posSamDateBCADMedian jS ++
    allNothing posSamDateBCADStart jS ++
    allNothing posSamDateBCADStop jS ++
    allNothing posSamDateType jS ++
    allNothing posSamNrLibraries jS ++
    allNothing posSamDataType jS ++
    allNothing posSamGenotypePloidy jS ++
    "M" ++
    "M" ++
    allNothing posSamNrAutosomalSNPs jS ++
    allNothing posSamCoverage1240K jS ++
    allNothing posSamMTHaplogroup jS ++
    allNothing posSamYHaplogroup jS ++
    allNothing posSamEndogenous jS ++
    allNothing posSamUDG jS ++
    allNothing posSamDamage jS ++
    allNothing posSamNuclearContam jS ++
    allNothing posSamNuclearContamErr jS ++
    allNothing posSamMTContam jS ++
    allNothing posSamMTContamErr jS ++
    allNothing posSamPrimaryContact jS ++
    allNothing posSamPublication jS ++
    allNothing posSamComments jS ++
    allNothing posSamKeywords jS

allNothing :: (PoseidonSample -> Maybe a) -> [PoseidonSample] -> String
allNothing column jannoSamples =
    if all (isNothing . column) jannoSamples
        then "."
        else "X"