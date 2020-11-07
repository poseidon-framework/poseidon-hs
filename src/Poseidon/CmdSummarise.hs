{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdSummarise (runSummarise, SummariseOptions(..)) where

import           Poseidon.Package   (loadPoseidonPackages,
                                    loadJannoFiles,
                                    PoseidonPackage(..), 
                                    PoseidonSample(..))
import qualified Data.List          as L
import qualified Data.Maybe         as DM
import           System.IO          (hPutStrLn, stderr)

-- | A datatype representing command line options for the janno command
data SummariseOptions = SummariseOptions
    { _jaBaseDirs  :: [FilePath]
    }

pasteFirst3 :: [String] -> String
pasteFirst3 [] = "no values"
pasteFirst3 xs = 
    (L.intercalate ", " $ take 3 xs) ++ if (length xs > 3) then ", ..." else ""

removeNothing :: [Maybe a] -> [a]
removeNothing xs =
    let onlyJust = filter DM.isJust xs
    in DM.catMaybes onlyJust

summarisePoseidonSamples :: [PoseidonSample] -> IO ()
summarisePoseidonSamples xs = do
    putStrLn $ "Number of samples: " ++ (show $ length xs)
    putStrLn $ "Individuals: " ++ pasteFirst3 (map posSamIndividualID xs)
    putStrLn $ "Populations: " ++ pasteFirst3 (map head (map posSamGroupName xs))
    putStrLn $ "Countries: " ++ pasteFirst3 (removeNothing (map posSamCountry xs))

-- | The main function running the janno command
runSummarise :: SummariseOptions -> IO ()
runSummarise (SummariseOptions baseDirs) = do 
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    let jannoFilePaths = map posPacJannoFile packages
    let jannoFiles = loadJannoFiles jannoFilePaths
    jannoSamples <- fmap concat jannoFiles
    summarisePoseidonSamples jannoSamples
