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
    (L.intercalate ", " $ take 2 xs) ++ if (length xs > 2) then ", ..." else ""

removeNothing :: [Maybe a] -> [a]
removeNothing xs =
    let onlyJust = filter DM.isJust xs
    in DM.catMaybes onlyJust

avg :: [Double] -> Double
avg xs = let sum = L.foldl' (+) 0 xs
         in sum / (fromIntegral $ length xs)

roundTo :: Int -> Double -> Double
roundTo n x = (fromIntegral (floor (x * t))) / t
    where t = 10^n

stdev :: [Double] -> Double
stdev xs = sqrt . avg . map ((^2) . (-) (avg xs)) $ xs

meanAndSdRoundTo :: Int -> [Double] -> String
meanAndSdRoundTo n xs = (show $ roundTo n $ avg xs) ++ " ± " ++ (show $ roundTo n $ stdev xs)

meanAndSdInteger :: [Double] -> String
meanAndSdInteger xs = (show $ round $ avg xs) ++ " ± " ++ (show $ round $ stdev xs)

summarisePoseidonSamples :: [PoseidonSample] -> IO ()
summarisePoseidonSamples xs = do
    putStrLn $ "Number of samples:\t" ++ (show $ length xs)
    putStrLn $ "Individuals:\t\t" ++ pasteFirst3 (map posSamIndividualID xs)
    putStrLn $ "Populations:\t\t" ++ pasteFirst3 (L.nub $ map head (map posSamGroupName xs))
    putStrLn $ "Countries:\t\t" ++ pasteFirst3 (L.nub $ removeNothing $ map posSamCountry xs)
    putStrLn $ "Mean age BC/AD:\t\t" ++ meanAndSdInteger (map fromIntegral (removeNothing $ map posSamDateBCADMedian xs))

-- | The main function running the janno command
runSummarise :: SummariseOptions -> IO ()
runSummarise (SummariseOptions baseDirs) = do 
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    let jannoFilePaths = map posPacJannoFile packages
    let jannoFiles = loadJannoFiles jannoFilePaths
    jannoSamples <- fmap concat jannoFiles
    summarisePoseidonSamples jannoSamples
