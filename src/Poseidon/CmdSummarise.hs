{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CmdSummarise (runSummarise, SummariseOptions(..)) where

import           Poseidon.Package   (loadPoseidonPackages,
                                    loadJannoFiles,
                                    PoseidonPackage(..), 
                                    PoseidonSample(..))
import qualified Data.List          as L
import qualified Data.Maybe         as DM
import           System.IO          (hPutStrLn, stderr)

-- | A datatype representing command line options for the summarise command
data SummariseOptions = SummariseOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runSummarise :: SummariseOptions -> IO ()
runSummarise (SummariseOptions baseDirs) = do 
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    putStrLn "---"
    let jannoFilePaths = map posPacJannoFile packages
    let jannoFiles = loadJannoFiles jannoFilePaths
    jannoSamples <- fmap concat jannoFiles
    summarisePoseidonSamples jannoSamples

-- | A function to print meaningful summary information for a list of poseidon samples
summarisePoseidonSamples :: [PoseidonSample] -> IO ()
summarisePoseidonSamples xs = do
    putStrLn $ "Number of samples:\t" ++ 
                (show $ length xs)
    putStrLn $ "Individuals:\t\t" ++ 
                pasteFirstN 4 (map posSamIndividualID xs)
    putStrLn $ "Populations:\t\t" ++ 
                pasteFirstN 2 (L.nub $ map head (map posSamGroupName xs))
    putStrLn $ "Countries:\t\t" ++ 
                pasteFirstN 4 (L.nub $ removeNothing $ map posSamCountry xs)
    putStrLn $ "Mean age BC/AD:\t\t" ++ 
               meanAndSdInteger (map fromIntegral (removeNothing $ map posSamDateBCADMedian xs))
    putStrLn "---"
    putStrLn $ "Sex distribution:\t" ++ 
                printFrequency (frequency (map posSamGeneticSex xs))
    putStrLn $ "% endogenous human DNA:\t" ++ 
                meanAndSdRoundTo 2 (removeNothing $ map posSamEndogenous xs)
    putStrLn $ "# of SNPs on 1240K:\t" ++ 
               meanAndSdInteger (map fromIntegral (removeNothing $ map posSamNrAutosomalSNPs xs))
    putStrLn $ "Coverage on 1240K:\t" ++ 
                meanAndSdRoundTo 2 (removeNothing $ map posSamCoverage1240K xs)
    putStrLn $ "UDG treatment:\t\t" ++ 
                printFrequency (frequency (map posSamUDG xs))
    putStrLn $ "Library type:\t\t" ++ 
                printFrequency (frequency (map posSamLibraryBuilt xs))

-- | A helper function to concat the first N elements of a string list in a nice way
pasteFirstN :: Int -> [String] -> String
pasteFirstN _ [] = "no values"
pasteFirstN n xs = 
    (L.intercalate ", " $ take n xs) ++ if (length xs > n) then ", ..." else ""

-- | A helper function to remove all nothings from a list of maybes
removeNothing :: [Maybe a] -> [a]
removeNothing xs =
    let onlyJust = filter DM.isJust xs
    in DM.catMaybes onlyJust

-- | A helper function to calculate the mean of a list of doubles
avg :: [Double] -> Double
avg xs = let sum = L.foldl' (+) 0 xs
         in sum / (fromIntegral $ length xs)

-- | A helper function to round doubles
roundTo :: Int -> Double -> Double
roundTo n x = (fromIntegral (floor (x * t))) / t
    where t = 10^n

-- | A helper function to calculate the standard deviation of a list of doubles
stdev :: [Double] -> Double
stdev xs = sqrt . avg . map ((^2) . (-) (avg xs)) $ xs

-- | A helper function to get a nice string with mean and sd for a list of doubles
meanAndSdRoundTo :: Int -> [Double] -> String
meanAndSdRoundTo n xs = (show $ roundTo n $ avg xs) ++ " ± " ++ (show $ roundTo n $ stdev xs)

-- | A helper function to get a nice string with mean and sd for a list of doubles 
-- (here rounded to integer)
meanAndSdInteger :: [Double] -> String
meanAndSdInteger xs = (show $ round $ avg xs) ++ " ± " ++ (show $ round $ stdev xs)

-- | A helper function to determine the frequency of objects in a list 
-- (similar to the table function in R)
frequency :: Ord a => [a] -> [(a,Int)] 
frequency list = map (\l -> (head l, length l)) (L.group (L.sort list))

-- | A helper function to print the output of frequency nicely
printFrequency :: Show a => [(a,Int)] -> String
printFrequency [] = ""
printFrequency (x:[]) = show(fst(x)) ++ ": " ++ show(snd(x))
printFrequency (x:xs) = show(fst(x)) ++ ": " ++ show(snd(x)) ++ "\n\t\t\t" ++ printFrequency xs
