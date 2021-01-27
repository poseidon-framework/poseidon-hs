{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Summarise where

import           Poseidon.MathHelpers   (meanAndSdRoundTo, meanAndSdInteger)

import           Control.Monad          (when)
import           Data.List              (nub, group, sort, intercalate)
import           Data.Maybe             (catMaybes, isJust, mapMaybe)
import           Poseidon.Janno         (Percent (..), PoseidonSample (..), jannoToSimpleMaybeList)
import           Poseidon.Package       (PoseidonPackageMeta (..),
                                         loadPoseidonPackages, 
                                         maybeLoadJannoFiles)
import           System.IO              (hPutStrLn, stderr)

-- | A datatype representing command line options for the summarise command
data SummariseOptions = SummariseOptions
    { _jaBaseDirs :: [FilePath]
    }

-- | The main function running the janno command
runSummarise :: SummariseOptions -> IO ()
runSummarise (SummariseOptions baseDirs) = do
    allMetaPackages <- loadPoseidonPackages baseDirs False
    let packages = map posPac allMetaPackages
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    -- JANNO
    jannoFiles <- maybeLoadJannoFiles packages
    let jannoMaybeList = jannoToSimpleMaybeList jannoFiles
    let jannoSamples = concat $ catMaybes jannoMaybeList
    let anyJannoIssues = not $ all isJust jannoMaybeList
    -- print information
    summarisePoseidonSamples jannoSamples
    -- print read issue warning
    when anyJannoIssues $
        putStrLn "\nThere were issues with missing, incomplete or invalid data. Run trident validate to learn more."

-- | A function to print meaningful summary information for a list of poseidon samples
summarisePoseidonSamples :: [PoseidonSample] -> IO ()
summarisePoseidonSamples xs = do
    putStrLn "---"
    putStrLn $ "Number of samples:\t" ++
                show (length xs)
    putStrLn $ "Individuals:\t\t" ++
                pasteFirstN 5 (map posSamIndividualID xs)
    putStrLn $ "Sex distribution:\t" ++
                printFrequency ", " (frequency (map posSamGeneticSex xs))
    putStrLn $ "Populations:\t\t" ++
                pasteFirstN 2 (nub $ map (head . posSamGroupName) xs)
    putStrLn $ "Publications:\t\t" ++
                pasteFirstN 2 (nub $ mapMaybe posSamPublication xs)
    putStrLn $ "Countries:\t\t" ++
                pasteFirstN 5 (nub $ mapMaybe posSamCountry xs)
    putStrLn $ "Mean age BC/AD:\t\t" ++
               meanAndSdInteger (map fromIntegral (mapMaybe posSamDateBCADMedian xs))
    putStrLn $ "Dating type:\t\t" ++
                printFrequencyMaybe ", " (frequency (map posSamDateType xs))
    putStrLn "---"
    putStrLn $ "MT haplogroups:\t\t" ++
                pasteFirstN 5 (nub $ mapMaybe posSamMTHaplogroup xs)
    putStrLn $ "Y haplogroups:\t\t" ++
                pasteFirstN 5 (nub $ mapMaybe posSamYHaplogroup xs)
    putStrLn $ "% endogenous human DNA:\t" ++
                meanAndSdRoundTo 2 (map (\(Percent x) -> x) (mapMaybe posSamEndogenous xs))
    putStrLn $ "# of SNPs on 1240K:\t" ++
                meanAndSdInteger (map fromIntegral (mapMaybe posSamNrAutosomalSNPs xs))
    putStrLn $ "Coverage on 1240K:\t" ++
                meanAndSdRoundTo 2 (mapMaybe posSamCoverage1240K xs)
    putStrLn $ "Library type:\t\t" ++
                printFrequencyMaybe ", " (frequency (map posSamLibraryBuilt xs))
    putStrLn $ "UDG treatment:\t\t" ++
                printFrequencyMaybe ", " (frequency (map posSamUDG xs))

-- | A helper function to concat the first N elements of a string list in a nice way
pasteFirstN :: Int -> [String] -> String
pasteFirstN _ [] = "no values"
pasteFirstN n xs = intercalate ", " (take n xs) ++ if length xs > n then ", ..." else ""

-- | A helper function to determine the frequency of objects in a list
-- (similar to the table function in R)
frequency :: Ord a => [a] -> [(a,Int)]
frequency list = map (\l -> (head l, length l)) (group (sort list))

-- | A helper function to print the output of frequency nicely
printFrequency :: Show a => String -> [(a,Int)] -> String
printFrequency _ [] = "no values"
printFrequency _ [x] = show (fst x) ++ ": " ++ show (snd x)
printFrequency sep (x:xs) = show (fst x) ++ ": " ++ show (snd x) ++ sep ++ printFrequency sep xs

-- | A helper function to print the output of frequency over Maybe values nicely
printFrequencyMaybe :: Show a => String -> [(Maybe a,Int)] -> String
printFrequencyMaybe _ [] = "no values"
printFrequencyMaybe _ [(Nothing,_)] = "no values"
printFrequencyMaybe _ [x] = maybeShow (fst x) ++ ": " ++ show (snd x)
printFrequencyMaybe sep (x:xs) = maybeShow (fst x) ++ ": " ++ show (snd x) ++ sep ++ printFrequencyMaybe sep xs

-- | A helper function to unwrap a maybe
maybeShow :: Show a => Maybe a -> String
maybeShow (Just x) = show x
maybeShow Nothing  = "n/a"
