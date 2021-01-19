{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Summarise where

import           Poseidon.Janno         (Percent (..), PoseidonSample (..), jannoToSimpleMaybeList)
import           Poseidon.MathHelpers   (meanAndSdRoundTo, meanAndSdInteger)
import           Poseidon.Package       (loadPoseidonPackages, maybeLoadJannoFiles)

import           Control.Monad          (when)
import           Data.List              (nub, group, sort, intercalate)
import           Data.Maybe             (catMaybes, isJust, mapMaybe)
import           System.IO              (hPutStrLn, stderr)
import           Text.Layout.Table      (asciiRoundS, column, def, expand,
                                         rowsG, tableString, titlesH, expandUntil)

-- | A datatype representing command line options for the summarise command
data SummariseOptions = SummariseOptions
    { _jaBaseDirs :: [FilePath]
    , _optRawOutput :: Bool
    }

-- | The main function running the janno command
runSummarise :: SummariseOptions -> IO ()
runSummarise (SummariseOptions baseDirs rawOutput) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    -- JANNO
    jannoFiles <- maybeLoadJannoFiles packages
    let jannoMaybeList = jannoToSimpleMaybeList jannoFiles
    let jannoSamples = concat $ catMaybes jannoMaybeList
    let anyJannoIssues = not $ all isJust jannoMaybeList
    -- print information
    summarisePoseidonSamples jannoSamples rawOutput
    -- print read issue warning
    when anyJannoIssues $
        hPutStrLn stderr "\nThere were issues with missing, incomplete or invalid data. Run trident validate to learn more."

-- | A function to print meaningful summary information for a list of poseidon samples
summarisePoseidonSamples :: [PoseidonSample] -> Bool -> IO ()
summarisePoseidonSamples xs rawOutput = do
    (tableH, tableB) <- do
        let tableH = ["Summary", "Value"]
            tableB = [
                ["Nr Individuals", show (length xs)],
                ["Individuals", paste $ map posSamIndividualID xs],
                ["Nr Groups", show $ length $ nub $ map posSamGroupName xs],
                ["Groups", paste $ nub $ map (head . posSamGroupName) xs],
                ["Nr Publications", show $ length $ nub $ map posSamPublication xs],
                ["Publications", paste $ nub $ mapMaybe posSamPublication xs],
                ["Nr Countries", show $ length $ nub $ map posSamCountry xs],
                ["Countries", paste $ nub $ mapMaybe posSamCountry xs],
                ["Mean age BC/AD", meanAndSdInteger $ map fromIntegral $ mapMaybe posSamDateBCADMedian xs],
                ["Dating type", printFrequencyMaybe ", " $ frequency $ map posSamDateType xs],
                ["Sex distribution", printFrequency ", " $ frequency $ map posSamGeneticSex xs],
                ["MT haplogroups", paste $ nub $ mapMaybe posSamMTHaplogroup xs],
                ["Y haplogroups", paste $ nub $ mapMaybe posSamYHaplogroup xs],
                ["% endogenous human DNA", meanAndSdRoundTo 2 $ map (\(Percent x) -> x) $ mapMaybe posSamEndogenous xs],
                ["Nr of SNPs on 1240K", meanAndSdInteger $ map fromIntegral $ mapMaybe posSamNrAutosomalSNPs xs],
                ["Coverage on 1240K", meanAndSdRoundTo 2 $ mapMaybe posSamCoverage1240K xs],
                ["Library type", printFrequencyMaybe ", " $ frequency $ map posSamLibraryBuilt xs],
                ["UDG treatment", printFrequencyMaybe ", " $ frequency $ map posSamUDG xs]
                ]
        return (tableH, tableB)
    let colSpecs = replicate 2 (column (expandUntil 60) def def def)
    if rawOutput
    then putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

-- | A helper function to concat the first N elements of a string list in a nice way
paste :: [String] -> String
paste [] = "no values"
paste xs = intercalate ", " xs

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
