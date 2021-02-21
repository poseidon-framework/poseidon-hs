{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Summarise where

import           Poseidon.Janno         (Percent (..), PoseidonSample (..))
import           Poseidon.MathHelpers   (meanAndSdRoundTo, meanAndSdInteger)
import           Poseidon.Package       (PoseidonPackage(..), readPoseidonPackageCollection)

import           Control.Monad          (when)
import           Data.List              (sortBy, nub, group, sort, intercalate)
import           Data.Maybe             (catMaybes, isJust, mapMaybe, fromJust)
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
    allPackages <- readPoseidonPackageCollection False True baseDirs
    let jannos = map posPacJanno allPackages
    summarisePoseidonSamples (concat jannos) rawOutput

-- | A function to print meaningful summary information for a list of poseidon samples
summarisePoseidonSamples :: [PoseidonSample] -> Bool -> IO ()
summarisePoseidonSamples xs rawOutput = do
    (tableH, tableB) <- do
        let tableH = ["Summary", "Value"]
            tableB = [
                ["Nr Individuals", show (length xs)],
                ["Individuals", paste $ sort $ map posSamIndividualID xs],
                ["Nr Groups", show $ length $ nub $ map posSamGroupName xs],
                ["Groups", printFrequencyString ", " $ frequency $ map (head . posSamGroupName) xs],
                ["Nr Publications", show $ length $ nub $ map posSamPublication xs],
                ["Publications", printFrequencyMaybeString ", " $ frequency $ map posSamPublication xs],
                ["Nr Countries", show $ length $ nub $ map posSamCountry xs],
                ["Countries", printFrequencyMaybeString ", " $ frequency $ map posSamCountry xs],
                ["Mean age BC/AD", meanAndSdInteger $ map fromIntegral $ mapMaybe posSamDateBCADMedian xs],
                ["Dating type", printFrequencyMaybe ", " $ frequency $ map posSamDateType xs],
                ["Sex distribution", printFrequency ", " $ frequency $ map posSamGeneticSex xs],
                ["MT haplogroups", printFrequencyMaybeString ", " $ frequency $ map posSamMTHaplogroup xs],
                ["Y haplogroups",printFrequencyMaybeString ", " $ frequency $ map posSamYHaplogroup xs],
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
frequency list = sortBy sortTupelsBySndDesc $ map (\l -> (head l, length l)) (group (sort list))

sortTupelsBySndDesc :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
sortTupelsBySndDesc (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2

-- | A helper function to print the output of frequency nicely
printFrequency :: Show a => String -> [(a,Int)] -> String
printFrequency _ [] = "no values"
printFrequency _ [x] = show (fst x) ++ ": " ++ show (snd x)
printFrequency sep (x:xs) = show (fst x) ++ ": " ++ show (snd x) ++ sep ++ printFrequency sep xs

-- | A helper function to print the output of frequency over Maybe values nicely
printFrequencyMaybe :: Show a => String -> [(Maybe a,Int)] -> String
printFrequencyMaybe _ [] = "no values"
printFrequencyMaybe _ [x] = maybeShow (fst x) ++ ": " ++ show (snd x)
printFrequencyMaybe sep (x:xs) = maybeShow (fst x) ++ ": " ++ show (snd x) ++ sep ++ printFrequencyMaybe sep xs

-- | A helper function to unwrap a maybe
maybeShow :: Show a => Maybe a -> String
maybeShow (Just x) = show x
maybeShow Nothing  = "n/a"

-- | As printFrequency, but without additional quoting of strings
printFrequencyString :: String -> [(String,Int)] -> String
printFrequencyString _ [] = "no values"
printFrequencyString _ [x] = fst x ++ ": " ++ show (snd x)
printFrequencyString sep (x:xs) = fst x ++ ": " ++ show (snd x) ++ sep ++ printFrequencyString sep xs

-- | As printFrequencyMaybe, but without additional quoting of strings
printFrequencyMaybeString :: String -> [(Maybe String,Int)] -> String
printFrequencyMaybeString _ [] = "no values"
printFrequencyMaybeString _ [x] = maybeShowString (fst x) ++ ": " ++ show (snd x)
printFrequencyMaybeString sep (x:xs) = maybeShowString (fst x) ++ ": " ++ show (snd x) ++ sep ++ printFrequencyMaybeString sep xs

-- | As maybeShow, but without additional quoting of strings
maybeShowString :: Maybe String -> String
maybeShowString (Just x) = x
maybeShowString Nothing  = "n/a"
