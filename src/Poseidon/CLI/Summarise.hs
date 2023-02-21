module Poseidon.CLI.Summarise where

import           Poseidon.Janno         (BCADAge (..), JannoList (..),
                                         JannoRow (..), JannoRows (..),
                                         Percent (..))
import           Poseidon.MathHelpers   (meanAndSdInteger, meanAndSdRoundTo)
import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonIO)

import           Control.Monad.IO.Class (liftIO)
import           Data.List              (group, intercalate, nub, sort, sortBy)
import           Data.Maybe             (mapMaybe)
import           Text.Layout.Table      (asciiRoundS, column, def, expandUntil,
                                         rowsG, tableString, titlesH)

-- | A datatype representing command line options for the summarise command
data SummariseOptions = SummariseOptions
    { _summariseBaseDirs  :: [FilePath]
    , _summariseRawOutput :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }


-- | The main function running the janno command
runSummarise :: SummariseOptions -> PoseidonIO ()
runSummarise (SummariseOptions baseDirs rawOutput) = do

    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    let jannos = map posPacJanno allPackages
    liftIO $ summariseJannoRows (mconcat jannos) rawOutput

-- | A function to print meaningful summary information for a list of poseidon samples
summariseJannoRows :: JannoRows -> Bool -> IO ()
summariseJannoRows (JannoRows rows) rawOutput = do
    (tableH, tableB) <- do
        let tableH = ["Summary", "Value"]
            tableB = [
                ["Nr Individuals", show (length rows)],
                ["Individuals", paste $ sort $ map jPoseidonID rows],
                ["Nr Groups", show $ length $ nub $ map jGroupName rows],
                ["Groups", printFrequencyString ", " $ frequency $ map (head . getJannoList . jGroupName) rows],
                ["Nr Publications", show $ length $ nub $ map jPublication rows],
                ["Publications", paste . nub . concatMap getJannoList . mapMaybe jPublication $ rows],
                ["Nr Countries", show $ length $ nub $ map jCountry rows],
                ["Countries", printFrequencyMaybeString ", " $ frequency $ map jCountry rows],
                ["Mean age BC/AD", meanAndSdInteger $ map (\(BCADAge x) -> fromIntegral x) $ mapMaybe jDateBCADMedian rows],
                ["Dating type", printFrequencyMaybe ", " $ frequency $ map jDateType rows],
                ["Sex distribution", printFrequency ", " $ frequency $ map jGeneticSex rows],
                ["MT haplogroups", printFrequencyMaybeString ", " $ frequency $ map jMTHaplogroup rows],
                ["Y haplogroups",printFrequencyMaybeString ", " $ frequency $ map jYHaplogroup rows],
                ["% endogenous human DNA", meanAndSdRoundTo 2 $ map (\(Percent x) -> x) $ mapMaybe jEndogenous rows],
                ["Nr of SNPs", meanAndSdInteger $ map fromIntegral $ mapMaybe jNrSNPs rows],
                ["Coverage on target SNPs", meanAndSdRoundTo 2 $ mapMaybe jCoverageOnTargets rows],
                ["Library type", printFrequencyMaybe ", " $ frequency $ map jLibraryBuilt rows],
                ["UDG treatment", printFrequencyMaybe ", " $ frequency $ map jUDG rows]
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
  | b1 < b2   = GT
  | b1 > b2   = LT
  | b1 == b2  = compare a1 a2
  | otherwise = error "sortTuplesBySndDesc: should never happen"

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
