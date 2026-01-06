module Poseidon.CLI.Summarise where

import           Poseidon.ColumnTypesJanno (JannoDateBCADMedian (..))
import           Poseidon.ColumnTypesUtils (ListColumn (..))
import           Poseidon.Janno            (JannoRow (..), JannoRows (..))
import           Poseidon.MathHelpers      (meanAndSdInteger, meanAndSdRoundTo)
import           Poseidon.Package          (PackageReadOptions (..),
                                            PoseidonPackage (..),
                                            defaultPackageReadOptions,
                                            readPoseidonPackageCollection)
import           Poseidon.Utils            (PoseidonIO, logInfo, uniquePO)

import           Control.Monad.IO.Class    (liftIO)
import           Data.List                 (group, intercalate, sort, sortBy)
import           Data.Maybe                (mapMaybe)
import           Poseidon.ColumnTypesJanno (JannoCoverageOnTargets (..),
                                            JannoEndogenous (JannoEndogenous),
                                            JannoNrSNPs (..))
import           Text.Layout.Table         (asciiRoundS, column, def,
                                            expandUntil, rowsG, tableString,
                                            titlesH)

-- | A datatype representing command line options for the summarise command
data SummariseOptions = SummariseOptions
    { _summariseBaseDirs  :: [FilePath]
    , _summariseRawOutput :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums      = True
    , _readOptIgnoreGeno           = True
    , _readOptGenoCheck            = False
    , _readOptOnlyLatest           = True
    }

-- | The main function running the janno command
runSummarise :: SummariseOptions -> PoseidonIO ()
runSummarise (SummariseOptions baseDirs rawOutput) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    let jannos = map posPacJanno allPackages
    logInfo "Note that only the latest versions of packages are included in the summary"
    liftIO $ summariseJannoRows (mconcat jannos) rawOutput

-- | A function to print meaningful summary information for a list of poseidon samples
summariseJannoRows :: JannoRows -> Bool -> IO ()
summariseJannoRows (JannoRows rows) rawOutput = do
    (tableH, tableB) <- do
        let tableH = ["Summary", "Value"]
            tableB = [
                ["Nr Samples"
                , show (length rows)],
                ["Samples"
                , paste . sort . map (show . jPoseidonID) $ rows],
                ["Nr Primary Groups"
                , uniqueNumber . map (head . getListColumn . jGroupName) $ rows],
                ["Primary Groups"
                , printFrequencyString ", " . frequency . map (show . head . getListColumn . jGroupName) $ rows],
                ["Nr Publications"
                , uniqueNumber . concatMap getListColumn . mapMaybe jPublication $ rows],
                ["Publications"
                , paste . map show . uniquePO . concatMap getListColumn . mapMaybe jPublication $ rows],
                ["Nr Countries"
                , uniqueNumber . mapMaybe jCountry $ rows],
                ["Countries"
                , printFrequencyMaybeString ", " . frequency . map (fmap show . jCountry) $ rows
                ],
                ["Mean age BC/AD"
                , meanAndSdInteger . map (\(JannoDateBCADMedian x) -> fromIntegral x) . mapMaybe jDateBCADMedian $ rows],
                ["Dating type"
                , printFrequencyMaybe ", " . frequency . map jDateType $ rows],
                ["Sex distribution"
                , printFrequency ", " . frequency . map jGeneticSex $ rows],
                ["MT haplogroups"
                , printFrequencyMaybeString ", " . frequency . map (fmap show . jMTHaplogroup) $ rows],
                ["Y haplogroups"
                , printFrequencyMaybeString ", " . frequency . map (fmap show . jYHaplogroup) $ rows],
                ["% endogenous DNA"
                , meanAndSdRoundTo 2 . map (\(JannoEndogenous x) -> x) . mapMaybe jEndogenous $ rows],
                ["Nr of SNPs"
                , meanAndSdInteger . map fromIntegral . mapMaybe (fmap (\(JannoNrSNPs x) -> x) . jNrSNPs) $ rows],
                ["Coverage on target"
                , meanAndSdRoundTo 2 . mapMaybe (fmap (\(JannoCoverageOnTargets x) -> x) .  jCoverageOnTargets) $ rows],
                ["Library type"
                , printFrequencyMaybe ", " . frequency . map jLibraryBuilt $ rows],
                ["UDG treatment"
                , printFrequencyMaybe ", " . frequency . map jUDG $ rows]
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

uniqueNumber :: Ord a => [a] -> String
uniqueNumber = show . length . uniquePO

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
