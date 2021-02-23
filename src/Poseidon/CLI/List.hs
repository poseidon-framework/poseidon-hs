module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..)) where

import           Poseidon.Janno             (PoseidonSample (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             getIndividuals,
                                             readPoseidonPackageCollection)

import           Control.Monad              (forM)
import           Data.List                  (groupBy, intercalate, nub, sortOn)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..))
import           System.IO                  (hPutStrLn, stderr)
import           Text.Layout.Table          (asciiRoundS, column, def, expand,
                                             expandUntil, rowsG, tableString,
                                             titlesH)

-- | A datatype representing command line options for the list command
data ListOptions = ListOptions
    { _loBaseDirs    :: [FilePath] -- ^ the list of base directories to search for packages
    -- ^ what to list
    , _loListEntity  :: ListEntity -- ^ what to list
    -- ^ whether to output raw TSV instead of a nicely formatted table
    , _loRawOutput   :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
    , _optIgnoreGeno :: Bool
    }

-- | A datatype to represent the options what to list
data ListEntity = ListPackages
    | ListGroups
    | ListIndividuals

-- | The main function running the list command
runList :: ListOptions -> IO ()
runList (ListOptions baseDirs listEntity rawOutput ignoreGeno) = do
    allPackages <- readPoseidonPackageCollection False ignoreGeno baseDirs
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            let tableH = ["Title", "Date", "Nr Individuals"]
                tableB = do
                    pac <- allPackages
                    let jannoRows = posPacJanno pac
                    return [posPacTitle pac, showMaybeDate (posPacLastModified pac), show (length jannoRows)]
            return (tableH, tableB)
        ListGroups -> do
            let allInds = do
                    pac <- allPackages
                    jannoRow <- posPacJanno pac
                    return [posPacTitle pac, posSamIndividualID jannoRow, head (posSamGroupName jannoRow)]
            let allIndsSortedByGroup = groupBy (\a b -> a!!2 == b!!2) . sortOn (!!2) $ allInds
                tableB = do
                    indGroup <- allIndsSortedByGroup
                    let packages_ = nub [i!!0 | i <- indGroup]
                    let nrInds = length indGroup
                    return [(indGroup!!0)!!2, intercalate "," packages_, show nrInds]
            let tableH = ["Group", "Packages", "Nr Individuals"]
            return (tableH, tableB)
        ListIndividuals -> do
            let tableB = do
                    pac <- allPackages
                    jannoRow <- posPacJanno pac
                    return [posPacTitle pac, posSamIndividualID jannoRow, head (posSamGroupName jannoRow)]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " individuals.")
            let tableH = ["Package", "Individual", "Population"]
            return (tableH, tableB)

    if rawOutput then
        putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else
        case listEntity of
            ListGroups -> do
                let colSpecs = replicate 3 (column (expandUntil 50) def def def)
                putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
            _ -> do
                let colSpecs = replicate 3 (column expand def def def)
                putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    showMaybeDate (Just d) = show d
    showMaybeDate Nothing  = "n/a"


