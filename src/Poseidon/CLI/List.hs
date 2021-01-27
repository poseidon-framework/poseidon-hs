module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..)) where

import           Poseidon.Package          (loadPoseidonPackages,
                                            PoseidonPackage(..),
                                            getIndividuals)

import           Control.Monad             (forM)
import           Data.List                 (groupBy, intercalate, nub, sortOn)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..))
import           System.IO                 (hPutStrLn, stderr)
import           Text.Layout.Table         (asciiRoundS, column, def, expand,
                                            rowsG, tableString, titlesH, expandUntil)

-- | A datatype representing command line options for the list command
data ListOptions = ListOptions
    { _loBaseDirs   :: [FilePath] -- ^ the list of base directories to search for packages
    , _loListEntity :: ListEntity -- ^ what to list 
    , _loRawOutput  :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
    }

-- | A datatype to represent the options what to list
data ListEntity = ListPackages -- ^ list packages
    | ListGroups -- ^ list groups across all found packages, as defined in the Genotype Individual file
    | ListIndividuals -- ^ list individuals across all found packages

-- | The main function running the list command
runList :: ListOptions -> IO ()
runList (ListOptions baseDirs listEntity rawOutput) = do
    packages <- loadPoseidonPackages baseDirs False
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            let tableH = ["Title", "Date", "Nr Individuals"]
            tableB <- forM packages $ \pac -> do
                inds <- getIndividuals pac
                return [posPacTitle pac, showMaybeDate (posPacLastModified pac), show (length inds)]
            return (tableH, tableB)
        ListGroups -> do
            allInds <- fmap concat . forM packages $ \pac -> do
                inds <- getIndividuals pac
                return [[posPacTitle pac, name, pop] | (EigenstratIndEntry name _ pop) <- inds]
            let allIndsSortedByGroup = groupBy (\a b -> a!!2 == b!!2) . sortOn (!!2) $ allInds
                tableB = do
                    indGroup <- allIndsSortedByGroup
                    let packages_ = nub [i!!0 | i <- indGroup]
                    let nrInds = length indGroup
                    return [(indGroup!!0)!!2, intercalate "," packages_, show nrInds]
            let tableH = ["Group", "Packages", "Nr Individuals"]
            return (tableH, tableB)
        ListIndividuals -> do
            tableB <- fmap concat . forM packages $ \pac -> do
                inds <- getIndividuals pac
                return [[posPacTitle pac, name, pop] | (EigenstratIndEntry name _ pop) <- inds]
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


