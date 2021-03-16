{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..), RepoLocationSpec(..)) where

import           Poseidon.Janno             (JannoRow (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             getIndividuals,
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..))

import           Control.Exception          (throwIO)
import           Control.Monad              (forM, (>=>))
import           Data.Aeson                 (FromJSON, (.:), parseJSON, withObject, eitherDecode')
import qualified Data.ByteString.Lazy       as LB
import           Data.List                  (groupBy, intercalate, nub, sortOn)
import           Network.HTTP.Conduit       (simpleHttp)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..))
import           System.IO                  (hPutStrLn, stderr)
import           Text.Layout.Table          (asciiRoundS, column, def, expand,
                                             expandUntil, rowsG, tableString,
                                             titlesH)

-- | A datatype representing command line options for the list command
data ListOptions = ListOptions
    { _loRepoLocation  :: RepoLocationSpec -- ^ the list of base directories to search for packages
    , _loListEntity    :: ListEntity -- ^ what to list
    , _loRawOutput     :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
    , _optIgnoreGeno   :: Bool
    }

data RepoLocationSpec = RepoLocal [FilePath] | RepoRemote String

-- | A datatype to represent the options what to list
data ListEntity = ListPackages
    | ListGroups
    | ListIndividuals [String]

-- | The main function running the list command
runList :: ListOptions -> IO ()
runList (ListOptions repoLocation listEntity rawOutput ignoreGeno) = do
    allSampleInfo <- case repoLocation of
        RepoRemote remoteURL -> do
            -- load remote samples list
            hPutStrLn stderr "Downloading sample list from remote"
            remoteOverviewJSONByteString <- simpleHttp (remoteURL ++ "/janno_all")
            readSampleInfo remoteOverviewJSONByteString
        RepoLocal baseDirs -> do
            allPackages <- readPoseidonPackageCollection False True ignoreGeno baseDirs
            return [(posPacTitle pac, posPacJanno pac) | pac <- allPackages]
    -- construct output
    hPutStrLn stderr "Preparing output table"
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            let tableH = ["Title", "Nr Individuals"]
                tableB = [[name, show (length rows)] | (name, rows) <- sortOn fst allSampleInfo]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " packages")
            return (tableH, tableB)
        ListGroups -> do
            let tableH = ["Group", "Packages", "Nr Individuals"]
                pacJannoPairs = do
                    (pacName, rows) <- allSampleInfo
                    row <- rows
                    return (pacName, row)
                tableB = do
                    oneGroup <- groupBy (\x y -> (head . jGroupName . snd) x == (head . jGroupName . snd) y) $ 
                        sortOn (head . jGroupName . snd) pacJannoPairs
                    let groupName = head . jGroupName . snd . head $ oneGroup
                        groupPacs = intercalate "," $ nub $ map fst oneGroup
                        groupNrInds = show (length oneGroup)
                    return [groupName, groupPacs, groupNrInds]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " groups/populations")
            return (tableH, tableB)
        ListIndividuals moreJannoColumns -> do
            let tableH = ["Package", "Individual", "Group"] ++ moreJannoColumns
            tableB <- fmap concat . forM allSampleInfo $ \(pacName, rows) ->
                forM rows (\row -> do
                    moreFields <- extractAdditionalFields row moreJannoColumns
                    return ([pacName, jIndividualID row, head (jGroupName row)] ++ moreFields))
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " individuals/samples")
            return (tableH, tableB)
    if rawOutput then
        putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate (length tableH) (column (expandUntil 60) def def def)
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

readSampleInfo :: LB.ByteString -> IO [(String, [JannoRow])]
readSampleInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right sam -> return sam

extractAdditionalFields :: JannoRow -> [String] -> IO [String]
extractAdditionalFields jannoRow = mapM (\f -> extractAdditionalField f jannoRow)

extractAdditionalField :: String -> JannoRow -> IO String
extractAdditionalField "Individual_ID"         = return . jIndividualID
extractAdditionalField "Collection_ID"         = handleMaybe     jCollectionID 
extractAdditionalField "Source_Tissue"         = handleMaybe     (fmap (intercalate ",") . jSourceTissue)
extractAdditionalField "Country"               = handleMaybe     jCountry
extractAdditionalField "Location"              = handleMaybe     jLocation
extractAdditionalField "Site"                  = handleMaybe     jSite
extractAdditionalField "Latitude"              = handleMaybeShow jLatitude
extractAdditionalField "Longitude"             = handleMaybeShow jLongitude
extractAdditionalField "Date_C14_Labnr"        = handleMaybe     (fmap (intercalate ",") . jDateC14Labnr)
extractAdditionalField "Date_C14_Uncal_BP"     = handleMaybeShow jDateC14UncalBP
extractAdditionalField "Date_C14_Uncal_BP_Err" = handleMaybeShow jDateC14UncalBPErr
extractAdditionalField "Date_BC_AD_Median"     = handleMaybeShow jDateBCADMedian
extractAdditionalField "Date_BC_AD_Start"      = handleMaybeShow jDateBCADStart
extractAdditionalField "Date_BC_AD_Stop"       = handleMaybeShow jDateBCADStop
extractAdditionalField "Date_Type"             = handleMaybeShow jDateType
extractAdditionalField "No_of_Libraries"       = handleMaybeShow jNrLibraries
extractAdditionalField "Data_Type"             = handleMaybe     (fmap (intercalate "," . map show) . jDataType)
extractAdditionalField "Genotype_Ploidy"       = handleMaybeShow jGenotypePloidy
extractAdditionalField "Group_Name"            = return . intercalate "," . jGroupName
extractAdditionalField "Genetic_Sex"           = return . show . jGeneticSex
extractAdditionalField "Nr_autosomal_SNPs"     = handleMaybeShow jNrAutosomalSNPs
extractAdditionalField "Coverage_1240K"        = handleMaybeShow jCoverage1240K
extractAdditionalField "MT_Haplogroup"         = handleMaybe     jMTHaplogroup
extractAdditionalField "Y_Haplogroup"          = handleMaybe     jYHaplogroup
extractAdditionalField "Endogenous"            = handleMaybeShow jEndogenous
extractAdditionalField "UDG"                   = handleMaybeShow jUDG
extractAdditionalField "Library_Built"         = handleMaybeShow jLibraryBuilt
extractAdditionalField "Damage"                = handleMaybeShow jDamage
extractAdditionalField "Xcontam"               = handleMaybeShow jNuclearContam
extractAdditionalField "Xcontam_stderr"        = handleMaybeShow jNuclearContamErr
extractAdditionalField "mtContam"              = handleMaybeShow jMTContam
extractAdditionalField "mtContam_stderr"       = handleMaybeShow jMTContamErr
extractAdditionalField "Primary_Contact"       = handleMaybe     jPrimaryContact
extractAdditionalField "Publication_Status"    = handleMaybe     jPublication
extractAdditionalField "Note"                  = handleMaybe     jComments
extractAdditionalField "Keywords"              = handleMaybe     (fmap (intercalate ",") . jKeywords)
extractAdditionalField f                       = const (throwIO $ PoseidonGenericException (f ++ " is not a valid Janno column name"))

handleMaybe :: (JannoRow -> Maybe String) -> JannoRow -> IO String
handleMaybe func row =
    case func row of
        Just val -> return val
        Nothing -> return "n/a"

handleMaybeShow :: Show a => (JannoRow -> Maybe a) -> JannoRow -> IO String
handleMaybeShow func row =
    case func row of
        Just val -> return (show val)
        Nothing -> return "n/a"
