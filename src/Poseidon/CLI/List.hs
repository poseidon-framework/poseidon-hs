{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..), RepoLocationSpec(..)) where

import           Poseidon.Janno         (JannoList (..), JannoRow (..))
import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonException (..), PoseidonLogIO)

import           Colog                  (logInfo)
import           Control.Exception      (throwIO)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (eitherDecode')
import qualified Data.ByteString.Lazy   as LB
import           Data.List              (group, intercalate, sortOn)
import           Data.Text              (pack)
import           Network.HTTP.Conduit   (simpleHttp)
import           Text.Layout.Table      (asciiRoundS, column, def, expandUntil,
                                         rowsG, tableString, titlesH)

-- | A datatype representing command line options for the list command
data ListOptions = ListOptions
    { _listRepoLocation :: RepoLocationSpec -- ^ the list of base directories to search for packages
    , _listListEntity   :: ListEntity -- ^ what to list
    , _listRawOutput    :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
    , _listIgnoreGeno   :: Bool
    }

data RepoLocationSpec = RepoLocal [FilePath] | RepoRemote String

-- | A datatype to represent the options what to list
data ListEntity = ListPackages
    | ListGroups
    | ListIndividuals [String]

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptGenoCheck        = False
    }

-- | The main function running the list command
runList :: ListOptions -> PoseidonLogIO ()
runList (ListOptions repoLocation listEntity rawOutput ignoreGeno) = do
    allSampleInfo <- case repoLocation of
        RepoRemote remoteURL -> do
            -- load remote samples list
            logInfo "Downloading sample list from remote"
            remoteOverviewJSONByteString <- simpleHttp (remoteURL ++ "/janno_all")
            liftIO $ readSampleInfo remoteOverviewJSONByteString
        RepoLocal baseDirs -> do
            allPackages <- readPoseidonPackageCollection pacReadOpts {_readOptIgnoreGeno = ignoreGeno} baseDirs
            return [(posPacTitle pac, posPacJanno pac) | pac <- allPackages]
    -- construct output
    logInfo "Preparing output table"
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            let tableH = ["Title", "Nr Individuals"]
                tableB = [[name, show (length rows)] | (name, rows) <- sortOn fst allSampleInfo]
            logInfo $ pack ("found " ++ show (length tableB) ++ " packages")
            return (tableH, tableB)
        ListGroups -> do
            let tableH = ["Group", "Packages", "Nr Individuals"]
                pacJannoPairs = do
                    (pacName, rows) <- allSampleInfo
                    row <- rows
                    return (pacName, row)
                tableB = do
                    let pacGroupPairsForIndividuals = unnestGroupNames pacJannoPairs
                    oneGroup <- group $ sortOn snd pacGroupPairsForIndividuals
                    let groupName = head $ map snd oneGroup
                        groupPacs = head $ map fst oneGroup
                        groupNrInds = show (length oneGroup)
                    return [groupName, groupPacs, groupNrInds]
            logInfo $ pack ("found " ++ show (length tableB) ++ " groups/populations")
            return (tableH, tableB)
        ListIndividuals moreJannoColumns -> do
            let tableH = ["Package", "Individual", "Group"] ++ moreJannoColumns
            tableB <- fmap concat . forM allSampleInfo $ \(pacName, rows) ->
                forM rows (\row -> do
                    moreFields <- liftIO $ extractAdditionalFields row moreJannoColumns
                    return ([pacName, jPoseidonID row, head . getJannoList . jGroupName $ row] ++ moreFields))
            logInfo $ pack ("found " ++ show (length tableB) ++ " individuals/samples")
            return (tableH, tableB)
    if rawOutput then
        liftIO $ putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate (length tableH) (column (expandUntil 60) def def def)
        liftIO $ putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

unnestGroupNames :: [(String, JannoRow)] -> [(String, String)]
unnestGroupNames = concatMap unnestOne
    where
        unnestOne :: (String, JannoRow) -> [(String, String)]
        unnestOne (pac, jR) =
            let groups = getJannoList . jGroupName $ jR
            in zip (repeat pac) groups

readSampleInfo :: LB.ByteString -> IO [(String, [JannoRow])]
readSampleInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right sam -> return sam

extractAdditionalFields :: JannoRow -> [String] -> IO [String]
extractAdditionalFields jannoRow = mapM (\f -> extractAdditionalField f jannoRow)

extractAdditionalField :: String -> JannoRow -> IO String
extractAdditionalField "Poseidon_ID"                    = return .                   jPoseidonID
extractAdditionalField "Alternative_IDs"                = handleMaybeList            jAlternativeIDs
extractAdditionalField "Relation_To"                    = handleMaybeList            jRelationTo
extractAdditionalField "Relation_Degree"                = handleMaybeShowList        jRelationDegree
extractAdditionalField "Relation_Type"                  = handleMaybeList            jRelationType
extractAdditionalField "Relation_Note"                  = handleMaybe                jRelationNote
extractAdditionalField "Collection_ID"                  = handleMaybe                jCollectionID
extractAdditionalField "Source_Tissue"                  = handleMaybeList            jSourceTissue
extractAdditionalField "Country"                        = handleMaybe                jCountry
extractAdditionalField "Location"                       = handleMaybe                jLocation
extractAdditionalField "Site"                           = handleMaybe                jSite
extractAdditionalField "Latitude"                       = handleMaybeShow            jLatitude
extractAdditionalField "Longitude"                      = handleMaybeShow            jLongitude
extractAdditionalField "Date_C14_Labnr"                 = handleMaybeList            jDateC14Labnr
extractAdditionalField "Date_C14_Uncal_BP"              = handleMaybeShowList        jDateC14UncalBP
extractAdditionalField "Date_C14_Uncal_BP_Err"          = handleMaybeShowList        jDateC14UncalBPErr
extractAdditionalField "Date_BC_AD_Median"              = handleMaybeShow            jDateBCADMedian
extractAdditionalField "Date_BC_AD_Start"               = handleMaybeShow            jDateBCADStart
extractAdditionalField "Date_BC_AD_Stop"                = handleMaybeShow            jDateBCADStop
extractAdditionalField "Date_Type"                      = handleMaybeShow            jDateType
extractAdditionalField "Date_Note"                      = handleMaybe                jDateNote
extractAdditionalField "Nr_Libraries"                   = handleMaybeShow            jNrLibraries
extractAdditionalField "Capture_Type"                   = handleMaybeShowList        jCaptureType
extractAdditionalField "Genotype_Ploidy"                = handleMaybeShow            jGenotypePloidy
extractAdditionalField "Group_Name"                     = return . intercalate ";" . getJannoList . jGroupName
extractAdditionalField "Genetic_Sex"                    = return . show .            jGeneticSex
extractAdditionalField "Nr_SNPs"                        = handleMaybeShow            jNrSNPs
extractAdditionalField "Coverage_on_Target_SNPs"        = handleMaybeShow            jCoverageOnTargets
extractAdditionalField "MT_Haplogroup"                  = handleMaybe                jMTHaplogroup
extractAdditionalField "Y_Haplogroup"                   = handleMaybe                jYHaplogroup
extractAdditionalField "Endogenous"                     = handleMaybeShow            jEndogenous
extractAdditionalField "UDG"                            = handleMaybeShow            jUDG
extractAdditionalField "Library_Built"                  = handleMaybeShow            jLibraryBuilt
extractAdditionalField "Damage"                         = handleMaybeShow            jDamage
extractAdditionalField "Contamination"                  = handleMaybeList            jContamination
extractAdditionalField "Contamination_Err"              = handleMaybeList            jContaminationErr
extractAdditionalField "Contamination_Meas"             = handleMaybeList            jContaminationMeas
extractAdditionalField "Contamination_Note"             = handleMaybe                jContaminationNote
extractAdditionalField "Primary_Contact"                = handleMaybe                jPrimaryContact
extractAdditionalField "Genetic_Source_Accession_IDs"   = handleMaybeShowList        jGeneticSourceAccessionIDs
extractAdditionalField "Data_Preparation_Pipeline_URL"  = handleMaybeShow            jDataPreparationPipelineURL
extractAdditionalField "Publication"                    = handleMaybeList            jPublication
extractAdditionalField "Note"                           = handleMaybe                jComments
extractAdditionalField "Keywords"                       = handleMaybeList            jKeywords
extractAdditionalField f                                = const (throwIO $ PoseidonGenericException (f ++ " is not a standard Janno column name"))

handleMaybe :: (JannoRow -> Maybe String) -> JannoRow -> IO String
handleMaybe func row =
    case func row of
        Just val -> return val
        Nothing  -> return "n/a"

handleMaybeShow :: Show a => (JannoRow -> Maybe a) -> JannoRow -> IO String
handleMaybeShow func row =
    case func row of
        Just val -> return $ show val
        Nothing  -> return "n/a"

handleMaybeList :: (JannoRow -> Maybe (JannoList String)) -> JannoRow -> IO String
handleMaybeList func row =
    case func row of
        Just vals -> return . intercalate ";" . getJannoList $ vals
        Nothing   -> return "n/a"

handleMaybeShowList :: Show a => (JannoRow -> Maybe (JannoList a)) -> JannoRow -> IO String
handleMaybeShowList func row =
    case func row of
        Just vals -> return . intercalate ";" . map show . getJannoList $ vals
        Nothing   -> return "n/a"
