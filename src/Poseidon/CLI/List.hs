{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..), RepoLocationSpec(..)) where

import           Poseidon.Janno         (JannoList (..), JannoRow (..),
                                         jannoHeaderString)
import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logInfo, logWarning)

import           Control.Exception      (throwIO)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (eitherDecode')
import qualified Data.ByteString.Lazy   as LB
import           Data.Either            (lefts, rights)
import           Data.List              (group, intercalate, intersect, sortOn,
                                         (\\))
import           Data.Maybe             (fromMaybe)
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
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptGenoCheck        = False
}

-- | The main function running the list command
runList :: ListOptions -> PoseidonIO ()
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
            logInfo $ "found " ++ show (length tableB) ++ " packages"
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
            logInfo $ "found " ++ show (length tableB) ++ " groups/populations"
            return (tableH, tableB)
        ListIndividuals moreJannoColumns -> do
            let goodMore = moreJannoColumns `intersect` jannoHeaderString
            let badMore = moreJannoColumns \\ jannoHeaderString
            mapM_ (\x -> logWarning $ "Undefined janno column: " ++ x) badMore
            let tableH = ["Package", "Individual", "Group"] ++ goodMore
            tableB <- fmap concat . forM allSampleInfo $ \(pacName, rows) ->
                forM rows (\row -> do
                    moreFields <- extractAdditionalFields row goodMore
                    return ([pacName, jPoseidonID row, head . getJannoList . jGroupName $ row] ++ moreFields))
            logInfo $ "found " ++ show (length tableB) ++ " individuals/samples"
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

extractAdditionalFields :: JannoRow -> [String] -> PoseidonIO [String]
extractAdditionalFields jannoRow requestedCols = do
    let addFields = map (`extractAdditionalField` jannoRow) requestedCols
        unknownFields = lefts addFields
        goodFields = rights addFields
    mapM_ logWarning unknownFields
    return goodFields

extractAdditionalField :: String -> JannoRow -> Either String String
extractAdditionalField "Poseidon_ID"                    = Right .                    jPoseidonID
extractAdditionalField "Alternative_IDs"                = Right . handleMaybeList            jAlternativeIDs
extractAdditionalField "Relation_To"                    = Right . handleMaybeList            jRelationTo
extractAdditionalField "Relation_Degree"                = Right . handleMaybeShowList        jRelationDegree
extractAdditionalField "Relation_Type"                  = Right . handleMaybeList            jRelationType
extractAdditionalField "Relation_Note"                  = Right . handleMaybe                jRelationNote
extractAdditionalField "Collection_ID"                  = Right . handleMaybe                jCollectionID
extractAdditionalField "Source_Tissue"                  = Right . handleMaybeList            jSourceTissue
extractAdditionalField "Country"                        = Right . handleMaybe                jCountry
extractAdditionalField "Location"                       = Right . handleMaybe                jLocation
extractAdditionalField "Site"                           = Right . handleMaybe                jSite
extractAdditionalField "Latitude"                       = Right . handleMaybeShow            jLatitude
extractAdditionalField "Longitude"                      = Right . handleMaybeShow            jLongitude
extractAdditionalField "Date_C14_Labnr"                 = Right . handleMaybeList            jDateC14Labnr
extractAdditionalField "Date_C14_Uncal_BP"              = Right . handleMaybeShowList        jDateC14UncalBP
extractAdditionalField "Date_C14_Uncal_BP_Err"          = Right . handleMaybeShowList        jDateC14UncalBPErr
extractAdditionalField "Date_BC_AD_Median"              = Right . handleMaybeShow            jDateBCADMedian
extractAdditionalField "Date_BC_AD_Start"               = Right . handleMaybeShow            jDateBCADStart
extractAdditionalField "Date_BC_AD_Stop"                = Right . handleMaybeShow            jDateBCADStop
extractAdditionalField "Date_Type"                      = Right . handleMaybeShow            jDateType
extractAdditionalField "Date_Note"                      = Right . handleMaybe                jDateNote
extractAdditionalField "Nr_Libraries"                   = Right . handleMaybeShow            jNrLibraries
extractAdditionalField "Capture_Type"                   = Right . handleMaybeShowList        jCaptureType
extractAdditionalField "Genotype_Ploidy"                = Right . handleMaybeShow            jGenotypePloidy
extractAdditionalField "Group_Name"                     = Right . intercalate ";" . getJannoList . jGroupName
extractAdditionalField "Genetic_Sex"                    = Right . show .            jGeneticSex
extractAdditionalField "Nr_SNPs"                        = Right . handleMaybeShow            jNrSNPs
extractAdditionalField "Coverage_on_Target_SNPs"        = Right . handleMaybeShow            jCoverageOnTargets
extractAdditionalField "MT_Haplogroup"                  = Right . handleMaybe                jMTHaplogroup
extractAdditionalField "Y_Haplogroup"                   = Right . handleMaybe                jYHaplogroup
extractAdditionalField "Endogenous"                     = Right . handleMaybeShow            jEndogenous
extractAdditionalField "UDG"                            = Right . handleMaybeShow            jUDG
extractAdditionalField "Library_Built"                  = Right . handleMaybeShow            jLibraryBuilt
extractAdditionalField "Damage"                         = Right . handleMaybeShow            jDamage
extractAdditionalField "Contamination"                  = Right . handleMaybeList            jContamination
extractAdditionalField "Contamination_Err"              = Right . handleMaybeList            jContaminationErr
extractAdditionalField "Contamination_Meas"             = Right . handleMaybeList            jContaminationMeas
extractAdditionalField "Contamination_Note"             = Right . handleMaybe                jContaminationNote
extractAdditionalField "Primary_Contact"                = Right . handleMaybe                jPrimaryContact
extractAdditionalField "Genetic_Source_Accession_IDs"   = Right . handleMaybeShowList        jGeneticSourceAccessionIDs
extractAdditionalField "Data_Preparation_Pipeline_URL"  = Right . handleMaybeShow            jDataPreparationPipelineURL
extractAdditionalField "Publication"                    = Right . handleMaybeList            jPublication
extractAdditionalField "Note"                           = Right . handleMaybe                jComments
extractAdditionalField "Keywords"                       = Right . handleMaybeList            jKeywords
extractAdditionalField f                                = const $ Left ("Unkown column: " ++ f)

handleMaybe :: (JannoRow -> Maybe String) -> JannoRow -> String
handleMaybe func row = fromMaybe "n/a" (func row)

handleMaybeShow :: Show a => (JannoRow -> Maybe a) -> JannoRow -> String
handleMaybeShow func row = maybe "n/a" show (func row)

handleMaybeList :: (JannoRow -> Maybe (JannoList String)) -> JannoRow -> String
handleMaybeList func row =
    case func row of
        Just vals -> intercalate ";" (getJannoList vals)
        Nothing   -> "n/a"

handleMaybeShowList :: Show a => (JannoRow -> Maybe (JannoList a)) -> JannoRow -> String
handleMaybeShowList func row =
    case func row of
        Just vals -> intercalate ";" . map show . getJannoList $ vals
        Nothing   -> "n/a"
