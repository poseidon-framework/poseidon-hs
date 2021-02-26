{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..)) where

import           Poseidon.Janno             (PoseidonSample (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             getIndividuals,
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..))

import           Control.Exception          (throwIO)
import           Control.Monad              (forM)
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
    { _loBaseDirs     :: Maybe [FilePath] -- ^ the list of base directories to search for packages
    , _loRemote       :: Bool -- ^ list data froma a remote server
    , _loRemoteURL    :: String -- ^ remote server URL
    , _loListEntity   :: ListEntity -- ^ what to list
    , _loRawOutput    :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
    , _optIgnoreGeno  :: Bool
    }

-- | A datatype to represent the options what to list
data ListEntity = ListPackages
    | ListGroups
    | ListIndividuals

-- | A datatype to represent the content of the entries in /individuals_all
data RemoteSample = RemoteSample 
    { remSamIndividualID :: String
    , remSamGroupName   :: String
    , remSamPacTitle     :: String
    }

instance FromJSON RemoteSample where
    parseJSON = withObject "RemoteSample" $ \v -> RemoteSample
        <$> v .:   "name"
        <*> v .:   "group"
        <*> v .:   "pacName"

-- | The main function running the list command
runList :: ListOptions -> IO ()
runList (ListOptions _ True remoteURL listEntity rawOutput _) = do
    let remote = remoteURL
    -- load remote samples list
    hPutStrLn stderr "Downloading sample list from remote"
    remoteOverviewJSONByteString <- simpleHttp (remote ++ "/individuals_all")
    allRemoteSamples <- readSampleInfo remoteOverviewJSONByteString
    -- construct output
    hPutStrLn stderr "Preparing output table"
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            let tableH = ["Title", "Nr Individuals"]
                tableB = do
                    onePac <- groupBy (\x y -> remSamPacTitle x == remSamPacTitle y) $ 
                        sortOn remSamPacTitle allRemoteSamples
                    let pacTitle = remSamPacTitle $ head onePac
                        pacNrInds = show (length onePac)
                    return [pacTitle, pacNrInds]
            return (tableH, tableB)
        ListGroups -> do
            let tableH = ["Group", "Packages", "Nr Individuals"]
                tableB = do
                    oneGroup <- groupBy (\x y -> remSamGroupName x == remSamGroupName y) $ 
                        sortOn remSamGroupName allRemoteSamples
                    let groupName = remSamGroupName $ head oneGroup
                        groupPacs = intercalate "," $ nub $ map remSamPacTitle oneGroup
                        groupNrInds = show (length oneGroup)
                    return [groupName, groupPacs, groupNrInds]
            return (tableH, tableB)
        ListIndividuals -> do
            let tableH = ["Package", "Individual", "Group"]
            let tableB = do
                    oneSample <- allRemoteSamples
                    return [remSamPacTitle oneSample, remSamIndividualID oneSample, remSamGroupName oneSample]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " individuals.")
            return (tableH, tableB)

    if rawOutput then
        putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate 3 (column (expandUntil 60) def def def)
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

runList (ListOptions (Just baseDirs) False _ listEntity rawOutput ignoreGeno) = do
    -- load local packages
    allPackages <- readPoseidonPackageCollection True ignoreGeno baseDirs
    -- construct output
    hPutStrLn stderr "Preparing output table"
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
            let tableH = ["Group", "Packages", "Nr Individuals"]
                tableB = do
                    indGroup <- allIndsSortedByGroup
                    let packages_ = nub [i!!0 | i <- indGroup]
                    let nrInds = length indGroup
                    return [(indGroup!!0)!!2, intercalate "," packages_, show nrInds]
            return (tableH, tableB)
        ListIndividuals -> do
            let tableH = ["Package", "Individual", "Group"]
            let tableB = do
                    pac <- allPackages
                    jannoRow <- posPacJanno pac
                    return [posPacTitle pac, posSamIndividualID jannoRow, head (posSamGroupName jannoRow)]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " individuals.")
            return (tableH, tableB)

    if rawOutput then
        putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate 3 (column (expandUntil 60) def def def)
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    showMaybeDate (Just d) = show d
    showMaybeDate Nothing  = "n/a"

readSampleInfo :: LB.ByteString -> IO [RemoteSample]
readSampleInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right sam -> return sam
