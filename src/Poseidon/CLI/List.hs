{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..), RepoLocationSpec(..)) where

import           Poseidon.Janno             (PoseidonSample (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             getIndividuals,
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..), IndividualInfo(..))

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
    { _loRepoLocation  :: RepoLocationSpec -- ^ the list of base directories to search for packages
    , _loListEntity    :: ListEntity -- ^ what to list
    , _loRawOutput     :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
    , _optIgnoreGeno   :: Bool
    }

data RepoLocationSpec = RepoLocal [FilePath] | RepoRemote String

-- | A datatype to represent the options what to list
data ListEntity = ListPackages
    | ListGroups
    | ListIndividuals

-- | The main function running the list command
runList :: ListOptions -> IO ()
runList (ListOptions repoLocation listEntity rawOutput ignoreGeno) = do
    allSampleInfo <- case repoLocation of
        RepoRemote remoteURL -> do
            -- load remote samples list
            hPutStrLn stderr "Downloading sample list from remote"
            remoteOverviewJSONByteString <- simpleHttp (remoteURL ++ "/individuals_all")
            readSampleInfo remoteOverviewJSONByteString
        RepoLocal baseDirs -> do
            allPackages <- readPoseidonPackageCollection True ignoreGeno baseDirs
            return $ do -- this do starts the List Monad interface. Not to be confused with IO.
                pac <- allPackages
                jannoRow <- posPacJanno pac
                return $ IndividualInfo (posSamIndividualID jannoRow) (head (posSamGroupName jannoRow)) (posPacTitle pac)
    -- construct output
    hPutStrLn stderr "Preparing output table"
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            let tableH = ["Title", "Nr Individuals"]
                tableB = do
                    onePac <- groupBy (\x y -> indInfoPacName x == indInfoPacName y) $ 
                        sortOn indInfoPacName allSampleInfo
                    let pacTitle = indInfoPacName $ head onePac
                        pacNrInds = show (length onePac)
                    return [pacTitle, pacNrInds]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " packages")
            return (tableH, tableB)
        ListGroups -> do
            let tableH = ["Group", "Packages", "Nr Individuals"]
                tableB = do
                    oneGroup <- groupBy (\x y -> indInfoGroup x == indInfoGroup y) $ 
                        sortOn indInfoGroup allSampleInfo
                    let groupName = indInfoGroup $ head oneGroup
                        groupPacs = intercalate "," $ nub $ map indInfoPacName oneGroup
                        groupNrInds = show (length oneGroup)
                    return [groupName, groupPacs, groupNrInds]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " groups/populations")
            return (tableH, tableB)
        ListIndividuals -> do
            let tableH = ["Package", "Individual", "Group"]
            let tableB = do
                    oneSample <- allSampleInfo
                    return [indInfoPacName oneSample, indInfoName oneSample, indInfoGroup oneSample]
            hPutStrLn stderr ("found " ++ show (length tableB) ++ " individuals/samples")
            return (tableH, tableB)
    if rawOutput then
        putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate 3 (column (expandUntil 60) def def def)
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

readSampleInfo :: LB.ByteString -> IO [IndividualInfo]
readSampleInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right sam -> return sam
