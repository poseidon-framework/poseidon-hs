{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..), RepoLocationSpec(..)) where

import           Poseidon.Janno         (JannoList (..), JannoRow (..),
                                         JannoRows (..))
import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logInfo, logWarning)

import           Control.Exception      (throwIO)
import           Control.Monad          (forM, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (eitherDecode')
import qualified Data.ByteString.Char8  as Bchs
import qualified Data.ByteString.Lazy   as LB
import qualified Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import           Data.List              (group, intercalate, sortOn, (\\))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
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
                tableB = [[name, show (length rows)] | (name, JannoRows rows) <- sortOn fst allSampleInfo]
            logInfo $ "found " ++ show (length tableB) ++ " packages"
            return (tableH, tableB)
        ListGroups -> do
            let tableH = ["Group", "Packages", "Nr Individuals"]
                pacJannoPairs = do
                    (pacName, (JannoRows rows)) <- allSampleInfo
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
            -- warning in case -j does not exist in the entire janno dataset
            let (JannoRows allJannoRows) = mconcat $ map snd allSampleInfo
            unless (null allJannoRows) $ do
                let allAvailableCols = HM.keys $ Csv.toNamedRecord $ head allJannoRows
                    requestedCols = map Bchs.pack moreJannoColumns
                    unknownCols = requestedCols \\ allAvailableCols
                mapM_ (\x -> logWarning $ "The column requested with -j does not exist: " ++ Bchs.unpack x) unknownCols
            -- construct table
            let tableH = ["Package", "Individual", "Group"] ++ moreJannoColumns
            tableB <- fmap concat . forM allSampleInfo $ \(pacName, JannoRows rows) ->
                forM rows (\row -> do
                    let moreFields = extractAdditionalFields row moreJannoColumns
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

readSampleInfo :: LB.ByteString -> IO [(String, JannoRows)]
readSampleInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right sam -> return sam

extractAdditionalFields :: JannoRow -> [String] -> [String]
extractAdditionalFields jannoRow =
    map (\j -> T.unpack $ T.decodeUtf8 $ HM.findWithDefault "" (Bchs.pack j) (Csv.toNamedRecord jannoRow))
