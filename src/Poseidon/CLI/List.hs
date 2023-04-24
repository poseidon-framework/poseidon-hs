{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..), RepoLocationSpec(..)) where

import           Poseidon.Janno          (JannoList (..), JannoRow (..),
                                          JannoRows (..))
import           Poseidon.Package        (PackageReadOptions (..),
                                          PoseidonPackage (..),
                                          defaultPackageReadOptions,
                                          packageToPackageInfo,
                                          readPoseidonPackageCollection)
import           Poseidon.SecondaryTypes (ApiReturnData (..), PackageInfo (..), ServerApiReturnType(..))
import           Poseidon.Utils          (PoseidonException (..), PoseidonIO,
                                          logError, logInfo, logWarning)

import           Control.Exception       (throwIO)
import           Control.Monad           (forM, unless, when, forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (FromJSON, eitherDecode')
import qualified Data.ByteString.Char8   as Bchs
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Csv                as Csv
import qualified Data.HashMap.Strict     as HM
import           Data.List               (group, intercalate, sortOn, (\\))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Version            (showVersion)
import           Network.HTTP.Conduit    (simpleHttp)
import           Text.Layout.Table       (asciiRoundS, column, def, expandUntil,
                                          rowsG, tableString, titlesH)

-- | A datatype representing command line options for the list command
data ListOptions = ListOptions
    { _listRepoLocation :: RepoLocationSpec -- ^ the list of base directories to search for packages
    , _listListEntity   :: ListEntity -- ^ what to list
    , _listRawOutput    :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
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
    , _readOptKeepMultipleVersions = True
    , _readOptIgnoreGeno = True
}

-- | The main function running the list command
runList :: ListOptions -> PoseidonIO ()
runList (ListOptions repoLocation listEntity rawOutput) = do
    (tableH, tableB) <- case listEntity of
        ListPackages -> do
            packageInfo <- case repoLocation of
                RepoRemote remoteURL -> do
                    logInfo "Downloading package data from server"
                    apiReturn <- processApiResponse (remoteURL ++ "/packages")
                    case apiReturn of
                        ApiReturnPackageInfo pacInfo -> return pacInfo
                        _ -> error "should not happen"
                RepoLocal baseDirs ->
                    map packageToPackageInfo <$> readPoseidonPackageCollection pacReadOpts baseDirs
            let tableH = ["Title", "Package Version", "Poseidon Version", "Description", "Last modified", "Nr Individuals"]
                tableB = sortOn head $ do
                    PackageInfo t v pv d l i <- packageInfo
                    return [t, showMaybe (showVersion <$> v), showVersion pv, showMaybe d, showMaybe (show <$> l), show i]
            return (tableH, tableB)
        _ -> undefined
        ListGroups -> do
            groupInfo <- case repoLocation of
                RepoRemote remoteURL -> do
                    logInfo "Downloading group data from server"
                    apiReturn <- processApiResponse (remoteURL ++ "/groups")
                    case apiReturn of
                        ApiReturnGroupInfo groupInfo -> return groupInfo
                        _ -> error "should not happen"
                RepoLocal baseDirs -> getAllGroupInfo <$> readPoseidonPackageCollection pacReadOpts baseDirs
            let tableH = ["Group", "Packages", "Nr Individuals"]
                tableB = do
                    GroupInfo groupName pacsAndVersions nrInds <- groupInfo
                    let pacString = intercalate ", " $ do
                            pacGroup <- groupBy ((==) . fst) . sortBy fst $ pacsAndVersions
                            let pacGroupNames = map fst pacGroup
                                pacGroupMaybeVersions = map snd pacGroup
                            case pacGroup of
                                [(name, _)] -> return name -- just a single package with that name, print it
                                multiplePacs -> do -- multiple packages with that name, add versions in brackets
                                    (pacName, maybeVersion) <- multiplePacs
                                    case maybeVersion of
                                        Nothing -> error "should never happen" -- If there are multiple packages with the same name, all of them should have a version
                                        Just v -> return $ pacName ++ "(" ++ showVersion v ++ ")"
                    return [groupName, pacString, show nrInds]
            return (tableH, tableB)
        ListIndividuals moreJannoColumns -> do
            indInfo <- case repoLocation of
                RepoRemote remoteURL -> do
                    logInfo "Downloading individual data from server"
                    apiReturn <- processApiResponse (remoteURL ++ "/individuals")
                    case apiReturn of
                        ApiReturnIndividualInfo indInfo pacVersions additionalColumns -> return (indInfo, pacVersions, additionalColumns)
                        _ -> error "should not happen"
                RepoLocal -> do
                    
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
  where
    showMaybe :: Maybe String -> String
    showMaybe = maybe "n/a" id

unnestGroupNames :: [(String, JannoRow)] -> [(String, String)]
unnestGroupNames = concatMap unnestOne
    where
        unnestOne :: (String, JannoRow) -> [(String, String)]
        unnestOne (pac, jR) =
            let groups = getJannoList . jGroupName $ jR
            in zip (repeat pac) groups

processApiResponse :: String -> PoseidonIO ApiReturnData
processApiResponse url = do
    remoteData <- simpleHttp url
    ServerApiReturnType messages maybeReturn <- case eitherDecode' remoteData of
        Left err  -> liftIO . throwIO $ PoseidonRemoteJSONParsingException err
        Right sam -> return sam
    unless (null messages) $
        forM_ messages (\msg -> logInfo $ "Message from the Server: " ++ msg)
    case maybeReturn of
        Just apiReturn -> return apiReturn
        Nothing -> do
            logError "The server request was unsuccessful"
            liftIO . throwIO $ PoseidonServerCommunicationException "Server error"


extractAdditionalFields :: JannoRow -> [String] -> [String]
extractAdditionalFields jannoRow =
    map (\j -> T.unpack $ T.decodeUtf8 $ HM.findWithDefault "" (Bchs.pack j) (Csv.toNamedRecord jannoRow))
