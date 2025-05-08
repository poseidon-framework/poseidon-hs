{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.List (runList, ListOptions(..), ListEntity(..), RepoLocationSpec(..)) where

import           Poseidon.BibFile       (authorAbbrvString, parseAuthors)
import           Poseidon.Contributor   (ContributorSpec (..), renderORCID)
import           Poseidon.EntityTypes   (HasNameAndVersion (..))
import           Poseidon.Package       (PackageReadOptions (..),
                                         defaultPackageReadOptions,
                                         getAllGroupInfo, getBibliographyInfo,
                                         getExtendedIndividualInfo,
                                         packagesToPackageInfos,
                                         readPoseidonPackageCollection)
import           Poseidon.ServerClient  (AddColSpec (..), ApiReturnData (..),
                                         ArchiveEndpoint (..),
                                         BibliographyInfo (..),
                                         ExtendedIndividualInfo (..),
                                         GroupInfo (..), PackageInfo (..),
                                         processApiResponse, qDefault)
import           Poseidon.Utils         (PoseidonIO, logInfo, logWarning)

import           Control.Monad          (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (intercalate, nub, sortOn)
import           Data.Maybe             (catMaybes, fromMaybe)
import qualified Data.Text              as T
import           Data.Version           (Version, showVersion)
import           Text.Layout.Table      (asciiRoundS, column, def, expandUntil,
                                         rowsG, tableString, titlesH)

-- | A datatype representing command line options for the list command
data ListOptions = ListOptions
    { _listRepoLocation :: RepoLocationSpec -- ^ the list of base directories to search for packages
    , _listListEntity   :: ListEntity -- ^ what to list
    , _listRawOutput    :: Bool -- ^ whether to output raw TSV instead of a nicely formatted table
    , _listOnlyLatest   :: Bool -- ^ whether to show only latest versions of packages
    }

data RepoLocationSpec = RepoLocal [FilePath] | RepoRemote ArchiveEndpoint

-- | A datatype to represent the options what to list
data ListEntity = ListPackages Bool -- an option to list all YAML columns as well
    | ListGroups
    | ListIndividuals AddColSpec
    | ListBibliography AddColSpec

-- | The main function running the list command
runList :: ListOptions -> PoseidonIO ()
runList (ListOptions repoLocation listEntity rawOutput onlyLatest) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums      = True
        , _readOptGenoCheck            = False
        , _readOptIgnoreGeno           = True
        , _readOptOnlyLatest           = onlyLatest
    }
    -- build tables
    (tableH, tableB) <- case listEntity of
        ListPackages fullOutput -> do
            packageInfos <- case repoLocation of
                RepoRemote (ArchiveEndpoint remoteURL archive) -> do
                    logInfo "Downloading package data from server"
                    apiReturn <- processApiResponse (remoteURL ++ "/packages" ++ qDefault archive) False
                    case apiReturn of
                        ApiReturnPackageInfo pacInfo -> return pacInfo
                        _ -> error "should not happen"
                RepoLocal baseDirs -> do
                    pacCollection <- readPoseidonPackageCollection pacReadOpts baseDirs
                    packagesToPackageInfos pacCollection
            let tableH =
                    let baseColumnH = ["Package", "Package Version", "Is Latest", "Poseidon Version", "Description", "Last modified", "Nr Individuals"]
                    in  if fullOutput then
                            baseColumnH ++ ["Contributors", "Base Directory", "GenotypeFiles", "JannoFile", "SeqSourceFile", "BibFile", "ReadmeFile", "ChangelogFile"]
                        else
                            baseColumnH
                tableB = sortOn head $ do
                    pInf <- packageInfos
                    -- for the locally read packages this doesn't do anything,
                    -- because the dataset is already reduced to the latest packages
                    -- in the reading process
                    True <- return (not onlyLatest || pIsLatest pInf)
                    let baseCols = [getPacName pInf, showMaybeVersion (getPacVersion pInf), show $ pIsLatest pInf,
                            showVersion $ pPosVersion pInf, showMaybe $ pDescription pInf, showMaybe (show <$> pLastModified pInf), show $ pNrIndividuals pInf]
                    if fullOutput then
                        return $ baseCols ++ [intercalate ";" . map showContributor . pContributors $ pInf, pBaseDir pInf, intercalate "," . pGenotypeFiles $ pInf,
                            showMaybe $ pJannoFile pInf, showMaybe $ pSeqSourceFile pInf, showMaybe $ pBibFile pInf, showMaybe $ pReadmeFile pInf,
                            showMaybe $ pChangelogFile pInf]
                    else
                        return baseCols
            return (tableH, tableB)
        ListGroups -> do
            groupInfos <- case repoLocation of
                RepoRemote (ArchiveEndpoint remoteURL archive) -> do
                    logInfo "Downloading group data from server"
                    apiReturn <- processApiResponse (remoteURL ++ "/groups" ++ qDefault archive) False
                    case apiReturn of
                        ApiReturnGroupInfo groupInfo -> return groupInfo
                        _ -> error "should not happen"
                RepoLocal baseDirs -> do
                    pacCollection <- readPoseidonPackageCollection pacReadOpts baseDirs
                    getAllGroupInfo pacCollection
            let tableH = ["Group", "Package", "Package Version", "Is Latest", "Nr Individuals"]
                tableB = do
                    gi@(GroupInfo groupName _ isLatest nrInds) <- groupInfos
                    True <- return (not onlyLatest || isLatest)
                    return [groupName, getPacName gi, showMaybeVersion (getPacVersion gi), show isLatest, show nrInds]
            return (tableH, tableB)
        ListIndividuals addJannoColSpec -> do
            extIndInfos <- case repoLocation of
                RepoRemote (ArchiveEndpoint remoteURL archive) -> do
                    logInfo "Downloading individual data from server"
                    let addJannoColFlag = case addJannoColSpec of
                            AddColAll -> "&additionalJannoColumns=ALL"
                            AddColList [] -> ""
                            AddColList moreJannoColumns -> "&additionalJannoColumns=" ++ intercalate "," moreJannoColumns
                    apiReturn <- processApiResponse (remoteURL ++ "/individuals" ++ qDefault archive ++ addJannoColFlag) False
                    case apiReturn of
                        ApiReturnExtIndividualInfo indInfo -> return indInfo
                        _ -> error "should not happen"
                RepoLocal baseDirs -> do
                    pacCollection <- readPoseidonPackageCollection pacReadOpts baseDirs
                    getExtendedIndividualInfo pacCollection addJannoColSpec

            let addJannoCols = case extIndInfos of -- get all add-column names from first extIndInfo
                    []    -> []
                    (e:_) -> map fst . extIndInfoAddCols $ e

            -- warning in case the additional Columns do not exist in the entire janno dataset,
            -- we only output this warning if the columns were requested explicitly. Not if
            -- all columns were requested. We consider such an "all" request to mean "all columns that are present".
            case addJannoColSpec of
                AddColList (_:_) -> do
                    forM_ (zip [0..] addJannoCols) $ \(i, columnKey) -> do
                        -- check entries in all individuals for that key
                        let nonEmptyEntries = catMaybes [snd (entries !! i) | ExtendedIndividualInfo _ _ _ _ entries <- extIndInfos]
                        when (null nonEmptyEntries) . logWarning $ "Column Name " ++ columnKey ++ " not present in any individual"
                _ -> return ()

            let tableH = ["Individual", "Group", "Package", "PackageVersion", "Is Latest"] ++ addJannoCols
                tableB = do
                    i@(ExtendedIndividualInfo name groups _ isLatest addColumnEntries) <- extIndInfos
                    True <- return (not onlyLatest || isLatest)
                    return $ [name, intercalate ", " groups, getPacName i,
                              showMaybeVersion (getPacVersion i), show  isLatest] ++
                              map (fromMaybe "n/a" . snd) addColumnEntries
            return (tableH, tableB)
        ListBibliography addColSpec -> do
            bibInfos <- case repoLocation of
                RepoRemote (ArchiveEndpoint remoteURL archive) -> do
                    logInfo "Downloading bibliography data from server"
                    let addJannoColFlag = case addColSpec of
                            AddColAll -> "&additionalBibColumns=ALL"
                            AddColList [] -> ""
                            AddColList moreBibFields -> "&additionalBibColumns=" ++ intercalate "," moreBibFields
                    apiReturn <- processApiResponse (remoteURL ++ "/bibliography" ++ qDefault archive ++ addJannoColFlag) False
                    case apiReturn of
                        ApiReturnBibInfo bibInfo -> return bibInfo
                        _                        -> error "should not happen"
                RepoLocal baseDirs -> do
                    pacCollection <- readPoseidonPackageCollection pacReadOpts baseDirs
                    getBibliographyInfo pacCollection addColSpec

            let addBibFieldNames = case addColSpec of
                    AddColAll -> nub . concatMap (map fst . bibInfoAddCols) $ bibInfos
                    AddColList names -> names

            -- warning in case the additional Columns do not exist in the entire janno dataset,
            -- we only output this warning if the columns were requested explicitly. Not if
            -- all columns were requested. We consider such an "all" request to mean "all columns that are present".
            case addColSpec of
                AddColList (_:_) -> do
                    forM_ addBibFieldNames $ \bibFieldKey -> do
                        -- check entries in all individuals for that key
                        let nonEmptyEntries = do
                                bibInfo <- bibInfos
                                Just (Just _) <- return $ bibFieldKey `lookup` bibInfoAddCols bibInfo
                                return ()
                        when (null nonEmptyEntries) . logWarning $
                            "Bibliography field " ++ bibFieldKey ++ "is not present in any bibliography entry"
                _ -> return ()

            let tableH = ["BibKey", "Title", "Author", "Year", "DOI",
                          "Nr of samples"] ++ addBibFieldNames
                tableB = do
                    bibInfo <- bibInfos
                    let addBibFieldColumns = do
                            bibFieldName <- addBibFieldNames
                            case bibFieldName `lookup` bibInfoAddCols bibInfo of
                                Just (Just v) -> return v
                                _             -> return ""
                    authors <- parseAuthors . curateBibField . bibInfoAuthor $ bibInfo
                    return $ [bibInfoKey bibInfo, curateBibField $ bibInfoTitle bibInfo, authorAbbrvString authors,
                              curateBibField $ bibInfoYear bibInfo,
                              curateBibField $ bibInfoDoi bibInfo, show (bibInfoNrSamples bibInfo)] ++
                              addBibFieldColumns
            return (tableH, tableB)

    if rawOutput then
        liftIO $ putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableH:tableB]
    else do
        let colSpecs = replicate (length tableH) (column (expandUntil 60) def def def)
        liftIO $ putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    showMaybe :: Maybe String -> String
    showMaybe = fromMaybe "n/a"
    showMaybeVersion :: Maybe Version -> String
    showMaybeVersion = maybe "n/a" showVersion

    -- this function is necessary, as BibTeX sometimes has arbitrary line breaks within fields,
    -- which we need to get rid of to avoid down-stream problems
    curateBibField :: Maybe String -> String
    curateBibField = T.unpack . T.intercalate " " . map T.strip . T.lines . T.pack . fromMaybe ""
    showContributor :: ContributorSpec -> String
    showContributor (ContributorSpec n e o) = n ++ "(" ++ e ++ ") ORCID: " ++ maybe "n/a" renderORCID o
