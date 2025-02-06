{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Poseidon.CLI.Serve (runServer, runServerMainThread, ServeOptions(..), ArchiveConfig (..), ArchiveSpec (..)) where

import           Poseidon.EntityTypes         (HasNameAndVersion (..),
                                               PacNameAndVersion,
                                               renderNameWithVersion)
import           Poseidon.GenotypeData        (GenotypeDataSpec (..),
                                               GenotypeFileSpec (..))
import           Poseidon.Janno               (JannoRow (..), getJannoRows)
import           Poseidon.Package             (PackageReadOptions (..),
                                               PoseidonPackage (..),
                                               defaultPackageReadOptions,
                                               getAllGroupInfo,
                                               getBibliographyInfo,
                                               getExtendedIndividualInfo,
                                               getJannoRowsFromPac,
                                               packagesToPackageInfos,
                                               readPoseidonPackageCollection)
import           Poseidon.PoseidonVersion     (minimalRequiredClientVersion)
import           Poseidon.ServerClient        (AddColSpec (..),
                                               ApiReturnData (..),
                                               ServerApiReturnType (..))
import           Poseidon.ServerHTML
import           Poseidon.ServerStylesheet    (stylesBS)
import           Poseidon.Utils               (LogA, PoseidonIO, envLogAction,
                                               logDebug, logInfo, logWithEnv)

import           Codec.Archive.Zip            (Archive, addEntryToArchive,
                                               emptyArchive, fromArchive,
                                               toEntry)
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar)
import           Control.Monad                (foldM, forM, when)
import           Control.Monad.IO.Class       (liftIO, MonadIO)
import qualified Data.ByteString.Lazy         as B
import           Data.List                    (groupBy, intercalate,
                                               sortOn)
import           Data.List.Split              (splitOn)
import           Data.Maybe                   (isJust, mapMaybe)
import           Data.Ord                     (Down (..))
import           Data.Text.Lazy               (pack)
import           Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)
import           Data.Version                 (Version, parseVersion,
                                               showVersion)
import           Network.Wai                  (pathInfo, queryString)
import           Network.Wai.Handler.Warp     (defaultSettings, runSettings,
                                               setBeforeMainLoop, setPort)
import           Network.Wai.Handler.WarpTLS  (runTLS, tlsSettings,
                                               tlsSettingsChain)
import           Network.Wai.Middleware.Cors  (simpleCors)
import           Paths_poseidon_hs            (version)
import           Poseidon.BibFile             (renderBibEntry)
import           Poseidon.ColumnTypes         (JannoLatitude (..),
                                               JannoLongitude (..))
import           System.Directory             (createDirectoryIfMissing,
                                               doesFileExist,
                                               getModificationTime)
import           System.FilePath              ((<.>), (</>))
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Web.Scotty                   (ActionM, ScottyM, captureParam,
                                               file, get, json, middleware,
                                               notFound, queryParamMaybe, raw,
                                               redirect, request, scottyApp,
                                               setHeader, text)
import Data.Yaml (FromJSON, parseJSON, (.:?), decodeFileThrow)
import Data.Yaml.Aeson (withObject, (.:))

data ServeOptions = ServeOptions
    { cliArchiveConfig   :: Either ArchiveConfig FilePath
    , cliZipDir          :: Maybe FilePath
    , cliPort            :: Int
    , cliIgnoreChecksums :: Bool
    , cliCertFiles       :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving (Show)

newtype ArchiveConfig = ArchiveConfig [ArchiveSpec] deriving Show

instance FromJSON ArchiveConfig where
    parseJSON = withObject "PoseidonYamlStruct" $ \v -> ArchiveConfig
        <$> v .: "contributor"

parseArchiveConfigFile :: (MonadIO m) => FilePath -> m ArchiveConfig
parseArchiveConfigFile = decodeFileThrow

data ArchiveSpec = ArchiveSpec
    { _archSpecName :: ArchiveName
    , _archSpecPaths :: [FilePath]
    , _archSpecDescription :: Maybe String
    , _archSpecURL :: Maybe String
    } deriving (Show)

instance FromJSON ArchiveSpec where
    parseJSON = withObject "contributor" $ \v -> ArchiveSpec
        <$> v .:  "name"
        <*> v .:  "paths"
        <*> v .:? "description"
        <*> v .:? "URL"

type ZipStore = [(PacNameAndVersion, FilePath)] -- maps PackageName+Version to a zipfile-path

type ArchiveName = String

type ArchiveStore a = [(ArchiveSpec, a)] -- a generic lookup table from an archive name to an item
-- we have two concrete ones: ArchiveStore [PoseidonPackage]   and     ArchiveStore ZipStore

getArchiveSpecs :: ArchiveStore a -> [ArchiveSpec]
getArchiveSpecs = map fst

getArchiveByName :: (MonadFail m) => ArchiveName -> ArchiveStore a -> m a
getArchiveByName name store =
    case filter (\(spec, _) -> _archSpecName spec == name) store of
      []      -> fail $ "Archive " <> name <> " does not exist"
      [(_,a)] -> pure a
      _       -> fail $ "Archive " <> name <> " is ambiguous"

runServerMainThread :: ServeOptions -> PoseidonIO ()
runServerMainThread opts = do
    -- the MVar is used as a signal from the server to the calling thread that it is ready.
    -- It is used for testing. Here we just use it as a dummy.
    dummy <- liftIO newEmptyMVar
    runServer opts dummy

runServer :: ServeOptions -> MVar () -> PoseidonIO ()
runServer (ServeOptions archBaseDirs maybeZipPath port ignoreChecksums certFiles) serverReady = do
    let pacReadOpts = defaultPackageReadOptions {
              _readOptIgnoreChecksums  = ignoreChecksums
            , _readOptGenoCheck        = isJust maybeZipPath
        }

    logInfo "Server starting up. Loading packages..."
    archiveStore <- case archBaseDirs of
        Left archiveConfig -> readArchiveStore archiveConfig pacReadOpts
        Right path -> do
            archiveConfig <- parseArchiveConfigFile path
            readArchiveStore archiveConfig pacReadOpts

    logInfo $ "Using " ++ (_archSpecName . fst . head) archiveStore ++ " as the default archive"

    zipArchiveStore <- case maybeZipPath of
        Nothing -> return []
        Just z  -> createZipArchiveStore archiveStore z

    let archiveSpecs = getArchiveSpecs archiveStore

    let runScotty = case certFiles of
            Nothing                              -> scottyHTTP  serverReady port
            Just (certFile, chainFiles, keyFile) -> scottyHTTPS serverReady port certFile chainFiles keyFile

    logA <- envLogAction
    runScotty $ do
        middleware simpleCors

        get "/server_version" $ do
            logRequest logA
            text . pack . showVersion $ version

        get "/packages" . conditionOnClientVersion $ do
            logRequest logA
            pacs <- getItemFromArchiveStore archiveStore
            pacInfos <- packagesToPackageInfos pacs
            let retData = ApiReturnPackageInfo pacInfos
            return $ ServerApiReturnType [] (Just retData)

        get "/groups" . conditionOnClientVersion $ do
            logRequest logA
            pacs <- getItemFromArchiveStore archiveStore
            groupInfos <- getAllGroupInfo pacs
            let retData = ApiReturnGroupInfo groupInfos
            return $ ServerApiReturnType [] (Just retData)

        get "/individuals" . conditionOnClientVersion $ do
            logRequest logA
            pacs <- getItemFromArchiveStore archiveStore
            maybeAdditionalColumnsString <- queryParamMaybe "additionalJannoColumns"
            indInfo <- case maybeAdditionalColumnsString of
                    Just "ALL" -> getExtendedIndividualInfo pacs AddColAll -- Nothing means all Janno Columns
                    Just additionalColumnsString ->
                        let additionalColumnNames = splitOn "," additionalColumnsString
                        in  getExtendedIndividualInfo pacs (AddColList additionalColumnNames)
                    Nothing -> getExtendedIndividualInfo pacs (AddColList [])
            let retData = ApiReturnExtIndividualInfo indInfo
            return $ ServerApiReturnType [] (Just retData)

        get "/bibliography" . conditionOnClientVersion $ do
            logRequest logA
            pacs <- getItemFromArchiveStore archiveStore
            maybeAdditionalBibFieldsString <- queryParamMaybe "additionalBibColumns"
            bibInfo <- case maybeAdditionalBibFieldsString of
                    Just "ALL" -> getBibliographyInfo pacs AddColAll -- Nothing means all Janno Columns
                    Just additionalBibFieldsString ->
                        let additionalBibFields = splitOn "," additionalBibFieldsString
                        in  getBibliographyInfo pacs (AddColList additionalBibFields)
                    Nothing -> getBibliographyInfo pacs (AddColList [])
            let retData = ApiReturnBibInfo bibInfo
            return $ ServerApiReturnType [] (Just retData)

        -- API for retreiving package zip files
        when (isJust maybeZipPath) . get "/zip_file/:package_name" $ do
            logRequest logA
            zipStore <- getItemFromArchiveStore zipArchiveStore
            packageName <- captureParam "package_name"
            maybeVersionString <- queryParamMaybe "package_version"
            maybeVersion <- case maybeVersionString of
                Nothing -> return Nothing
                Just versionStr -> case parseVersionString versionStr of
                    Nothing -> fail $ "Could not parse package version string " ++ versionStr
                    Just v -> return $ Just v
            case sortOn (Down . fst) . filter ((==packageName) . getPacName . fst) $ zipStore of
                [] -> fail $ "unknown package " ++ packageName -- no version found
                [(pacNameAndVersion, fn)] -> case maybeVersion of -- exactly one version found
                    Nothing -> file fn
                    Just v -> if getPacVersion pacNameAndVersion == Just v then file fn else fail $ "Package " ++ packageName ++ " is not available for version " ++ showVersion v
                pl@((_, fnLatest) : _) -> case maybeVersion of
                    Nothing -> file fnLatest
                    Just v -> case filter ((==Just v) . getPacVersion . fst) pl of
                        [] -> fail $ "Package " ++ packageName ++ "is not available for version " ++ showVersion v
                        [(_, fn)] -> file fn
                        _ -> error "Should never happen" -- packageCollection should have been filtered to have only one version per package

        -- html API

        -- css stylesheet
        get "/styles.css" $ do
            setHeader "Content-Type" "text/css; charset=utf-8"
            raw stylesBS
        -- landing page
        get "/" $ do
            redirect ("/explorer")
        get "/explorer" $ do
            logRequest logA
            pacsPerArchive <- forM archiveSpecs $ \spec -> do
                let n = _archSpecName spec
                    d = _archSpecDescription spec
                    u = _archSpecURL spec
                pacs <- selectLatest <$> getArchiveByName n archiveStore
                return (n, d, u, pacs)
            mainPage pacsPerArchive
        -- archive pages
        get "/explorer/:archive_name" $ do
            logRequest logA
            archiveName <- captureParam "archive_name"
            latestPacs  <- selectLatest <$> getArchiveByName archiveName archiveStore
            let mapMarkers = concatMap (prepMappable archiveName) latestPacs
            archivePage archiveName mapMarkers latestPacs
        -- per package pages
        get "/explorer/:archive_name/:package_name" $ do
            archive_name <- captureParam "archive_name"
            package_name <- captureParam "package_name"
            redirect ("/explorer/" <> archive_name <> "/" <> package_name <> "/latest")
        get "/explorer/:archive_name/:package_name/:package_version" $ do
            logRequest logA
            archiveName      <- captureParam "archive_name"
            pacName          <- captureParam "package_name"
            pacVersionString <- captureParam "package_version"
            pacVersionWL <- case parsePackageVersionString pacVersionString of
                Nothing -> fail $ "Could not parse package version string " ++ pacVersionString
                Just v -> return v
            allPacs     <- getArchiveByName archiveName archiveStore
            allVersions <- prepPacVersions pacName allPacs
            oneVersion  <- prepPacVersion pacVersionWL allVersions
            let mapMarkers = prepMappable archiveName oneVersion
                bib = intercalate "\n" $ map renderBibEntry $ posPacBib oneVersion
                pacVersion = getPacVersion oneVersion
            samples <- prepSamples oneVersion
            packageVersionPage archiveName pacName pacVersion mapMarkers bib oneVersion allVersions samples
        -- per sample pages
        get "/explorer/:archive_name/:package_name/:package_version/:sample" $ do
            logRequest logA
            archiveName <- captureParam "archive_name"
            allPacs <- getArchiveByName archiveName archiveStore
            pacName <- captureParam "package_name"
            allVersions <- prepPacVersions pacName allPacs
            pacVersionString <- captureParam "package_version"
            pacVersionWL <- case parsePackageVersionString pacVersionString of
                    Nothing -> fail $ "Could not parse package version string " ++ pacVersionString
                    Just v -> return v
            oneVersion <- prepPacVersion pacVersionWL allVersions
            let pacVersion = showVersion <$> getPacVersion oneVersion
            samples <- prepSamples oneVersion
            sampleName <- captureParam "sample"
            sample <- prepSample sampleName samples
            let maybeMapMarker = extractPosJannoRow archiveName pacName pacVersion sample
            samplePage maybeMapMarker sample

        -- catch anything else
        notFound $ fail "Unknown request"

-- prepare data for the html API

data PacVersion =
      Latest
    | NumericalVersion Version

instance Show PacVersion where
  show Latest               = "latest"
  show (NumericalVersion v) = showVersion v

selectLatest :: [PoseidonPackage] -> [PoseidonPackage]
selectLatest =
      map last
    . groupBy (\a b -> posPacNameAndVersion a == posPacNameAndVersion b)
    . sortOn posPacNameAndVersion

prepMappable :: String -> PoseidonPackage -> [MapMarker]
prepMappable archiveName pac =
    let packageName = getPacName pac
        packageVersion = showVersion <$> getPacVersion pac
        jannoRows = getJannoRows . posPacJanno $ pac
    in mapMaybe (extractPosJannoRow archiveName packageName packageVersion) jannoRows

extractPosJannoRow :: String -> String -> Maybe String -> JannoRow -> Maybe MapMarker
extractPosJannoRow archiveName pacName pacVersion row = case (jLatitude row, jLongitude row) of
    (Just (JannoLatitude lat), Just (JannoLongitude lon)) ->
        let poseidonID = jPoseidonID row
            loc = show <$> jLocation row
            age = show <$> jDateBCADMedian row
        in Just $ MapMarker lat lon poseidonID pacName pacVersion archiveName loc age
    _                                                     -> Nothing

prepPacVersions :: String -> [PoseidonPackage] -> ActionM [PoseidonPackage]
prepPacVersions pacName pacs = do
    case filter (\pac -> getPacName pac == pacName) pacs of
       [] -> fail $ "Package " <> pacName <> " does not exist"
       xs -> return xs

prepPacVersion :: PacVersion -> [PoseidonPackage] -> ActionM PoseidonPackage
prepPacVersion pacVersion pacs = do
    case pacVersion of
        Latest -> return $ head $ selectLatest pacs
        NumericalVersion v -> case filter (\pac -> getPacVersion pac == Just v) pacs of
            [] -> fail $ "Package version " <> (show pacVersion) <> " does not exist"
            [x] -> return x
            _ -> fail $ "Package version " <> (show pacVersion) <> " exists multiple times"

prepSamples :: PoseidonPackage -> ActionM [JannoRow]
prepSamples pac = return $ getJannoRowsFromPac pac

prepSample :: String -> [JannoRow] -> ActionM JannoRow
prepSample sampleName rows = do
    case filter (\r -> jPoseidonID r == sampleName) rows of
       []  -> fail $ "Sample " <> sampleName <> " does not exist"
       [x] -> return x
       _   -> fail $ "Sample " <> sampleName <> " exists multiple times"

readArchiveStore :: ArchiveConfig -> PackageReadOptions -> PoseidonIO (ArchiveStore [PoseidonPackage])
readArchiveStore (ArchiveConfig archiveSpecs) pacReadOpts = do
    forM archiveSpecs $ \spec -> do
        logInfo $ "Loading packages for archive " ++ _archSpecName spec
        let relevantDirs = _archSpecPaths spec
        pacs <- readPoseidonPackageCollection pacReadOpts relevantDirs
        return (spec, pacs)

createZipArchiveStore :: ArchiveStore [PoseidonPackage] -> FilePath -> PoseidonIO (ArchiveStore ZipStore)
createZipArchiveStore archiveStore zipPath =
    forM archiveStore $ \(spec, packages) -> do
        logInfo $ "Zipping packages in archive " ++ _archSpecName spec
        (spec,) <$> forM packages (\pac -> do
            logInfo "Checking whether zip files are missing or outdated"
            liftIO $ createDirectoryIfMissing True (zipPath </> _archSpecName spec)
            let combinedPackageVersionTitle = renderNameWithVersion pac
            let fn = zipPath </> _archSpecName spec </> combinedPackageVersionTitle <.> "zip"
            zipFileOutdated <- liftIO $ checkZipFileOutdated pac fn
            when zipFileOutdated $ do
                logInfo ("Zip Archive for package " ++ combinedPackageVersionTitle ++ " missing or outdated. Zipping now")
                zip_ <- liftIO $ makeZipArchive pac
                let zip_raw = fromArchive zip_
                liftIO $ B.writeFile fn zip_raw
            return (posPacNameAndVersion pac, fn))

-- this serves as a point to broadcast messages to clients. Adapt in the future as necessary.
genericServerMessages :: [String]
genericServerMessages = ["Greetings from the Poseidon Server, version " ++ showVersion version]


-- other helper functions

parsePackageVersionString :: String -> Maybe PacVersion
parsePackageVersionString vStr = case vStr of
    "" -> Just Latest
    "latest" -> Just Latest
    x -> case filter ((=="") . snd) $ readP_to_S parseVersion x of
        [(v, "")] -> Just $ NumericalVersion v
        _         -> Nothing

parseVersionString :: String -> Maybe Version
parseVersionString vStr = case filter ((=="") . snd) $ readP_to_S parseVersion vStr of
    [(v', "")] -> Just v'
    _          -> Nothing

conditionOnClientVersion :: ActionM ServerApiReturnType -> ActionM ()
conditionOnClientVersion contentAction = do
    maybeClientVersion <- queryParamMaybe "client_version"
    (clientVersion, versionWarnings) <- case maybeClientVersion of
        Nothing            -> return (version, ["No client_version passed. Assuming latest version " ++ showVersion version])
        Just versionString -> case parseVersionString versionString of
            Just v -> return (v, [])
            Nothing -> return (version, ["Could not parse Client Version string " ++ versionString ++ ", assuming latest version " ++ showVersion version])
    if clientVersion < minimalRequiredClientVersion then do
        let msg = "This Server API requires trident version at least " ++ show minimalRequiredClientVersion ++
                "Please go to https://poseidon-framework.github.io/#/trident and update your trident installation."
        json $ ServerApiReturnType (genericServerMessages ++ versionWarnings ++ [msg]) Nothing
    else do
        ServerApiReturnType messages content <- contentAction
        json $ ServerApiReturnType (genericServerMessages ++ versionWarnings ++ messages) content

checkZipFileOutdated :: PoseidonPackage -> FilePath -> IO Bool
checkZipFileOutdated pac fn = do
    zipFileExists <- doesFileExist fn
    if zipFileExists
    then do
        zipModTime <- getModificationTime fn
        yamlOutdated <- checkOutdated zipModTime (posPacBaseDir pac </> "POSEIDON.yml")
        bibOutdated <- case posPacBibFile pac of
            Just fn_ -> checkOutdated zipModTime (posPacBaseDir pac </> fn_)
            Nothing  -> return False
        jannoOutdated <- case posPacJannoFile pac of
            Just fn_ -> checkOutdated zipModTime (posPacBaseDir pac </> fn_)
            Nothing  -> return False
        readmeOutdated <- case posPacReadmeFile pac of
            Just fn_ -> checkOutdated zipModTime (posPacBaseDir pac </> fn_)
            Nothing  -> return False
        changelogOutdated <- case posPacChangelogFile pac of
            Just fn_ -> checkOutdated zipModTime (posPacBaseDir pac </> fn_)
            Nothing  -> return False
        ssfOutdated <- case posPacSeqSourceFile pac of
            Just fn_ -> checkOutdated zipModTime (posPacBaseDir pac </> fn_)
            Nothing  -> return False
        let gd = posPacGenotypeData pac
        genoFilesOutdated <- case  genotypeFileSpec gd of
            GenotypeEigenstrat gf _ sf _ i _ -> mapM (checkOutdated zipModTime . (posPacBaseDir pac </>)) [gf, sf, i]
            GenotypePlink gf _ sf _ i _      -> mapM (checkOutdated zipModTime . (posPacBaseDir pac </>)) [gf, sf, i]
            GenotypeVCF gf _                 -> mapM (checkOutdated zipModTime . (posPacBaseDir pac </>)) [gf]
        return . or $ [yamlOutdated, bibOutdated, jannoOutdated, readmeOutdated, changelogOutdated, ssfOutdated] ++ genoFilesOutdated
    else
        return True
  where
    checkOutdated zipModTime fn_ = (> zipModTime) <$> getModificationTime fn_

makeZipArchive :: PoseidonPackage -> IO Archive
makeZipArchive pac =
    addYaml emptyArchive >>= addJanno >>= addBib >>= addReadme >>= addChangelog >>= addGenos >>= addSSF
  where
    addYaml      = addFN "POSEIDON.yml"
    addJanno     = maybe return addFN (posPacJannoFile pac)
    addBib       = maybe return addFN (posPacBibFile pac)
    addReadme    = maybe return addFN (posPacReadmeFile pac)
    addChangelog = maybe return addFN (posPacChangelogFile pac)
    addSSF       = maybe return addFN (posPacSeqSourceFile pac)
    addGenos archive = case genotypeFileSpec . posPacGenotypeData $ pac of
        GenotypeEigenstrat gf _ sf _ i _ -> foldM (flip addFN) archive [gf, sf, i]
        GenotypePlink gf _ sf _ i _      -> foldM (flip addFN) archive [gf, sf, i]
        GenotypeVCF gf _                 -> addFN gf archive
    addFN :: FilePath -> Archive -> IO Archive
    addFN fn a = do
        let fullFN = posPacBaseDir pac </> fn
        rawFN <- B.readFile fullFN
        modTime <- round . utcTimeToPOSIXSeconds <$> getModificationTime fullFN
        let zipEntry = toEntry fn modTime rawFN
        return (addEntryToArchive zipEntry a)

scottyHTTPS :: MVar () -> Int -> FilePath -> [FilePath] -> FilePath -> ScottyM () -> PoseidonIO ()
scottyHTTPS serverReady port cert chains key s = do
    -- this is just the same output as with scotty, to make it consistent whether or not using https
    logInfo $ "Server now listening via HTTPS on " ++ show port
    let tsls = case chains of
            [] -> tlsSettings cert key
            c  -> tlsSettingsChain cert c key
        settings = setPort port . setBeforeMainLoop (putMVar serverReady ()) $ defaultSettings
    liftIO $ do
        app <- liftIO $ scottyApp s
        runTLS tsls settings app

scottyHTTP :: MVar () -> Int -> ScottyM () -> PoseidonIO ()
scottyHTTP serverReady port s = do
    logInfo $ "Server now listening via HTTP on " ++ show port
    let settings = setPort port . setBeforeMainLoop (putMVar serverReady ()) $ defaultSettings
    liftIO $ do
        app <- scottyApp s
        runSettings settings app

logRequest :: LogA -> ActionM ()
logRequest logA = do
    req <- request
    let p = pathInfo req
        q = queryString req
    liftIO . logWithEnv logA . logDebug $ "Request: Path=" ++ show p ++ ", qstring=" ++ show q

getItemFromArchiveStore :: ArchiveStore a -> ActionM a
getItemFromArchiveStore store = do
    maybeArchiveName <- queryParamMaybe "archive"
    case maybeArchiveName of
        Nothing   -> return . snd . head $ store
        Just name -> getArchiveByName name store
