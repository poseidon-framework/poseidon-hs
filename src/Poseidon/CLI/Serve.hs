{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Poseidon.CLI.Serve (runServer, runServerMainThread, ServeOptions(..)) where

import           Poseidon.EntityTypes         (HasNameAndVersion (..),
                                               PacNameAndVersion,
                                               renderNameWithVersion)
import           Poseidon.GenotypeData        (GenotypeDataSpec (..),
                                               GenotypeFileSpec (..))
import           Poseidon.Janno               (JannoRow (..))
import           Poseidon.Package             (PackageReadOptions (..),
                                               PoseidonPackage (..),
                                               defaultPackageReadOptions,
                                               getAllGroupInfo,
                                               getExtendedIndividualInfo,
                                               getJannoRowsFromPac,
                                               packagesToPackageInfos,
                                               readPoseidonPackageCollection)
import           Poseidon.PoseidonVersion     (minimalRequiredClientVersion)
import           Poseidon.ServerClient        (AddJannoColSpec (..),
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
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString.Lazy         as B
import           Data.List                    (groupBy, nub, sortOn)
import           Data.List.Split              (splitOn)
import           Data.Maybe                   (isJust)
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
import           System.Directory             (createDirectoryIfMissing,
                                               doesFileExist,
                                               getModificationTime)
import           System.FilePath              ((<.>), (</>))
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Web.Scotty                   (ActionM, ScottyM, file, get,
                                               json, middleware, notFound,
                                               param, raise, raw, request,
                                               rescue, scottyApp, setHeader,
                                               text)

data ServeOptions = ServeOptions
    { cliArchiveBaseDirs :: [(String, FilePath)]
    , cliZipDir          :: Maybe FilePath
    , cliPort            :: Int
    , cliIgnoreChecksums :: Bool
    , cliCertFiles       :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving (Show)

type ZipStore = [(PacNameAndVersion, FilePath)] -- maps PackageName+Version to a zipfile-path

type ArchiveName = String

type ArchiveStore a = [(ArchiveName, a)] -- a generic lookup table from an archive name to an item
-- we have two concrete ones: ArchiveStore [PoseidonPackage]   and     ArchiveStore ZipStore

getArchiveNames :: ArchiveStore a -> [String]
getArchiveNames = map fst

getArchiveByName :: ArchiveName -> ArchiveStore a -> Maybe a
getArchiveByName name store =
    case filter (\(n, _) -> n == name) store of
      []      -> Nothing
      [(_,a)] -> Just a
      _       -> Nothing

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
    archiveStore <- readArchiveStore archBaseDirs pacReadOpts

    logInfo $ "Using " ++ (fst . head) archiveStore ++ " as the default archive"

    zipArchiveStore <- case maybeZipPath of
        Nothing -> return []
        Just z  -> createZipArchiveStore archiveStore z

    let archiveNames = getArchiveNames archiveStore

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
            maybeAdditionalColumnsString <- (Just <$> param "additionalJannoColumns") `rescue` (\_ -> return Nothing)
            indInfo <- case maybeAdditionalColumnsString of
                    Just "ALL" -> getExtendedIndividualInfo pacs AddJannoColAll -- Nothing means all Janno Columns
                    Just additionalColumnsString ->
                        let additionalColumnNames = splitOn "," additionalColumnsString
                        in  getExtendedIndividualInfo pacs (AddJannoColList additionalColumnNames)
                    Nothing -> getExtendedIndividualInfo pacs (AddJannoColList [])
            let retData = ApiReturnExtIndividualInfo indInfo
            return $ ServerApiReturnType [] (Just retData)

        -- API for retreiving package zip files
        when (isJust maybeZipPath) . get "/zip_file/:package_name" $ do
            logRequest logA
            zipStore <- getItemFromArchiveStore zipArchiveStore
            packageName <- param "package_name"
            maybeVersionString <- (Just <$> param "package_version") `rescue` (\_ -> return Nothing)
            maybeVersion <- case maybeVersionString of
                Nothing -> return Nothing
                Just versionStr -> case parseVersionString versionStr of
                    Nothing -> raise . pack $ "Could not parse package version string " ++ versionStr
                    Just v -> return $ Just v
            case sortOn (Down . fst) . filter ((==packageName) . getPacName . fst) $ zipStore of
                [] -> raise . pack $ "unknown package " ++ packageName -- no version found
                [(pacNameAndVersion, fn)] -> case maybeVersion of -- exactly one version found
                    Nothing -> file fn
                    Just v -> if getPacVersion pacNameAndVersion == Just v then file fn else raise . pack $ "Package " ++ packageName ++ " is not available for version " ++ showVersion v
                pl@((_, fnLatest) : _) -> case maybeVersion of
                    Nothing -> file fnLatest
                    Just v -> case filter ((==Just v) . getPacVersion . fst) pl of
                        [] -> raise . pack $ "Package " ++ packageName ++ "is not available for version " ++ showVersion v
                        [(_, fn)] -> file fn
                        _ -> error "Should never happen" -- packageCollection should have been filtered to have only one version per package

        -- html API

        -- css stylesheet
        get "/styles.css" $ do
            setHeader "Content-Type" "text/css; charset=utf-8"
            raw stylesBS
        -- landing page
        get "/" $ do
            logRequest logA
            pacsPerArchive <- mapM (\n -> selectLatest <$> prepPacs n archiveStore) archiveNames
            mainPage archiveNames pacsPerArchive
        -- archive pages
        get "/:archive_name" $ do
            logRequest logA
            archiveName <- param "archive_name"
            allPacs <- prepPacs archiveName archiveStore
            archivePage archiveName allPacs
        -- per package pages
        get "/:archive_name/:package_name" $ do
            logRequest logA
            archiveName <- param "archive_name"
            allPacs <- prepPacs archiveName archiveStore
            pacName <- param "package_name"
            allVersions <- prepPacVersions pacName allPacs
            packagePage archiveName pacName allVersions
        -- per package version pages
        get "/:archive_name/:package_name/:package_version" $ do
            logRequest logA
            archiveName <- param "archive_name"
            allPacs <- prepPacs archiveName archiveStore
            pacName <- param "package_name"
            allVersions <- prepPacVersions pacName allPacs
            pacVersionString <- param "package_version"
            pacVersion <- case parseVersionString pacVersionString of
                    Nothing -> raise . pack $ "Could not parse package version string " ++ pacVersionString
                    Just v -> return v
            samples <- prepSamples pacVersion allVersions
            packageVersionPage archiveName pacName pacVersion samples
        -- per sample pages
        get "/:archive_name/:package_name/:package_version/:sample" $ do
            logRequest logA
            archiveName <- param "archive_name"
            allPacs <- prepPacs archiveName archiveStore
            pacName <- param "package_name"
            allVersions <- prepPacVersions pacName allPacs
            pacVersionString <- param "package_version"
            pacVersion <- case parseVersionString pacVersionString of
                    Nothing -> raise . pack $ "Could not parse package version string " ++ pacVersionString
                    Just v -> return v
            samples <- prepSamples pacVersion allVersions
            sampleName <- param "sample"
            sample <- prepSample sampleName samples
            samplePage sample

        -- catch anything else
        notFound $ raise "Unknown request"

prepPacs :: String -> ArchiveStore [PoseidonPackage] -> ActionM [PoseidonPackage]
prepPacs archiveName archiveStore = do
    let maybePacs = getArchiveByName archiveName archiveStore
    case maybePacs of
        Nothing -> raise $ "Archive " <> pack archiveName <> " does not exist"
        Just p  -> return p

selectLatest :: [PoseidonPackage] -> [PoseidonPackage]
selectLatest =
      map last
    . groupBy (\a b -> posPacNameAndVersion a == posPacNameAndVersion b)
    . sortOn posPacNameAndVersion

prepPacVersions :: String -> [PoseidonPackage] -> ActionM [PoseidonPackage]
prepPacVersions pacName pacs = do
    case filter (\pac -> getPacName pac == pacName) pacs of
       [] -> raise $ "Package " <> pack pacName <> " does not exist"
       xs -> return xs

prepSamples :: Version -> [PoseidonPackage] -> ActionM [JannoRow]
prepSamples pacVersion pacs = do
    case filter (\pac -> getPacVersion pac == Just pacVersion) pacs of
       [] -> raise $ "Package version " <> pack (showVersion pacVersion) <> " does not exist"
       [x] -> return $ getJannoRowsFromPac x
       _ -> raise $ "Package version " <> pack (showVersion pacVersion) <> " exists multiple times"

prepSample :: String -> [JannoRow] -> ActionM JannoRow
prepSample sampleName rows = do
    case filter (\r -> jPoseidonID r == sampleName) rows of
       []  -> raise $ "Sample " <> pack sampleName <> " does not exist"
       [x] -> return x
       _   -> raise $ "Sample " <> pack sampleName <> " exists multiple times"

readArchiveStore :: [(ArchiveName, FilePath)] -> PackageReadOptions -> PoseidonIO (ArchiveStore [PoseidonPackage])
readArchiveStore archBaseDirs pacReadOpts = do
    let archiveNames = nub . map fst $ archBaseDirs
    forM archiveNames $ \archiveName -> do
        logInfo $ "Loading packages for archive " ++ archiveName
        let relevantDirs = map snd . filter ((==archiveName) . fst) $ archBaseDirs
        pacs <- readPoseidonPackageCollection pacReadOpts relevantDirs
        return (archiveName, pacs)

createZipArchiveStore :: ArchiveStore [PoseidonPackage] -> FilePath -> PoseidonIO (ArchiveStore ZipStore)
createZipArchiveStore archiveStore zipPath =
    forM archiveStore $ \(archiveName, packages) -> do
        logInfo $ "Zipping packages in archive " ++ archiveName
        (archiveName,) <$> forM packages (\pac -> do
            logInfo "Checking whether zip files are missing or outdated"
            liftIO $ createDirectoryIfMissing True (zipPath </> archiveName)
            let combinedPackageVersionTitle = renderNameWithVersion pac
            let fn = zipPath </> archiveName </> combinedPackageVersionTitle <.> "zip"
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

parseVersionString :: String -> Maybe Version
parseVersionString vStr = case filter ((=="") . snd) $ readP_to_S parseVersion vStr of
    [(v', "")] -> Just v'
    _          -> Nothing

conditionOnClientVersion :: ActionM ServerApiReturnType -> ActionM ()
conditionOnClientVersion contentAction = do
    maybeClientVersion <- (Just <$> param "client_version") `rescue` (\_ -> return Nothing)
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
    maybeArchiveName <- (Just <$> param "archive") `rescue` (\_ -> return Nothing)
    case maybeArchiveName of
        Nothing -> return . snd . head $ store
        Just a -> case lookup a store of
            Nothing -> raise . pack $
                "The requested archive named " ++ a ++ " does not exist. Possible archives are " ++
                show (map fst store)
            Just pacs -> return pacs

