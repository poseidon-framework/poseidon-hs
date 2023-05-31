{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Serve (runServer, runServerMainThread, ServeOptions(..)) where

import           Poseidon.GenotypeData        (GenotypeDataSpec (..))
import           Poseidon.Package             (PackageReadOptions (..),
                                               PoseidonPackage (..),
                                               defaultPackageReadOptions,
                                               getAllGroupInfo,
                                               getExtendedIndividualInfo,
                                               packageToPackageInfo,
                                               readPoseidonPackageCollection)
import           Poseidon.PoseidonVersion     (minimalRequiredClientVersion)
import           Poseidon.SecondaryTypes      (ApiReturnData (..),
                                               ServerApiReturnType (..))
import           Poseidon.Utils               (PoseidonIO,
                                               extendNameWithVersion, logInfo)

import           Codec.Archive.Zip            (Archive, addEntryToArchive,
                                               emptyArchive, fromArchive,
                                               toEntry)
import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar)
import           Control.Monad                (forM, unless, when)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString.Lazy         as B
import           Data.List                    (sortOn)
import           Data.List.Split              (splitOn)
import           Data.Maybe                   (isJust)
import           Data.Ord                     (Down (..))
import           Data.Text.Lazy               (pack)
import           Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)
import           Data.Version                 (Version, makeVersion,
                                               parseVersion, showVersion)
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
                                               param, raise, rescue, scottyApp,
                                               text)
data ServeOptions = ServeOptions
    { cliBaseDirs        :: [FilePath]
    , cliZipDir          :: Maybe FilePath
    , cliPort            :: Int
    , cliIgnoreChecksums :: Bool
    , cliCertFiles       :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving (Show)

runServerMainThread :: ServeOptions -> PoseidonIO ()
runServerMainThread opts = do
    -- the MVar is used as a signal from the server to the calling thread that it is ready.
    -- It is used for testing. Here we just use it as a dummy.
    dummy <- liftIO newEmptyMVar
    runServer opts dummy

runServer :: ServeOptions -> MVar () -> PoseidonIO ()
runServer (ServeOptions baseDirs maybeZipPath port ignoreChecksums certFiles) serverReady = do
    let pacReadOpts = defaultPackageReadOptions {
            _readOptStopOnDuplicates = False
            , _readOptIgnoreChecksums  = ignoreChecksums
            , _readOptGenoCheck        = isJust maybeZipPath
            , _readOptKeepMultipleVersions = True
        }

    logInfo "Server starting up. Loading packages..."
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs

    zipDict <- case maybeZipPath of
        Nothing -> return []
        Just zipPath -> forM allPackages (\pac -> do
            logInfo "Checking whether zip files are missing or outdated"
            liftIO $ createDirectoryIfMissing True zipPath
            let combinedPackageVersionTitle = extendNameWithVersion (posPacTitle pac) (posPacPackageVersion pac)
            let fn = zipPath </> combinedPackageVersionTitle <.> "zip"
            zipFileOutdated <- liftIO $ checkZipFileOutdated pac fn
            when zipFileOutdated $ do
                logInfo ("Zip Archive for package " ++ combinedPackageVersionTitle ++ " missing or outdated. Zipping now")
                zip_ <- liftIO $ makeZipArchive pac
                let zip_raw = fromArchive zip_
                liftIO $ B.writeFile fn zip_raw
            return ((posPacTitle pac, posPacPackageVersion pac), fn))

    let runScotty = case certFiles of
            Nothing                              -> scottyHTTP  serverReady port
            Just (certFile, chainFiles, keyFile) -> scottyHTTPS serverReady port certFile chainFiles keyFile

    runScotty $ do
        middleware simpleCors

        get "/server_version" $
            text . pack . showVersion $ version

        get "/packages" . conditionOnClientVersion $ do
            let retData = ApiReturnPackageInfo . map packageToPackageInfo $ allPackages
            return $ ServerApiReturnType [] (Just retData)

        get "/groups" . conditionOnClientVersion $ do
            let retData = ApiReturnGroupInfo . getAllGroupInfo $ allPackages
            return $ ServerApiReturnType [] (Just retData)

        get "/individuals" . conditionOnClientVersion $ do
            maybeAdditionalColumnsString <- (Just <$> param "additionalJannoColumns") `rescue` (\_ -> return Nothing)

            let (indInfo, pacVersions, additionalColumnEntries) = case maybeAdditionalColumnsString of
                    Just additionalColumnsString ->
                        let additionalColumnNames = splitOn "," additionalColumnsString
                        in  getExtendedIndividualInfo allPackages additionalColumnNames
                    Nothing -> getExtendedIndividualInfo allPackages []
            let retData = ApiReturnIndividualInfo indInfo pacVersions additionalColumnEntries
            return $ ServerApiReturnType [] (Just retData)

        -- API for retreiving package zip files
        unless (null zipDict) . get "/zip_file/:package_name" $ do
            packageName <- param "package_name"
            maybeVersionString <- (Just <$> param "package_version") `rescue` (\_ -> return Nothing)
            maybeVersion <- case maybeVersionString of
                Nothing -> return Nothing
                Just versionStr -> case parseVersionString versionStr of
                    Nothing -> raise . pack $ "Could not parse package version string " ++ versionStr
                    Just v -> return $ Just v
            case sortOn (Down . snd . fst) . filter ((==packageName) . fst . fst) $ zipDict of
                [] -> raise . pack $ "unknown package " ++ packageName
                [((_, pv), fn)] -> case maybeVersion of
                    Nothing -> file fn
                    Just v -> if pv == Just v then file fn else raise . pack $ "Package " ++ packageName ++ " is not available for version " ++ showVersion v
                pl@((_, fnLatest) : _) -> case maybeVersion of
                    Nothing -> file fnLatest
                    Just v -> case filter ((==Just v) . snd . fst) pl of
                        [] -> raise . pack $ "Package " ++ packageName ++ "is not available for version " ++ showVersion v
                        [(_, fn)] -> file fn
                        _ -> error "Should never happen" -- packageCollection should have been filtered to have only one version per package
        notFound $ raise "Unknown request"

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
        genoOutdated <- checkOutdated zipModTime (posPacBaseDir pac </> genoFile gd)
        snpOutdated <- checkOutdated zipModTime (posPacBaseDir pac </> snpFile gd)
        indOutdated <- checkOutdated zipModTime (posPacBaseDir pac </> indFile gd)
        return $ or [yamlOutdated, bibOutdated, jannoOutdated, readmeOutdated,
                     changelogOutdated, genoOutdated, snpOutdated, indOutdated,
                     ssfOutdated]
    else
        return True
  where
    checkOutdated zipModTime fn_ = (> zipModTime) <$> getModificationTime fn_

makeZipArchive :: PoseidonPackage -> IO Archive
makeZipArchive pac =
    addYaml emptyArchive >>= addJanno >>= addBib >>= addReadme >>= addChangelog >>= addInd >>= addSnp >>= addGeno >>= addSSF
  where
    addYaml = addFN "POSEIDON.yml" (posPacBaseDir pac)
    addJanno = case posPacJannoFile pac of
        Nothing -> return
        Just fn -> addFN fn (posPacBaseDir pac)
    addBib = case posPacBibFile pac of
        Nothing -> return
        Just fn -> addFN fn (posPacBaseDir pac)
    addReadme = case posPacReadmeFile pac of
        Nothing -> return
        Just fn -> addFN fn (posPacBaseDir pac)
    addChangelog = case posPacChangelogFile pac of
        Nothing -> return
        Just fn -> addFN fn (posPacBaseDir pac)
    addSSF = case posPacSeqSourceFile pac of
        Nothing -> return
        Just fn -> addFN fn (posPacBaseDir pac)
    addInd = addFN (indFile . posPacGenotypeData $ pac) (posPacBaseDir pac)
    addSnp = addFN (snpFile . posPacGenotypeData $ pac) (posPacBaseDir pac)
    addGeno = addFN (genoFile . posPacGenotypeData $ pac) (posPacBaseDir pac)
    addFN :: FilePath -> FilePath -> Archive -> IO Archive
    addFN fn baseDir a = do
        let fullFN = baseDir </> fn
        raw <- B.readFile fullFN
        modTime <- round . utcTimeToPOSIXSeconds <$> getModificationTime fullFN
        let zipEntry = toEntry fn modTime raw
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
