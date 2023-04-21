{-# LANGUAGE OverloadedStrings #-}

import           Poseidon.CLI.OptparseApplicativeParsers (parseInputPlinkPopMode)
import           Poseidon.GenotypeData                   (GenotypeDataSpec (..))
import           Poseidon.Janno                          (JannoRow (..),
                                                          JannoRows (..))
import           Poseidon.Package                        (PackageReadOptions (..),
                                                          PoseidonPackage (..),
                                                          defaultPackageReadOptions,
                                                          getJannoRowsFromPac,
                                                          readPoseidonPackageCollection,
                                                          getJointIndividualInfo,
                                                          packageToPackageInfo)
import           Poseidon.SecondaryTypes                 (ApiReturnData (..),
                                                          GroupInfo (..),
                                                          IndividualInfo (..),
                                                          PackageInfo (..),
                                                          ServerApiReturnType (..))
import           Poseidon.Utils                          (LogMode (..),
                                                          PoseidonIO, logInfo,
                                                          usePoseidonLogger)

import           Codec.Archive.Zip                       (Archive,
                                                          addEntryToArchive,
                                                          emptyArchive,
                                                          fromArchive, toEntry)
import           Control.Applicative                     (optional)
import           Control.Monad                           (forM, unless, when)
import           Control.Monad.IO.Class                  (liftIO)
import qualified Data.ByteString.Char8                   as BSC
import qualified Data.ByteString.Lazy                    as B
import           Data.Csv                                (NamedRecord,
                                                          toNamedRecord)
import qualified Data.HashMap.Strict                     as HM
import           Data.List                               (group, nub, sortOn)
import           Data.List.Split                         (splitOn)
import           Data.Maybe                              (isJust)
import           Data.Text.Lazy                          (pack)
import           Data.Time.Clock.POSIX                   (utcTimeToPOSIXSeconds)
import           Data.Version                            (Version, makeVersion,
                                                          parseVersion,
                                                          showVersion)
import           Network.Wai.Handler.Warp                (defaultSettings, run,
                                                          setPort)
import           Network.Wai.Handler.WarpTLS             (runTLS, tlsSettings,
                                                          tlsSettingsChain)
import           Network.Wai.Middleware.Cors             (simpleCors)
import qualified Options.Applicative                     as OP
import           Paths_poseidon_hs                       (version)
import           SequenceFormats.Plink                   (PlinkPopNameMode)
import           System.Directory                        (createDirectoryIfMissing,
                                                          doesFileExist,
                                                          getModificationTime)
import           System.FilePath                         ((<.>), (</>))
import           Text.ParserCombinators.ReadP            (readP_to_S)
import           Web.Scotty                              (ActionM, ScottyM,
                                                          file, get, json,
                                                          middleware, notFound,
                                                          param, raise, rescue,
                                                          scottyApp, text)

data CommandLineOptions = CommandLineOptions
    { cliBaseDirs        :: [FilePath]
    , cliZipDir          :: Maybe FilePath
    , cliPort            :: Int
    , cliIgnoreChecksums :: Bool
    , cliCertFiles       :: Maybe (FilePath, [FilePath], FilePath)
    , cliPlinkPopMode    :: PlinkPopNameMode
    }
    deriving (Show)

main :: IO ()
main = do
    CommandLineOptions baseDirs maybeZipPath port ignoreChecksums certFiles plinkMode <-
        OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo

    usePoseidonLogger VerboseLog plinkMode $ do
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
                let combinedPackageVersionTitle = case posPacPackageVersion pac of
                        Nothing -> posPacTitle pac
                        Just v -> posPacTitle pac ++ "-" ++ showVersion v
                let fn = zipPath </> combinedPackageVersionTitle <.> "zip"
                zipFileOutdated <- liftIO $ checkZipFileOutdated pac fn
                when zipFileOutdated $ do
                    logInfo ("Zip Archive for package " ++ combinedPackageVersionTitle ++ " missing or outdated. Zipping now")
                    zip_ <- liftIO $ makeZipArchive pac
                    let zip_raw = fromArchive zip_
                    liftIO $ B.writeFile fn zip_raw
                return ((posPacTitle pac, posPacPackageVersion pac), fn))

        let runScotty = case certFiles of
                Nothing                              -> scottyHTTP  port
                Just (certFile, chainFiles, keyFile) -> scottyHTTPS port certFile chainFiles keyFile

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
                let additionalColumnEntries = case maybeAdditionalColumnsString of
                        Just additionalColumnsString ->
                            let additionalColumnNames = splitOn "," additionalColumnsString
                                getEntries hm = [(k, BSC.unpack <$> hm HM.!? (BSC.pack k)) | k <- additionalColumnNames]
                                jannoRows = (concat . map ((\(JannoRows jr) -> jr) . snd) . getAllPacJannoPairs $ allPackages) :: [JannoRow]
                                namedRecords = map toNamedRecord jannoRows :: [NamedRecord]
                            in  Just $ map getEntries namedRecords
                        Nothing -> Nothing
                let packageVersions = map posPacPackageVersion allPackages
                let indInfos = getJointIndividualInfo allPackages
                let retData = ApiReturnIndividualInfo indInfos packageVersions additionalColumnEntries 
                return $ ServerApiReturnType [] (Just retData)

            get "/janno" . conditionOnClientVersion $ do
                let retData = ApiReturnJanno . getAllPacJannoPairs $ allPackages
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
                case reverse . sortOn (snd . fst) . filter ((==packageName) . fst . fst) $ zipDict of
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
        json $ ServerApiReturnType (versionWarnings ++ [msg]) Nothing
    else do
        ServerApiReturnType messages content <- contentAction
        json $ ServerApiReturnType (versionWarnings ++ messages) content

minimalRequiredClientVersion :: Version
minimalRequiredClientVersion = makeVersion [1, 1, 8, 5]

parseVersionString :: String -> Maybe Version
parseVersionString vStr = case filter ((=="") . snd) $ readP_to_S parseVersion vStr of
    [(v', "")] -> Just v'
    _          -> Nothing

optParserInfo :: OP.ParserInfo CommandLineOptions
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "poseidon-http-server is a HTTP Server to provide information about \
        \Poseidon package repositories. \
        \More information: \
        \https://github.com/poseidon-framework/poseidon-hs. \
        \Report issues: \
        \https://github.com/poseidon-framework/poseidon-hs/issues")

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser CommandLineOptions
optParser = CommandLineOptions <$> parseBasePaths <*> parseMaybeZipDir <*> parsePort <*>
    parseIgnoreChecksums <*> parseMaybeCertFiles <*> parseInputPlinkPopMode

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages"))

parseMaybeZipDir :: OP.Parser (Maybe FilePath)
parseMaybeZipDir = OP.option (Just <$> OP.str) (OP.long "zipDir" <>
    OP.short 'z' <>
    OP.metavar "DIR" <>
    OP.help "a directory to store Zip files in. If not specified, do not generate zip files" <>
    OP.value Nothing)

parsePort :: OP.Parser Int
parsePort = OP.option OP.auto (OP.long "port" <> OP.short 'p' <> OP.metavar "PORT" <>
    OP.value 3000 <> OP.showDefault <>
    OP.help "the port on which the server listens")

parseIgnoreChecksums :: OP.Parser Bool
parseIgnoreChecksums = OP.switch (OP.long "ignoreChecksums" <> OP.short 'c' <>
    OP.help "whether to ignore checksums. Useful for speedup in debugging")

parseMaybeCertFiles :: OP.Parser (Maybe (FilePath, [FilePath], FilePath))
parseMaybeCertFiles = optional parseFiles
  where
    parseFiles = (,,) <$> parseCertFile <*> OP.many parseChainFile <*> parseKeyFile

parseKeyFile :: OP.Parser FilePath
parseKeyFile = OP.strOption (OP.long "keyFile" <> OP.metavar "KEYFILE" <>
                             OP.help "The key file of the TLS Certificate used for HTTPS")

parseChainFile :: OP.Parser FilePath
parseChainFile = OP.strOption (OP.long "chainFile" <> OP.metavar "CHAINFILE" <>
                               OP.help "The chain file of the TLS Certificate used for HTTPS. Can be given multiple times")

parseCertFile :: OP.Parser FilePath
parseCertFile = OP.strOption (OP.long "certFile" <> OP.metavar "CERTFILE" <>
                              OP.help "The cert file of the TLS Certificate used for HTTPS")

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
        let gd = posPacGenotypeData pac
        genoOutdated <- checkOutdated zipModTime (posPacBaseDir pac </> genoFile gd)
        snpOutdated <- checkOutdated zipModTime (posPacBaseDir pac </> snpFile gd)
        indOutdated <- checkOutdated zipModTime (posPacBaseDir pac </> indFile gd)
        return $ or [yamlOutdated, bibOutdated, jannoOutdated, readmeOutdated, changelogOutdated, genoOutdated, snpOutdated, indOutdated]
    else
        return True
  where
    checkOutdated zipModTime fn_ = (> zipModTime) <$> getModificationTime fn_

makeZipArchive :: PoseidonPackage -> IO Archive
makeZipArchive pac =
    addYaml emptyArchive >>= addJanno >>= addBib >>= addReadme >>= addChangelog >>= addInd >>= addSnp >>= addGeno
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

scottyHTTPS :: Int -> FilePath -> [FilePath] -> FilePath -> ScottyM () -> PoseidonIO ()
scottyHTTPS port cert chains key s = do
    -- this is just the same output as with scotty, to make it consistent whether or not using https
    logInfo $ "Server now listening via HTTPS on " ++ show port
    let tsls = case chains of
            [] -> tlsSettings cert key
            c  -> tlsSettingsChain cert c key
    liftIO $ do
        app <- liftIO $ scottyApp s
        runTLS tsls (setPort port defaultSettings) app

scottyHTTP :: Int -> ScottyM () -> PoseidonIO ()
scottyHTTP port s = do
    logInfo $ "Server now listening via HTTP on " ++ show port
    liftIO $ do
        app <- scottyApp s
        run port app

getAllPacJannoPairs :: [PoseidonPackage] -> [(String, JannoRows)]
getAllPacJannoPairs packages = [(posPacTitle pac, posPacJanno pac) | pac <- packages]
