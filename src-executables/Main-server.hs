{-# LANGUAGE OverloadedStrings #-}

import           Poseidon.GenotypeData       (GenotypeDataSpec (..))
import           Poseidon.Package            (PoseidonPackage (..),
                                              readPoseidonPackageCollection)

import           Codec.Archive.Zip           (Archive, addEntryToArchive,
                                              emptyArchive, fromArchive,
                                              toEntry)
import           Control.Applicative         ((<|>))
import           Control.Monad               (forM, (<=<))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString.Lazy        as B
import           Data.Text.Lazy              (Text, intercalate, pack, unpack)
import           Data.Time                   (Day)
import           Data.Time.Clock.POSIX       (utcTimeToPOSIXSeconds)
import           Data.Version                (Version, showVersion)
import           Network.Wai.Handler.Warp    (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings,
                                              tlsSettingsChain)
import           Network.Wai.Middleware.Cors (simpleCors)
import qualified Options.Applicative         as OP
import           Paths_poseidon_hs           (version)
import           System.Directory            (doesFileExist,
                                              getModificationTime)
import           System.FilePath.Posix       ((<.>), (</>))
import           System.IO                   (hPutStrLn, stderr)
import           Web.Scotty                  (ScottyM, file, get, html, json,
                                              middleware, notFound, param,
                                              raise, scotty, scottyApp, text)

data CommandLineOptions = CommandLineOptions
    { cliBaseDirs        :: [FilePath]
    , cliPort            :: Int
    , cliIgnoreGenoFiles :: Bool
    , cliCertFiles       :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving (Show)

data PackageInfo = PackageInfo
    { pTitle        :: String
    , pVersion      :: Maybe Version
    , pDescription  :: Maybe String
    , pLastModified :: Maybe Day
    }

instance ToJSON PackageInfo where
    toJSON x = object [
        "title"        .= pTitle x,
        "version"      .= pVersion x,
        "description"  .= pDescription x,
        "lastModified" .= pLastModified x
        ]

packageToPackageInfo :: PoseidonPackage -> PackageInfo
packageToPackageInfo pac = PackageInfo {
    pTitle = posPacTitle pac,
    pVersion = posPacPackageVersion pac,
    pDescription = posPacDescription pac,
    pLastModified = posPacLastModified pac
}

main :: IO ()
main = do
    opts@(CommandLineOptions baseDirs port ignoreGenoFiles certFiles) <- OP.customExecParser p optParserInfo
    allPackages <- readPoseidonPackageCollection False ignoreGenoFiles baseDirs
    zipDict <- forM allPackages (\pac -> do
        let fn = posPacBaseDir pac <.> "zip"
        zipFileOutdated <- checkZipFileOutdated pac fn ignoreGenoFiles
        if zipFileOutdated
        then do
            hPutStrLn stderr ("Preparing Zip Archive for package " ++ posPacTitle pac)
            zip <- makeZipArchive pac ignoreGenoFiles
            let zip_raw = fromArchive zip
            B.writeFile fn zip_raw
        else
            hPutStrLn stderr ("Zip Archive still up to date for package " ++ posPacTitle pac)
        return (posPacTitle pac, fn))
    let runScotty = case certFiles of
            Nothing                  -> scotty port
            Just (certFile, chainFiles, keyFile) -> scottyTLS port certFile chainFiles keyFile
    runScotty $ do
        middleware simpleCors
        get "/packages" $
            (json . map packageToPackageInfo) allPackages
        get "/package_table" $
            html . makeHTMLtable $ allPackages
        get "/package_table_md.md" $
            text . makeMDtable $ allPackages
        get "/zip_file/:package_name" $ do
            p <- param "package_name"
            let zipFN = lookup (unpack p) zipDict
            case zipFN of
                Just fn -> file fn
                Nothing -> raise ("unknown package " <> p)
        notFound $ raise "Unknown request"
  where
    p = OP.prefs OP.showHelpOnEmpty

scottyTLS :: Int -> FilePath -> [FilePath] -> FilePath -> ScottyM () -> IO ()
scottyTLS port cert chains key s = do
    -- this is just the same output as with scotty, to make it consistent whether or not using https
    putStrLn $ "Setting phasers to stun... (port " ++ show port ++ ") (ctrl-c to quit)"
    let tsls = case chains of
            [] -> tlsSettings cert key
            c  -> tlsSettingsChain cert c key
    app <- scottyApp s
    runTLS tsls (setPort port defaultSettings) app

checkZipFileOutdated :: PoseidonPackage -> FilePath -> Bool -> IO Bool
checkZipFileOutdated pac fn ignoreGenoFiles = do
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
        let gd = posPacGenotypeData pac
        genoOutdated <- if ignoreGenoFiles then return False else checkOutdated zipModTime (posPacBaseDir pac </> genoFile gd)
        snpOutdated <- if ignoreGenoFiles then return False else checkOutdated zipModTime (posPacBaseDir pac </> snpFile gd)
        indOutdated <- if ignoreGenoFiles then return False else checkOutdated zipModTime (posPacBaseDir pac </> indFile gd)
        return $ or [yamlOutdated, bibOutdated, jannoOutdated, genoOutdated, snpOutdated, indOutdated]
    else
        return True
  where
    checkOutdated zipModTime fn = (> zipModTime) <$> getModificationTime fn

makeHTMLtable :: [PoseidonPackage] -> Text
makeHTMLtable packages = "<table>" <> header <> body <> "</table>"
  where
    header :: Text
    header = "<tr><th>Package Name</th><th>Description</th><th>Version</th><th>Last updated</th><th>Download</th></tr>"
    body :: Text
    body = intercalate "\n" $ do
        pac <- packages
        let (PackageInfo title version desc lastMod) = packageToPackageInfo pac
        let link = "<a href=\"http://c107-224.cloud.gwdg.de:3000/zip_file/" <> pack title <> "\">" <> pack title <> "</a>"
        return $ "<tr><td>" <> pack title <> "</td><td>" <>
            maybe "n/a" pack desc <> "</td><td>" <>
            maybe "n/a" (pack . showVersion) version <> "</td><td>" <>
            maybe "n/a" (pack . show) lastMod <> "</td><td>" <>
            link <> "</td></tr>"

makeMDtable :: [PoseidonPackage] -> Text
makeMDtable packages = header <> "\n" <> body <> "\n"
  where
    header :: Text
    header = "| Package Name | Description | Version | Last updated | Download |\n| --- | --- | --- | --- | --- |"
    body :: Text
    body = intercalate "\n" $ do
        pac <- packages
        let (PackageInfo title version desc lastMod) = packageToPackageInfo pac
        let link = "[" <> pack title <> "](http://c107-224.cloud.gwdg.de:3000/zip_file/" <> pack title <> ")"
        return $ "| " <> pack title <> " | " <>
            maybe "n/a" pack desc <> " | " <>
            maybe "n/a" (pack . showVersion) version <> " | " <>
            maybe "n/a" (pack . show) lastMod <> " | " <>
            link <> " | "


makeZipArchive :: PoseidonPackage -> Bool -> IO Archive
makeZipArchive pac ignoreGenoFiles =
    return emptyArchive >>= addYaml >>= addJanno >>= addBib >>= addInd >>= addSnp >>= addGeno
  where
    addYaml = addFN "POSEIDON.yml" (posPacBaseDir pac)
    addJanno = case posPacJannoFile pac of
        Nothing -> return
        Just fn -> addFN fn (posPacBaseDir pac)
    addBib = case posPacBibFile pac of
        Nothing -> return
        Just fn -> addFN fn (posPacBaseDir pac)
    addInd = addFN (indFile . posPacGenotypeData $ pac) (posPacBaseDir pac)
    addSnp = if ignoreGenoFiles
             then return
             else addFN (snpFile . posPacGenotypeData $ pac) (posPacBaseDir pac)
    addGeno = if ignoreGenoFiles
              then return
              else addFN (genoFile . posPacGenotypeData $ pac) (posPacBaseDir pac)
    addFN :: FilePath -> FilePath -> Archive -> IO Archive
    addFN fn baseDir a = do
        let fullFN = baseDir </> fn
        raw <- B.readFile fullFN
        modTime <- (round . utcTimeToPOSIXSeconds) <$> getModificationTime fullFN
        let zipEntry = toEntry fn modTime raw
        return (addEntryToArchive zipEntry a)


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
optParser = CommandLineOptions <$> parseBasePaths <*> parsePort <*> parseIgnoreGenoFiles <*> parseMaybeCertFiles

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages (could be a Poseidon repository)"))

parsePort :: OP.Parser Int
parsePort = OP.option OP.auto (OP.long "port" <> OP.short 'p' <> OP.metavar "PORT" <>
    OP.value 3000 <> OP.showDefault <>
    OP.help "the port on which the server listens")

parseIgnoreGenoFiles :: OP.Parser Bool
parseIgnoreGenoFiles = OP.switch (OP.long "ignoreGenoFiles" <> OP.short 'i' <>
    OP.help "whether to ignore the bed and SNP files. Useful for debugging")

parseMaybeCertFiles :: OP.Parser (Maybe (FilePath, [FilePath], FilePath))
parseMaybeCertFiles = (Just <$> parseFiles) <|> pure Nothing
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
