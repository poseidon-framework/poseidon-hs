{-# LANGUAGE OverloadedStrings #-}

import           Poseidon.GenotypeData (GenotypeDataSpec (..))
import           Poseidon.Package      (PoseidonPackage (..),
                                        readPoseidonPackageCollection)

import           Codec.Archive.Zip     (Archive, addEntryToArchive,
                                        emptyArchive, fromArchive, toEntry)
import           Control.Monad         (forM)
import           Data.Aeson            (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString.Lazy  as B
import           Data.Text.Lazy        (Text, intercalate, pack, unpack)
import           Data.Time             (Day)
import           Data.Version          (Version, showVersion)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Options.Applicative   as OP
import           Paths_poseidon_hs     (version)
import           System.Directory      (getModificationTime, doesFileExist)
import           System.FilePath.Posix ((<.>), (</>))
import           System.IO             (hPutStrLn, stderr)
import           Web.Scotty            (file, get, html, json, notFound, param,
                                        raise, scotty, text)

data CommandLineOptions = CommandLineOptions
    { cliBaseDirs        :: [FilePath]
    , cliPort            :: Int
    , cliIgnoreGenoFiles :: Bool
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
    opts@(CommandLineOptions baseDirs port ignoreGenoFiles) <- OP.customExecParser p optParserInfo
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
    scotty port $ do
        get "/packages" $
            (json . map packageToPackageInfo) allPackages
        get "/package_table" $
            html . makeHTMLtable $ allPackages
        get "/package_table_md" $
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
    header = "| Package Name | Description | Version | Last updated | Download |"
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
optParser = CommandLineOptions <$> parseBasePaths <*> parsePort <*> parseIgnoreGenoFiles

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
