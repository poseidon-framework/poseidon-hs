{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.EntitiesList   (EntitiesList, PoseidonEntity (..),
                                          readEntitiesFromFile)
import           Poseidon.MathHelpers    (roundTo, roundToStr)
import           Poseidon.Package        (PackageReadOptions (..),
                                          PoseidonPackage (..),
                                          defaultPackageReadOptions,
                                          readPoseidonPackageCollection)
import           Poseidon.SecondaryTypes (PackageInfo (..))
import           Poseidon.Utils          (PoseidonException (..))

import           Codec.Archive.Zip       (ZipOption (..),
                                          extractFilesFromArchive, toArchive)
import           Conduit                 (ResourceT, await, runResourceT,
                                          sinkFile, yield)
import           Control.Exception       (throwIO)
import           Control.Monad           (unless, when)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (eitherDecode')
import qualified Data.ByteString         as B
import           Data.ByteString.Char8   as B8 (unpack)
import qualified Data.ByteString.Lazy    as LB
import           Data.Conduit            (ConduitT, sealConduitT, ($$+-), (.|))
import           Data.List               (nub)
import           Data.Version            (Version, showVersion)
import           Network.HTTP.Conduit    (http, newManager, parseRequest,
                                          responseBody, responseHeaders,
                                          simpleHttp, tlsManagerSettings)
import           Network.HTTP.Types      (hContentLength)
import           System.Console.ANSI     (hClearLine, hSetCursorColumn)
import           System.Directory        (createDirectoryIfMissing,
                                          removeDirectory, removeFile)
import           System.FilePath         ((</>))
import           System.IO               (hFlush, hPutStr, hPutStrLn, stderr)

data FetchOptions = FetchOptions
    { _jaBaseDirs      :: [FilePath]
    , _entityList      :: EntitiesList
    , _entityFiles     :: [FilePath]
    , _remoteURL       :: String
    , _upgrade         :: Bool
    , _downloadAllPacs :: Bool
    }

data PackageState = NotLocal
    | EqualLocalRemote
    | LaterRemote
    | LaterLocal

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    }

-- | The main function running the Fetch command
runFetch :: FetchOptions -> IO ()
runFetch (FetchOptions baseDirs entitiesDirect entitiesFile remoteURL upgrade downloadAllPacs) = do
    let remote = remoteURL --"https://c107-224.cloud.gwdg.de"
        downloadDir = head baseDirs
        tempDir = downloadDir </> ".trident_download_folder"
    -- compile entities
    entitiesFromFile <- mapM readEntitiesFromFile entitiesFile
    let entities = nub $ entitiesDirect ++ concat entitiesFromFile --this nub could also be relevant for forge
        desiredPacsTitles = entities2PacTitles entities -- this whole mechanism can be replaced when the server also returns the individuals and groups in a package
    -- load local packages
    allLocalPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    -- load remote package list
    hPutStrLn stderr "Downloading package list from remote"
    remoteOverviewJSONByteString <- simpleHttp (remote ++ "/packages")
    allRemotePackages <- readPackageInfo remoteOverviewJSONByteString
    -- check which remote packages the User wants to have
    hPutStr stderr "Determine requested packages... "
    let desiredRemotePackages = if downloadAllPacs
                                then allRemotePackages
                                else filter (\x -> pTitle x `elem` desiredPacsTitles) allRemotePackages
    hPutStrLn stderr $ show (length desiredRemotePackages) ++ " requested and available"
    unless (null desiredRemotePackages) $ do
        -- perform package download depending on local-remote state
        hPutStrLn stderr "Comparing local and remote package state"
        let packagesWithState = map (determinePackageState allLocalPackages) desiredRemotePackages
        hPutStrLn stderr "Handling packages"
        createDirectoryIfMissing False tempDir
        mapM_ (handlePackageByState downloadDir tempDir remote upgrade) packagesWithState
        removeDirectory tempDir

entities2PacTitles :: [PoseidonEntity] ->  [String]
entities2PacTitles xs = do
    let pacEntities = [ x | x@Pac {} <- xs]
    map getEntityStrings pacEntities
    where
        getEntityStrings :: PoseidonEntity -> String
        getEntityStrings (Pac x)   = x
        getEntityStrings (Group x) = x
        getEntityStrings (Ind x)   = x

readPackageInfo :: LB.ByteString -> IO [PackageInfo]
readPackageInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right pac -> return pac

determinePackageState :: [PoseidonPackage] -> PackageInfo -> (PackageState, String, Maybe Version, Maybe Version)
determinePackageState localPacs desiredRemotePac
    | desiredRemotePacTitle `notElem` localPacsTitles =
        (NotLocal, desiredRemotePacTitle, desiredRemotePacVersion, localVersionOfDesired)
    | desiredRemotePacSimple `elem` localPacsSimple =
        (EqualLocalRemote, desiredRemotePacTitle, desiredRemotePacVersion, localVersionOfDesired)
    | localVersionOfDesired < desiredRemotePacVersion =
        (LaterRemote, desiredRemotePacTitle, desiredRemotePacVersion, localVersionOfDesired)
    | localVersionOfDesired > desiredRemotePacVersion =
        (LaterLocal, desiredRemotePacTitle, desiredRemotePacVersion, localVersionOfDesired)
    | otherwise = error "determinePackageState: should never happen"
    where
        desiredRemotePacTitle = pTitle desiredRemotePac
        desiredRemotePacVersion = pVersion desiredRemotePac
        desiredRemotePacSimple = (desiredRemotePacTitle, desiredRemotePacVersion)
        localPacsTitles = map posPacTitle localPacs
        localPacsVersion = map posPacPackageVersion localPacs
        localPacsSimple = zip localPacsTitles localPacsVersion
        localVersionOfDesired = snd $ head $ filter (\x -> fst x == desiredRemotePacTitle) localPacsSimple

handlePackageByState :: FilePath -> FilePath -> String -> Bool -> (PackageState, String, Maybe Version, Maybe Version) -> IO ()
handlePackageByState downloadDir tempDir remote _ (NotLocal, pac, _, _) = do
    downloadAndUnzipPackage downloadDir tempDir remote pac
handlePackageByState _ _ _ _ (EqualLocalRemote, pac, remoteV, localV) = do
    hPutStrLn stderr $ padString 40 pac ++
        "local " ++ printV localV ++ " = remote " ++ printV remoteV
handlePackageByState downloadDir tempDir remote upgrade (LaterRemote, pac, remoteV, localV) = do
    if upgrade
    then downloadAndUnzipPackage downloadDir tempDir remote pac
    else hPutStrLn stderr $ padString 40 pac ++
        "local " ++ printV localV ++ " < remote " ++ printV remoteV ++
        " (overwrite with --upgrade)"
handlePackageByState _ _ _ _ (LaterLocal, pac, remoteV, localV) = do
    hPutStrLn stderr $ padString 40 pac ++
        "local " ++ printV localV ++ " > remote " ++ printV remoteV

printV :: Maybe Version -> String
printV Nothing  = "?.?.?"
printV (Just x) = showVersion x

downloadAndUnzipPackage :: FilePath -> FilePath -> String -> String -> IO ()
downloadAndUnzipPackage baseDir tempDir remote pacName = do
    downloadPackage tempDir remote pacName
    unzipPackage (tempDir </> pacName) (baseDir </> pacName)
    removeFile (tempDir </> pacName)

unzipPackage :: FilePath -> FilePath -> IO ()
unzipPackage zip_ outDir = do
    archiveBS <- LB.readFile zip_
    let archive = toArchive archiveBS
    extractFilesFromArchive [OptRecursive, OptDestination outDir] archive

downloadPackage :: FilePath -> String -> String -> IO ()
downloadPackage pathToRepo remote pacName = do
    let paddedPacName = padString 40 pacName
    downloadManager <- newManager tlsManagerSettings
    packageRequest <- parseRequest (remote ++ "/zip_file/" ++ pacName)
    runResourceT $ do
        response <- http packageRequest downloadManager
        let Just fileSize = lookup hContentLength (responseHeaders response)
        let fileSizeKB = (read $ B8.unpack fileSize) :: Int
        let fileSizeMB = roundTo 1 (fromIntegral fileSizeKB / 1000.0 / 1000.0)
        liftIO $ hPutStrLn stderr (paddedPacName ++ "> " ++ show fileSizeMB ++ "MB to download")
        sealConduitT (responseBody response) $$+-
            printDownloadProgress fileSizeMB .|
            sinkFile (pathToRepo </> pacName)
    putStrLn ""
    return ()

padString :: Int -> String -> String
padString n s
    | length s > n  = take (n-1) s ++ " "
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

printDownloadProgress :: Double -> ConduitT B.ByteString B.ByteString (ResourceT IO) ()
printDownloadProgress fileSizeMB = loop 0 0
    where
        loop loadedB loadedMB = do
            x <- await
            maybe (return ()) (showDownloaded fileSizeMB loadedB) x
            where
                showDownloaded fileSizeMB_ loadedB_ x = do
                    let newLoadedB = loadedB_ + B.length x
                    let curLoadedMB = roundTo 1 (fromIntegral newLoadedB / 1000 / 1000)
                                          -- update progress counter every 5%
                    let newLoadedMB = if (curLoadedMB/fileSizeMB_ - loadedMB/fileSizeMB_ >= 0.05 &&
                                          -- but only at at least 200KB
                                          curLoadedMB - loadedMB > 0.2) ||
                                          -- and of course at the end of the sequence
                                          curLoadedMB == fileSizeMB_
                                      then curLoadedMB
                                      else loadedMB
                    when (loadedMB /= newLoadedMB) $ do
                        let leadedPercent = roundTo 3 (newLoadedMB / fileSizeMB_) * 100
                        liftIO $ hClearLine stderr
                        liftIO $ hSetCursorColumn stderr 0
                        liftIO $ hPutStr stderr ("> " ++ roundToStr 1 leadedPercent ++ "% ")
                        liftIO $ hFlush stderr
                    yield x
                    loop newLoadedB newLoadedMB
