{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.EntitiesList   (EntitiesList,
                                          readEntitiesFromFile, findNonExistentEntities, indInfoFindRelevantPackageNames)
import           Poseidon.MathHelpers    (roundTo, roundToStr)
import           Poseidon.Package        (PackageReadOptions (..),
                                          PoseidonPackage (..),
                                          defaultPackageReadOptions,
                                          readPoseidonPackageCollection)
import           Poseidon.SecondaryTypes (PackageInfo (..), IndividualInfo(..))
import           Poseidon.Utils          (PoseidonException (..), PoseidonLogIO)

import           Codec.Archive.Zip       (ZipOption (..),
                                          extractFilesFromArchive, toArchive)
import           Colog                   (logInfo, logWarning)
import           Conduit                 (ResourceT, await, runResourceT,
                                          sinkFile, yield)
import           Control.Exception       (throwIO)
import           Control.Monad           (unless, when, forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (eitherDecode')
import qualified Data.ByteString         as B
import           Data.ByteString.Char8   as B8 (unpack)
import qualified Data.ByteString.Lazy    as LB
import           Data.Conduit            (ConduitT, sealConduitT, ($$+-), (.|))
import           Data.List               (nub)
import           Data.Text               (pack)
import           Data.Version            (Version, showVersion)
import           Network.HTTP.Conduit    (http, newManager, parseRequest,
                                          responseBody, responseHeaders,
                                          simpleHttp, tlsManagerSettings)
import           Network.HTTP.Types      (hContentLength)
import           System.Console.ANSI     (hClearLine, hSetCursorColumn)
import           System.Directory        (createDirectoryIfMissing,
                                          removeDirectory, removeFile)
import           System.FilePath         ((</>))
import           System.IO               (hFlush, hPutStr, stderr)

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
runFetch :: FetchOptions -> PoseidonLogIO ()
runFetch (FetchOptions baseDirs entitiesDirect entitiesFile remoteURL upgrade downloadAllPacs) = do
    
    let remote = remoteURL --"https://c107-224.cloud.gwdg.de"
        downloadDir = head baseDirs
        tempDir = downloadDir </> ".trident_download_folder"
    
    -- compile entities
    entitiesFromFile <- liftIO $ mapM readEntitiesFromFile entitiesFile
    let entities = nub $ entitiesDirect ++ concat entitiesFromFile
    
    -- load remote package list
    logInfo "Downloading individual list from remote"
    remoteIndList <- liftIO $ simpleHttp (remote ++ "/individuals_all") >>= readServerIndInfo

    logInfo "Downloading package list from remote"
    remotePacList <- liftIO $ simpleHttp (remote ++ "/packages") >>= readServerPackageInfo
    
    let nonExistentEntities = findNonExistentEntities entities remoteIndList

    if (not . null) nonExistentEntities then do
        logWarning "Cannot find the following requested entities:"
        logWarning $ pack $ show nonExistentEntities
    else do
        -- load local packages
        allLocalPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
        -- check which remote packages the User wants to have
        logInfo "Determine requested packages... "
        let remotePacTitles = map pTitle remotePacList
        let desiredPacTitles =
                if downloadAllPacs then
                    remotePacTitles
                else
                    indInfoFindRelevantPackageNames entities remoteIndList
        
        let desiredRemotePackages = filter (\x -> pTitle x `elem` desiredPacTitles) remotePacList

        logInfo $ pack $ show (length desiredPacTitles) ++ " requested"
        unless (null desiredRemotePackages) $ do
            liftIO $ createDirectoryIfMissing False tempDir
            forM_ desiredRemotePackages $ \pac -> do
                -- perform package download depending on local-remote state
                logInfo $ pack $ "Comparing local and remote package " ++ pTitle pac
                let packageState = determinePackageState allLocalPackages pac
                -- create new directory
                logInfo $ pack $ "Downloading to directory (will be created if needed): " ++ downloadDir
                liftIO $ createDirectoryIfMissing True downloadDir
                handlePackageByState downloadDir tempDir remote upgrade packageState            
            liftIO $ removeDirectory tempDir

    logInfo "Done"

readServerIndInfo :: LB.ByteString -> IO [IndividualInfo]
readServerIndInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right pac -> return pac

readServerPackageInfo :: LB.ByteString -> IO [PackageInfo]
readServerPackageInfo bs = do
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

handlePackageByState :: FilePath -> FilePath -> String -> Bool -> (PackageState, String, Maybe Version, Maybe Version) -> PoseidonLogIO ()
handlePackageByState downloadDir tempDir remote _ (NotLocal, pac, _, _) = do
    downloadAndUnzipPackage downloadDir tempDir remote pac
handlePackageByState _ _ _ _ (EqualLocalRemote, pac, remoteV, localV) = do
    logInfo $ pack $ padString 40 pac ++
        "local " ++ printV localV ++ " = remote " ++ printV remoteV
handlePackageByState downloadDir tempDir remote upgrade (LaterRemote, pac, remoteV, localV) = do
    if upgrade
    then downloadAndUnzipPackage downloadDir tempDir remote pac
    else logInfo $ pack $ padString 40 pac ++
        "local " ++ printV localV ++ " < remote " ++ printV remoteV ++
        " (overwrite with --upgrade)"
handlePackageByState _ _ _ _ (LaterLocal, pac, remoteV, localV) = do
    logInfo $ pack $ padString 40 pac ++
        "local " ++ printV localV ++ " > remote " ++ printV remoteV

printV :: Maybe Version -> String
printV Nothing  = "?.?.?"
printV (Just x) = showVersion x

downloadAndUnzipPackage :: FilePath -> FilePath -> String -> String -> PoseidonLogIO ()
downloadAndUnzipPackage baseDir tempDir remote pacName = do
    logInfo $ pack $  padString 40 pacName
    liftIO $ downloadPackage tempDir remote pacName
    liftIO $ unzipPackage (tempDir </> pacName) (baseDir </> pacName)
    liftIO $ removeFile (tempDir </> pacName)

unzipPackage :: FilePath -> FilePath -> IO ()
unzipPackage zip_ outDir = do
    archiveBS <- LB.readFile zip_
    let archive = toArchive archiveBS
    extractFilesFromArchive [OptRecursive, OptDestination outDir] archive

downloadPackage :: FilePath -> String -> String -> IO ()
downloadPackage pathToRepo remote pacName = do
    downloadManager <- newManager tlsManagerSettings
    packageRequest <- parseRequest (remote ++ "/zip_file/" ++ pacName)
    runResourceT $ do
        response <- http packageRequest downloadManager
        let Just fileSize = lookup hContentLength (responseHeaders response)
        let fileSizeKB = (read $ B8.unpack fileSize) :: Int
        let fileSizeMB = roundTo 1 (fromIntegral fileSizeKB / 1000.0 / 1000.0)
        sealConduitT (responseBody response) $$+-
            printDownloadProgress fileSizeMB .|
            sinkFile (pathToRepo </> pacName)
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
                        liftIO $ hPutStr stderr ("> " ++ show fileSizeMB ++ "MB > " ++ roundToStr 1 leadedPercent ++ "% ")
                        liftIO $ hFlush stderr
                    yield x
                    loop newLoadedB newLoadedMB
