{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.EntitiesList  (EntityInput, PoseidonEntity,
                                         determineNonExistentEntities,
                                         determineRelevantPackages,
                                         readEntityInputs)
import           Poseidon.EntityTypes   (ExtendedIndividualInfo (..),
                                         IndividualInfo (..),
                                         PacNameAndVersion (..),
                                         PackageInfo (..),
                                         makePacNameAndVersion,
                                         renderNameWithVersion)
import           Poseidon.MathHelpers   (roundTo, roundToStr)
import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.ServerClient  (ApiReturnData (..),
                                         ArchiveEndpoint (..),
                                         processApiResponse, qArchive, qDefault)
import           Poseidon.Utils         (LogA, PoseidonException (..),
                                         PoseidonIO, envLogAction, logInfo,
                                         logWarning, logWithEnv, padLeft)

import           Codec.Archive.Zip      (ZipOption (..),
                                         extractFilesFromArchive, toArchive)
import           Conduit                (ResourceT, await, runResourceT,
                                         sinkFile, yield)
import           Control.Exception      (catch, throwIO)
import           Control.Monad          (forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (eitherDecode')
import qualified Data.ByteString        as B
import           Data.ByteString.Char8  as B8 (unpack)
import qualified Data.ByteString.Lazy   as LB
import           Data.Conduit           (ConduitT, sealConduitT, ($$+-), (.|))
import           Data.List              (groupBy, sortBy)
import           Data.Maybe             (fromMaybe)
import           Data.Version           (Version, showVersion)
import           Network.HTTP.Conduit   (http, newManager, parseRequest,
                                         responseBody, responseHeaders,
                                         tlsManagerSettings)
import           Network.HTTP.Types     (hContentLength)
import           System.Directory       (createDirectoryIfMissing,
                                         removeDirectory, removeFile)
import           System.FilePath        ((</>))

data FetchOptions = FetchOptions
    { _jaBaseDirs  :: [FilePath]
    , _entityInput :: [EntityInput PoseidonEntity] -- Empty list = All packages
    , _archiveEnd  :: ArchiveEndpoint
    }

data PackageState = NotLocal
    | EqualLocalRemote
    | LaterRemote
    | LaterLocal

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    , _readOptKeepMultipleVersions = True
    }

-- | The main function running the Fetch command
runFetch :: FetchOptions -> PoseidonIO ()
runFetch (FetchOptions baseDirs entityInputs archiveE@(ArchiveEndpoint remoteURL archive)) = do

    let downloadDir = head baseDirs
        tempDir = downloadDir </> ".trident_download_folder"

    logInfo $ "Download directory (will be created if missing): " ++ downloadDir
    liftIO $ createDirectoryIfMissing True downloadDir

    -- compile entities
    entities <- readEntityInputs entityInputs

    -- load remote package list
    logInfo "Downloading individual list from remote"
    remoteIndList <- do
        r <- processApiResponse (remoteURL ++ "/individuals" ++ qDefault archive) False
        case r of
            ApiReturnExtIndividualInfo extIndInfo ->
                return [IndividualInfo i g (PacNameAndVersion p v) | ExtendedIndividualInfo i g p v _ <- extIndInfo]
            _                             -> error "should not happen"

    logInfo "Downloading package list from remote"
    remotePacList <- do
        r <- processApiResponse (remoteURL ++ "/packages" ++ qDefault archive) True
        case r of
            ApiReturnPackageInfo p -> return p
            _                      -> error "should not happen"

    let nonExistentEntities = determineNonExistentEntities entities remoteIndList

    if (not . null) nonExistentEntities then do
        logWarning "Cannot find the following requested entities:"
        logWarning $ show nonExistentEntities
    else do
        -- load local packages
        allLocalPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
        -- check which remote packages the User wants to have
        logInfo "Determine requested packages... "
        let remotePacs = map makePacNameAndVersion remotePacList
        let desiredPacs = if null entities then remotePacs else determineRelevantPackages entities remoteIndList

        let desiredRemotePackages =
                map last .
                groupBy (\x y -> pTitle x == pTitle y) .
                sortBy (\x y -> compare (pTitle x, pVersion x) (pTitle y, pVersion y)) .
                filter (\p -> makePacNameAndVersion p `elem` desiredPacs) $ remotePacList

        logInfo $ show (length desiredPacs) ++ " requested"
        logInfo $ "Comparing local and remote packages..."

        unless (null desiredRemotePackages) $ do
            liftIO $ createDirectoryIfMissing False tempDir
            forM_ desiredRemotePackages $ \pac -> do
                -- perform package download depending on local-remote state
                let packageState = determinePackageState allLocalPackages pac
                handlePackageByState downloadDir tempDir archiveE packageState
            liftIO $ removeDirectory tempDir
    logInfo "Done"

readServerIndInfo :: LB.ByteString -> IO [ExtendedIndividualInfo]
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

handlePackageByState :: FilePath -> FilePath -> ArchiveEndpoint -> (PackageState, String, Maybe Version, Maybe Version) -> PoseidonIO ()
handlePackageByState downloadDir tempDir archiveE (NotLocal, pac, remoteV, _) = do
    logInfo $ "[local _._._" ++ " x remote " ++ printV remoteV ++ "] " ++ pac
    downloadAndUnzipPackage downloadDir tempDir archiveE (PacNameAndVersion pac remoteV)
handlePackageByState _ _ _ (EqualLocalRemote, pac, remoteV, localV) = do
    logInfo $ "[local " ++ printV localV ++ " = remote " ++ printV remoteV ++ "] " ++ pac
handlePackageByState downloadDir tempDir archiveE (LaterRemote, pac, remoteV, localV) = do
    logInfo $ "[local " ++ printV localV ++ " < remote " ++ printV remoteV ++ "] " ++ pac
    downloadAndUnzipPackage downloadDir tempDir archiveE (PacNameAndVersion pac remoteV)
handlePackageByState _ _ _ (LaterLocal, pac, remoteV, localV) = do
    logInfo $ "[local " ++ printV localV ++ " > remote " ++ printV remoteV ++ "] " ++ pac

printV :: Maybe Version -> String
printV Nothing  = "?.?.?"
printV (Just x) = showVersion x

downloadAndUnzipPackage :: FilePath -> FilePath -> ArchiveEndpoint -> PacNameAndVersion -> PoseidonIO ()
downloadAndUnzipPackage baseDir tempDir archiveE pacNameAndVersion = do
    let PacNameAndVersion pacName _ = pacNameAndVersion
    logInfo $ "Downloading: " ++ pacName
    downloadPackage tempDir archiveE pacName
    liftIO $ do
        unzipPackage (tempDir </> pacName) (baseDir </> renderNameWithVersion pacNameAndVersion)
        removeFile (tempDir </> pacName)

unzipPackage :: FilePath -> FilePath -> IO ()
unzipPackage zip_ outDir = do
    archiveBS <- LB.readFile zip_
    let archive = toArchive archiveBS
    catch (extractFilesFromArchive [OptRecursive, OptDestination outDir] archive) (throwIO . PoseidonUnzipException)

downloadPackage :: FilePath -> ArchiveEndpoint -> String -> PoseidonIO ()
downloadPackage pathToRepo (ArchiveEndpoint remoteURL archive) pacName = do
    logA <- envLogAction
    downloadManager <- liftIO $ newManager tlsManagerSettings
    packageRequest <- parseRequest (remoteURL ++ "/zip_file/" ++ pacName ++ qArchive archive)
    liftIO $ runResourceT $ do
        response <- http packageRequest downloadManager
        let fileSize = fromMaybe "0" $ lookup hContentLength (responseHeaders response)
        let fileSizeKB = (read $ B8.unpack fileSize) :: Int
        let fileSizeMB = roundTo 1 (fromIntegral fileSizeKB / 1000.0 / 1000.0)
        logWithEnv logA $ logInfo $ "Package size: " ++ show (roundTo 1 fileSizeMB) ++ "MB"
        sealConduitT (responseBody response) $$+-
            printDownloadProgress logA fileSizeMB .|
            sinkFile (pathToRepo </> pacName)
    return ()

printDownloadProgress :: LogA -> Double -> ConduitT B.ByteString B.ByteString (ResourceT IO) ()
printDownloadProgress logA fileSizeMB = loop 0 0
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
                        logWithEnv logA $ logInfo ("MB:" ++ padLeft 9 (show curLoadedMB) ++ "    " ++ padLeft 5 (roundToStr 1 leadedPercent) ++ "% ")
                    yield x
                    loop newLoadedB newLoadedMB
