{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.EntityTypes   (EntityInput, HasNameAndVersion (..),
                                         IndividualInfo (..),
                                         PacNameAndVersion (..), PoseidonEntity,
                                         checkIfAllEntitiesExist,
                                         determineRelevantPackages,
                                         isLatestInCollection,
                                         makePacNameAndVersion,
                                         readEntityInputs,
                                         renderNameWithVersion)
import           Poseidon.MathHelpers   (roundTo, roundToStr)
import           Poseidon.Package       (PackageReadOptions (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.ServerClient  (ApiReturnData (..),
                                         ArchiveEndpoint (..),
                                         ExtendedIndividualInfo (..),
                                         PackageInfo (..), processApiResponse,
                                         qDefault, qPacVersion, (+&+))
import           Poseidon.Utils         (LogA, PoseidonException (..),
                                         PoseidonIO, envLogAction, logDebug,
                                         logInfo, logWithEnv, padLeft)

import           Codec.Archive.Zip      (ZipOption (..),
                                         extractFilesFromArchive, toArchive)
import           Conduit                (ResourceT, await, runResourceT,
                                         sinkFile, yield)
import           Control.Exception      (catch, throwIO)
import           Control.Monad          (filterM, forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (eitherDecode')
import qualified Data.ByteString        as B
import           Data.ByteString.Char8  as B8 (unpack)
import qualified Data.ByteString.Lazy   as LB
import           Data.Conduit           (ConduitT, sealConduitT, ($$+-), (.|))
import           Data.List              (intercalate)
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
    , _entityInput :: [EntityInput PoseidonEntity] -- Empty list = All latest packages
    , _archiveEnd  :: ArchiveEndpoint
    }

data PackageState = NotLocal
    | EqualLocalRemote
    | UnequalLocalRemote

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    }

-- | The main function running the Fetch command
runFetch :: FetchOptions -> PoseidonIO ()
runFetch (FetchOptions baseDirs entityInputs archiveE@(ArchiveEndpoint remoteURL archive)) = do
    -- create download directory + temporary storage for downloaded .zip archives
    let downloadDir = head baseDirs
        tempDir = downloadDir </> ".trident_download_folder"
    logInfo $ "Download directory (will be created if missing): " ++ downloadDir
    liftIO $ createDirectoryIfMissing True downloadDir
    -- compile entities
    entities <- readEntityInputs entityInputs
    logDebug "Requested entities:"
    mapM_ (logDebug . show) entities
    -- load remote information to decide what to download
    logInfo "Downloading individual list from remote"
    remoteIndList <- do
        r <- processApiResponse (remoteURL ++ "/individuals" ++ qDefault archive) False
        case r of
            ApiReturnExtIndividualInfo indInfo -> return [IndividualInfo n g p | ExtendedIndividualInfo n g p _ _ <- indInfo]
            _                               -> error "should not happen"


    -- find and report non-existent entities (throws an exception)
    checkIfAllEntitiesExist entities remoteIndList
    -- load local packages
    allLocalPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    let localPacs = map makePacNameAndVersion allLocalPackages
    -- check which remote packages the User wants to have
    logInfo "Determine requested packages... "
    -- prepare list of relevant packages with individual list
    desiredPacs <- if null entities then do
            -- load all latest packages
            logInfo "Downloading package list from remote"
            remotePacListAll <- do
                r <- processApiResponse (remoteURL ++ "/packages" ++ qDefault archive) True
                case r of
                    ApiReturnPackageInfo p -> return p
                    _                      -> error "should not happen"
            remotePacList <- filterM (isLatestInCollection remotePacListAll) remotePacListAll
            return $ map makePacNameAndVersion remotePacList
        else determineRelevantPackages entities remoteIndList
    logDebug "Desired packages based on remote individuals list:"
    mapM_ (logDebug . show) desiredPacs
    -- start comparison/download process
    logInfo $ show (length desiredPacs) ++ " requested"
    logInfo   "Comparing local and remote packages..."
    unless (null desiredPacs) $ do
        liftIO $ createDirectoryIfMissing False tempDir
        forM_ desiredPacs $ \pac -> do
            -- perform package download depending on local-remote state
            let packageState = determinePackageState localPacs pac
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

determinePackageState :: [PacNameAndVersion] -> PacNameAndVersion -> (PackageState, String, Maybe Version, [Maybe Version])
determinePackageState localPacs desiredRemotePac@(PacNameAndVersion desiredRemotePacTitle desiredRemotePacVersion)
    | desiredRemotePacTitle `notElem` map getPacName localPacs =
        (NotLocal,           desiredRemotePacTitle, desiredRemotePacVersion, [Nothing])
    | desiredRemotePac `elem` localPacs =
        (EqualLocalRemote,   desiredRemotePacTitle, desiredRemotePacVersion, [desiredRemotePacVersion])
    | desiredRemotePac `notElem` localPacs =
        (UnequalLocalRemote, desiredRemotePacTitle, desiredRemotePacVersion, localVersionsOfDesired)
    | otherwise = error "determinePackageState: should never happen"
    where
        localVersionsOfDesired = map getPacVersion $ filter (\x -> getPacName x == desiredRemotePacTitle) localPacs

handlePackageByState :: FilePath -> FilePath -> ArchiveEndpoint -> (PackageState, String, Maybe Version, [Maybe Version]) -> PoseidonIO ()
handlePackageByState downloadDir tempDir archiveE (NotLocal, pac, remoteV, _) = do
    logInfo $ "[local _._._" ++ " x remote " ++ printV remoteV ++ "] " ++ pac
    downloadAndUnzipPackage downloadDir tempDir archiveE (PacNameAndVersion pac remoteV)
handlePackageByState _ _ _ (EqualLocalRemote, pac, remoteV, localVs) = do
    logInfo $ "[local " ++ printVs localVs ++ " = remote " ++ printV remoteV ++ "] " ++ pac
handlePackageByState downloadDir tempDir archiveE (UnequalLocalRemote, pac, remoteV, localVs) = do
    logInfo $ "[local " ++ printVs localVs ++ " < remote " ++ printV remoteV ++ "] " ++ pac
    downloadAndUnzipPackage downloadDir tempDir archiveE (PacNameAndVersion pac remoteV)

printVs :: [Maybe Version] -> String
printVs [] = "?.?.?"
printVs xs = intercalate "," $ map printV xs

printV :: Maybe Version -> String
printV Nothing  = "?.?.?"
printV (Just x) = showVersion x

downloadAndUnzipPackage :: FilePath -> FilePath -> ArchiveEndpoint -> PacNameAndVersion -> PoseidonIO ()
downloadAndUnzipPackage baseDir tempDir archiveE pacNameAndVersion = do
    let pnv = renderNameWithVersion pacNameAndVersion
    logInfo $ "Downloading: " ++ pnv
    downloadPackage tempDir archiveE pacNameAndVersion
    liftIO $ do
        unzipPackage (tempDir </> pnv) (baseDir </> pnv)
        removeFile (tempDir </> pnv)

unzipPackage :: FilePath -> FilePath -> IO ()
unzipPackage zip_ outDir = do
    archiveBS <- LB.readFile zip_
    let archive = toArchive archiveBS
    catch (extractFilesFromArchive [OptRecursive, OptDestination outDir] archive) (throwIO . PoseidonUnzipException)

downloadPackage :: FilePath -> ArchiveEndpoint -> PacNameAndVersion -> PoseidonIO ()
downloadPackage outDir (ArchiveEndpoint remoteURL archive) pacNameAndVersion@(PacNameAndVersion pacName pacVersion) = do
    logA <- envLogAction
    downloadManager <- liftIO $ newManager tlsManagerSettings
    packageRequest <- parseRequest (remoteURL ++ "/zip_file/" ++ pacName ++ qDefault archive +&+ qPacVersion pacVersion)
    --logInfo $ show packageRequest
    liftIO $ runResourceT $ do
        response <- http packageRequest downloadManager
        let fileSize = fromMaybe "0" $ lookup hContentLength (responseHeaders response)
        let fileSizeKB = (read $ B8.unpack fileSize) :: Int
        let fileSizeMB = roundTo 1 (fromIntegral fileSizeKB / 1000.0 / 1000.0)
        logWithEnv logA $ logInfo $ "Package size: " ++ show (roundTo 1 fileSizeMB) ++ "MB"
        sealConduitT (responseBody response) $$+-
            printDownloadProgress logA fileSizeMB .|
            sinkFile (outDir </> renderNameWithVersion pacNameAndVersion)
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
