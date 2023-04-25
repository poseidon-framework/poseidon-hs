{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.EntitiesList   (EntityInput, PoseidonEntity,
                                          findNonExistentEntities,
                                          indInfoFindRelevantPackageNames,
                                          readEntityInputs)
import           Poseidon.MathHelpers    (roundTo, roundToStr)
import           Poseidon.Package        (PackageReadOptions (..),
                                          PoseidonPackage (..),
                                          defaultPackageReadOptions,
                                          readPoseidonPackageCollection)
import           Poseidon.SecondaryTypes (ApiReturnData (..),
                                          IndividualInfo (..), PackageInfo (..),
                                          processApiResponse)
import           Poseidon.Utils          (LogA, PoseidonException (..),
                                          PoseidonIO, envLogAction,
                                          extendNameWithVersion, logInfo,
                                          logWarning, logWithEnv, padLeft,
                                          padRight)

import           Codec.Archive.Zip       (ZipOption (..),
                                          extractFilesFromArchive, toArchive)
import           Conduit                 (ResourceT, await, runResourceT,
                                          sinkFile, yield)
import           Control.Exception       (throwIO)
import           Control.Monad           (forM_, unless, when)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (eitherDecode')
import qualified Data.ByteString         as B
import           Data.ByteString.Char8   as B8 (unpack)
import qualified Data.ByteString.Lazy    as LB
import           Data.Conduit            (ConduitT, sealConduitT, ($$+-), (.|))
import           Data.Maybe              (fromMaybe)
import           Data.Version            (Version, showVersion)
import           Network.HTTP.Conduit    (http, newManager, parseRequest,
                                          responseBody, responseHeaders,
                                          tlsManagerSettings)
import           Network.HTTP.Types      (hContentLength)
import           System.Directory        (createDirectoryIfMissing,
                                          removeDirectory, removeFile)
import           System.FilePath         ((</>))

data FetchOptions = FetchOptions
    { _jaBaseDirs  :: [FilePath]
    , _entityInput :: [EntityInput PoseidonEntity] -- Empty list = All packages
    , _remoteURL   :: String
    , _upgrade     :: Bool
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
    }

-- | The main function running the Fetch command
runFetch :: FetchOptions -> PoseidonIO ()
runFetch (FetchOptions baseDirs entityInputs remoteURL upgrade) = do

    let remote = remoteURL --"https://c107-224.cloud.gwdg.de"
        downloadDir = head baseDirs
        tempDir = downloadDir </> ".trident_download_folder"

    logInfo $ "Download directory (will be created if missing): " ++ downloadDir
    liftIO $ createDirectoryIfMissing True downloadDir

    -- compile entities
    entities <- readEntityInputs entityInputs

    -- load remote package list
    logInfo "Downloading individual list from remote"
    remoteIndList <- do
        r <- processApiResponse (remoteURL ++ "/individuals")
        case r of
            ApiReturnIndividualInfo i _ _ -> return i
            _                             -> error "should not happen"

    logInfo "Downloading package list from remote"
    remotePacList <- do
        r <- processApiResponse (remoteURL ++ "/packages")
        case r of
            ApiReturnPackageInfo p -> return p
            _                      -> error "should not happen"

    let nonExistentEntities = findNonExistentEntities entities remoteIndList

    if (not . null) nonExistentEntities then do
        logWarning "Cannot find the following requested entities:"
        logWarning $ show nonExistentEntities
    else do
        -- load local packages
        allLocalPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
        -- check which remote packages the User wants to have
        logInfo "Determine requested packages... "
        let remotePacTitles = map pTitle remotePacList
        let desiredPacTitles =
                if null entities then remotePacTitles else indInfoFindRelevantPackageNames entities remoteIndList

        let desiredRemotePackages = filter (\x -> pTitle x `elem` desiredPacTitles) remotePacList

        logInfo $ show (length desiredPacTitles) ++ " requested"
        unless (null desiredRemotePackages) $ do
            liftIO $ createDirectoryIfMissing False tempDir
            forM_ desiredRemotePackages $ \pac -> do
                -- perform package download depending on local-remote state
                logInfo $ "Comparing local and remote package " ++ extendNameWithVersion (pTitle pac) (pVersion pac)
                let packageState = determinePackageState allLocalPackages pac
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

handlePackageByState :: FilePath -> FilePath -> String -> Bool -> (PackageState, String, Maybe Version, Maybe Version) -> PoseidonIO ()
handlePackageByState downloadDir tempDir remote _ (NotLocal, pac, removeV, _) = do
    downloadAndUnzipPackage downloadDir tempDir remote pac removeV
handlePackageByState _ _ _ _ (EqualLocalRemote, pac, remoteV, localV) = do
    logInfo $ padRight 40 pac ++
        " local " ++ printV localV ++ " = remote " ++ printV remoteV
handlePackageByState downloadDir tempDir remote upgrade (LaterRemote, pac, remoteV, localV) = do
    if upgrade
    then downloadAndUnzipPackage downloadDir tempDir remote pac remoteV
    else logInfo $ padRight 40 pac ++
        " local " ++ printV localV ++ " < remote " ++ printV remoteV ++
        " (overwrite with --upgrade)"
handlePackageByState _ _ _ _ (LaterLocal, pac, remoteV, localV) = do
    logInfo $ padRight 40 pac ++
        " local " ++ printV localV ++ " > remote " ++ printV remoteV

printV :: Maybe Version -> String
printV Nothing  = "?.?.?"
printV (Just x) = showVersion x

downloadAndUnzipPackage :: FilePath -> FilePath -> String -> String -> Maybe Version -> PoseidonIO ()
downloadAndUnzipPackage baseDir tempDir remote pacName maybePackageVersion = do
    logInfo $ padRight 40 pacName ++ " now downloading"
    downloadPackage tempDir remote pacName
    liftIO $ do
        unzipPackage (tempDir </> pacName) (baseDir </> extendNameWithVersion pacName maybePackageVersion)
        removeFile (tempDir </> pacName)

unzipPackage :: FilePath -> FilePath -> IO ()
unzipPackage zip_ outDir = do
    archiveBS <- LB.readFile zip_
    let archive = toArchive archiveBS
    extractFilesFromArchive [OptRecursive, OptDestination outDir] archive

downloadPackage :: FilePath -> String -> String -> PoseidonIO ()
downloadPackage pathToRepo remote pacName = do
    logA <- envLogAction
    downloadManager <- liftIO $ newManager tlsManagerSettings
    packageRequest <- parseRequest (remote ++ "/zip_file/" ++ pacName)
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
