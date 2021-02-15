{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.EntitiesList       (PoseidonEntity (..), EntitiesList (..), 
                                             readEntitiesFromFile)
import           Poseidon.MathHelpers       (roundTo)
import           Poseidon.Package           (PackageInfo (..),
                                             PoseidonPackage (..),
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..))

import           Codec.Archive.Zip          (ZipOption (..), extractFilesFromArchive, toArchive)
import           Conduit                    (runResourceT, sinkFile, ResourceT, await, yield)
import           Control.Exception          (throwIO)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (Value, eitherDecode')
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.ByteString.Char8      as B8 (unpack)
import           Data.Conduit               (($$+-), (.|), sealConduitT, ConduitT)               
import           Data.List                  ((\\), nub, intercalate, intersect)
import           Data.Version               (Version, showVersion)
import           Network.HTTP.Conduit       (simpleHttp,
                                             newManager,
                                             tlsManagerSettings,
                                             parseRequest, 
                                             http, 
                                             responseBody,
                                             responseHeaders)
import           Network.HTTP.Types         (hContentLength)
import           System.Console.ANSI        (hClearLine, hSetCursorColumn)
import           System.Directory           (createDirectory, removeFile, removeDirectory)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (hPutStrLn, stderr, hFlush, hPutStr)

data FetchOptions = FetchOptions
    { _jaBaseDirs :: [FilePath]
    , _entityList :: EntitiesList
    , _entityFile :: Maybe FilePath
    , _remoteURL :: String
    , _upgrade :: Bool
    }

data PackageState = NotLocal -- ^ Package exists only on remote and can be downloaded directly
                  | EqualLocalRemote -- ^ Local and remote package version are equal. Nothing should be done
                  | LaterRemote -- ^ The remote version is later. An upgrade should be optional
                  | LaterLocal -- ^ The local version is later. Nothing should be done

-- | The main function running the Fetch command
runFetch :: FetchOptions -> IO ()
runFetch (FetchOptions baseDirs entitiesDirect entitiesFile remoteURL upgrade) = do
    let remote = remoteURL --"https://c107-224.cloud.gwdg.de:3000"
        downloadDir = head baseDirs
        tempDir = downloadDir </> ".trident_download_folder"
    -- compile entities
    entitiesFromFile <- case entitiesFile of
        Nothing -> return []
        Just f -> readEntitiesFromFile f
    let entities = nub $ entitiesDirect ++ entitiesFromFile --this nub could also be relevant for forge
        desiredPacsTitles = entities2PacTitles entities -- this whole mechanism can be replaced when the server also returns the individuals and groups in a package
    -- load local packages
    allLocalPackages <- readPoseidonPackageCollection False False baseDirs
    -- load remote package list
    remoteOverviewJSONByteString <- simpleHttp (remote ++ "/packages")
    allRemotePackages <- readPackageInfo remoteOverviewJSONByteString
    -- check which remote packages the User wants to have
    let desiredRemotePackages = filter (\x -> pTitle x `elem` desiredPacsTitles) allRemotePackages
    -- perform package download depending on local-remote state
    let packagesWithState = map (determinePackageState allLocalPackages) desiredRemotePackages
    createDirectory tempDir
    mapM_ (handlePackageByState downloadDir tempDir remote upgrade) packagesWithState
    removeDirectory tempDir

entities2PacTitles :: [PoseidonEntity] ->  [String]
entities2PacTitles xs = do
    let pacEntities = [ x | x@Pac {} <- xs]
    map getEntityStrings pacEntities
    where
        getEntityStrings :: PoseidonEntity -> String
        getEntityStrings (Pac x) = x
        getEntityStrings (Group x) = x
        getEntityStrings (Ind x) = x

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
printV Nothing = "?.?.?"
printV (Just x) = showVersion x

downloadAndUnzipPackage :: FilePath -> FilePath -> String -> String -> IO ()
downloadAndUnzipPackage baseDir tempDir remote pacName = do
    downloadPackage tempDir remote pacName
    unzipPackage (tempDir </> pacName) (baseDir </> pacName)
    removeFile (tempDir </> pacName)

unzipPackage :: FilePath -> FilePath -> IO ()
unzipPackage zip outDir = do
    archiveBS <- LB.readFile zip
    let archive = toArchive archiveBS
    extractFilesFromArchive [OptRecursive, OptVerbose, OptDestination outDir] archive

downloadPackage :: FilePath -> String -> String -> IO ()
downloadPackage pathToRepo remote pacName = do
    let pacNameNormsize = padString 40 pacName
    downloadManager <- newManager tlsManagerSettings
    packageRequest <- parseRequest (remote ++ "/zip_file/" ++ pacName)
    runResourceT $ do 
        response <- http packageRequest downloadManager
        let Just fileSize = lookup hContentLength (responseHeaders response)
        let fileSizeKB = read $ B8.unpack fileSize
        let fileSizeMB = roundTo 1 (fromIntegral fileSizeKB / 1000 / 1000)
        sealConduitT (responseBody response) $$+- 
            printDownloadProgress pacNameNormsize fileSizeMB .| 
            sinkFile (pathToRepo </> pacName)
    putStrLn ""
    return ()

padString :: Int -> String -> String
padString n s
    | length s > n  = take (n-1) s ++ " "
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

printDownloadProgress :: String -> Double -> ConduitT B.ByteString B.ByteString (ResourceT IO) ()
printDownloadProgress pacName fileSizeMB = loop 0 0
    where
        loop loadedKB loadedMB = do
            x <- await
            maybe (return ()) (showDownloaded fileSizeMB loadedKB) x
            where
                showDownloaded fileSizeMB loadedKB x = do
                    let newLoadedKB = loadedKB + B.length x
                    let curLoadedMB = roundTo 1 (fromIntegral newLoadedKB / 1000 / 1000)
                                          -- update progress counter every 1%
                    let newLoadedMB = if (curLoadedMB/fileSizeMB - loadedMB/fileSizeMB >= 0.01 &&
                                          -- but only at at least 200KB 
                                          curLoadedMB - loadedMB > 0.2) || 
                                          -- and of course at the end of the sequence
                                          curLoadedMB == fileSizeMB
                                      then curLoadedMB
                                      else loadedMB
                    when (loadedMB /= newLoadedMB) $ do
                        liftIO $ hClearLine stderr
                        liftIO $ hSetCursorColumn stderr 0
                        liftIO $ hPutStr stderr (pacName ++ show newLoadedMB ++ "/" ++ show fileSizeMB ++ "MB")
                        liftIO $ hFlush stderr
                    yield x
                    loop newLoadedKB newLoadedMB
