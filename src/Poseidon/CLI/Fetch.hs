{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.ForgeRecipe       (ForgeEntity (..), ForgeRecipe (..), 
                                             readEntitiesFromFile)
import           Poseidon.Package           (PackageInfo (..),
                                             PoseidonPackage (..),
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..))

import           Conduit
import           Control.Exception          (throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (Value, eitherDecode')
import qualified Data.ByteString.Lazy       as LB
import           Data.List                  ((\\), nub)
import           Network.HTTP.Simple        (httpLBS,
                                             httpSink,
                                             parseRequest,
                                             getResponseBody,
                                             Response (..))
import           System.IO                  (hPutStrLn, stderr, hFlush, hPutStr)

data FetchOptions = FetchOptions
    { _jaBaseDirs :: [FilePath]
    , _entityList :: ForgeRecipe
    , _entityFile :: Maybe FilePath
    --, _onlyPreview :: Bool
    --, _remoteURL :: String
    }

-- | The main function running the Fetch command
runFetch :: FetchOptions -> IO ()
runFetch (FetchOptions baseDirs entitiesDirect entitiesFile) = do --onlyPreview remoteURL) = do
    let remote = "http://c107-224.cloud.gwdg.de:3000"
    -- compile entities
    entitiesFromFile <- case entitiesFile of
        Nothing -> return []
        Just f -> readEntitiesFromFile f
    let entities = nub $ entitiesDirect ++ entitiesFromFile --this nub could also be relevant for forge
    let desiredPacsTitles = entities2PacTitles entities -- this whole mechanism can be replaced when the server also returns the individuals and groups in a package
    print desiredPacsTitles
    -- load local packages
    allLocalPackages <- readPoseidonPackageCollection False False baseDirs
    -- load remote package list
    overviewRequest <- parseRequest (remote ++ "/packages")
    overviewResponse <- httpLBS overviewRequest
    let remoteOverviewJSONByteString = getResponseBody overviewResponse
    allRemotePackages <- readPackageInfo remoteOverviewJSONByteString
    -- check which remote packages the User wants to have 
    let desiredRemotePackages = filter (\x -> pTitle x `elem` desiredPacsTitles) allRemotePackages
    -- check which packages need updating
    let localPacSimple = map (\x -> (posPacTitle x, posPacPackageVersion x)) allLocalPackages
    let desiredRemotePacSimple = map (\x -> (pTitle x, pVersion x)) desiredRemotePackages
    let pacsToDownload = map fst $ desiredRemotePacSimple \\ localPacSimple
    print pacsToDownload
    -- download
    mapM_ (downloadPackage remote) pacsToDownload
    -- 
    putStrLn "Ende"

downloadPackage :: String -> String -> IO ()
downloadPackage remote pacName = do
    packageRequest <- parseRequest (remote ++ "/zip_file/" ++ pacName)
    runResourceT $ httpSink packageRequest
        (\_res -> getZipSink (ZipSink (sinkFile "/home/clemens/test/fetchtest/huhu")))
    return ()

entities2PacTitles :: [ForgeEntity] ->  [String]
entities2PacTitles xs = do
    let pacEntities = [ x | x@ForgePac {} <- xs]
    map getEntityStrings pacEntities
    where
        getEntityStrings :: ForgeEntity -> String
        getEntityStrings (ForgePac x) = x
        getEntityStrings (ForgeGroup x) = x
        getEntityStrings (ForgeInd x) = x

readPackageInfo :: LB.ByteString -> IO [PackageInfo]
readPackageInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right pac -> return pac