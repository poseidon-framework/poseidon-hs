{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.ForgeRecipe       (ForgeEntity (..), ForgeRecipe (..), 
                                             readEntitiesFromFile)
import           Poseidon.Package           (PackageInfo (..),
                                             PoseidonPackage (..),
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..))

import           Control.Exception          (throwIO)
import qualified Data.ByteString.Lazy       as LB
import           Data.Aeson                 (Value, eitherDecode')
import           Network.HTTP.Conduit        (simpleHttp, 
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
    responseBS <- simpleHttp "http://c107-224.cloud.gwdg.de:3000/packages"
    hu <- readPackageInfo responseBS
    print hu

readPackageInfo :: LB.ByteString -> IO [PackageInfo]
readPackageInfo bs = do
    case eitherDecode' bs of
        Left err  -> throwIO $ PoseidonRemoteJSONParsingException err
        Right pac -> return pac