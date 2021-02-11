{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Fetch where

import           Poseidon.ForgeRecipe       (ForgeEntity (..), ForgeRecipe (..), 
                                             readEntitiesFromFile)

import           Data.Aeson                 (Value)
import           Network.HTTP.Simple        (getResponseStatusCode, 
                                             httpJSON, 
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
    response <- httpJSON "http://c107-224.cloud.gwdg.de:3000/packages" :: IO (Response ())
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)

 