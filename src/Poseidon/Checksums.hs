{-# LANGUAGE DeriveGeneric #-}

module Poseidon.Checksums where

import           Poseidon.Utils             (PoseidonException (..))

import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.=), (.:?))
import qualified Data.ByteString.Lazy       as LB
import           Data.Digest.Pure.MD5       (md5, MD5Digest (..))
import           Data.Maybe                 (Maybe (..), maybe)
import           Data.Text                  (pack)

-- | A datatype to specify a list of checksums in the POSEIDON.yml file
data ChecksumListSpec = ChecksumListSpec
    { genoFileCheck :: Maybe String
    , snpFileCheck  :: Maybe String
    , indFileCheck  :: Maybe String
    , jannoFileCheck  :: Maybe String
    , bibFileCheck  :: Maybe String
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of ChecksumListSpec from JSON files
instance FromJSON ChecksumListSpec where
    parseJSON = withObject "checksums" $ \v -> ChecksumListSpec
        <$> v .:? pack "genoFileCheck"
        <*> v .:? pack "snpFileCheck"
        <*> v .:? pack "indFileCheck"
        <*> v .:? pack "jannoFileCheck"
        <*> v .:? pack "bibFileCheck"

instance ToJSON ChecksumListSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        pack "genoFileCheck" .= genoFileCheck x,
        pack "snpFileCheck" .= snpFileCheck x,
        pack "indFileCheck" .= indFileCheck x,
        pack "jannoFileCheck" .= jannoFileCheck x,
        pack "bibFileCheck" .= bibFileCheck x
        ]

makeChecksumList :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> IO (Maybe ChecksumListSpec)
makeChecksumList Nothing Nothing Nothing Nothing Nothing = do return Nothing
makeChecksumList genoFile snpFile indFile jannoFile bibFile = do
    genoFileCheck  <- getChecksumMaybe genoFile
    snpFileCheck   <- getChecksumMaybe snpFile
    indFileCheck   <- getChecksumMaybe indFile
    jannoFileCheck <- getChecksumMaybe jannoFile
    bibFileCheck   <- getChecksumMaybe bibFile
    return $ Just $ ChecksumListSpec genoFileCheck snpFileCheck indFileCheck jannoFileCheck bibFileCheck

getChecksumMaybe :: Maybe FilePath -> IO (Maybe String)
getChecksumMaybe Nothing = do return Nothing 
getChecksumMaybe (Just mf) = do 
    md5Digest <- getChecksum mf
    return $ Just md5Digest

getChecksum :: FilePath -> IO String
getChecksum f = do
    fileContent <- LB.readFile f
    let md5Digest = md5 fileContent
    return $ show md5Digest
