{-# LANGUAGE DeriveGeneric #-}

module Poseidon.Checksums where

import           Poseidon.Utils             (PoseidonException (..))

import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.=), (.:?))

import qualified Data.ByteString.Lazy       as LB
import           Data.Digest.Pure.MD5       (md5, MD5Digest (..))
import           Data.Maybe                 (Maybe (..), maybe, isNothing)
import           Data.Text                  (pack)

-- | A datatype to specify a list of checksums in the POSEIDON.yml file
data ChecksumListSpec = ChecksumListSpec
    { genoFileCheck :: Maybe String
    , snpFileCheck  :: Maybe String
    , indFileCheck  :: Maybe String
    , jannoFileCheck  :: Maybe String
    , bibFileCheck  :: Maybe String
    }
    deriving (Show)

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

instance Eq ChecksumListSpec where
    (==) (ChecksumListSpec g1 s1 i1 j1 b1) (ChecksumListSpec g2 s2 i2 j2 b2) =
           (Just g1 ==  Just g2 || isNothing g1 || isNothing g2) 
        && (Just s1 ==  Just s2 || isNothing s1 || isNothing s2)
        && (Just i1 ==  Just i2 || isNothing i1 || isNothing i2)
        && (Just j1 ==  Just j2 || isNothing j1 || isNothing j2)
        && (Just b1 ==  Just b2 || isNothing b1 || isNothing b2)

renderCheckSumComparison :: ChecksumListSpec -> ChecksumListSpec -> String
renderCheckSumComparison (ChecksumListSpec g1 s1 i1 j1 b1) (ChecksumListSpec g2 s2 i2 j2 b2) =
    let gLine = if g1 /= g2 then "genoFile:  "  ++ maybeShow g1 ++ " <> " ++ maybeShow g2 ++ "\n" else ""
        sLine = if s1 /= s2 then "snpFile:   "   ++ maybeShow s1 ++ " <> " ++ maybeShow s2 ++ "\n" else ""
        iLine = if i1 /= i2 then "indFile:   "   ++ maybeShow i1 ++ " <> " ++ maybeShow i2 ++ "\n" else ""
        jLine = if j1 /= j2 then "jannoFile: " ++ maybeShow j1 ++ " <> " ++ maybeShow j2 ++ "\n" else ""
        bLine = if b1 /= b2 then "bibFile:   "   ++ maybeShow b1 ++ " <> " ++ maybeShow b2 ++ "\n" else ""
    in gLine ++ sLine ++ iLine ++ jLine ++ bLine

-- | A helper function to unwrap a maybe
maybeShow :: Show a => Maybe a -> String
maybeShow (Just x) = show x
maybeShow Nothing  = "nothing"

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
