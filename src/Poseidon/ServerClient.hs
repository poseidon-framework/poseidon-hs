{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.ServerClient (
    ServerApiReturnType(..),
    ApiReturnData(..),
    processApiResponse,
    ArchiveEndpoint(..),
    PackageInfo (..), GroupInfo (..),
    qDefault, qArchive, qPacVersion, (+&+)
) where

import           Paths_poseidon_hs      (version)
import           Poseidon.EntityTypes   (PacNameAndVersion (..), HasNameAndVersion(..), IndividualInfo(..))
import           Poseidon.Janno         (JannoRows)
import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logError, logInfo)

import           Control.Exception      (catch, throwIO)
import           Control.Monad          (forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON (..), Value (String),
                                         eitherDecode', object, parseJSON,
                                         toJSON, withObject, (.:), (.=))
import           Data.Time              (Day)
import           Data.Version           (Version, showVersion)
import           GHC.Generics           (Generic)
import           Network.HTTP.Conduit   (simpleHttp)

--  Client Server Communication types and functions

qDefault :: Maybe String -> String
qDefault archive = qVersion +&+ qArchive archive

(+&+) :: String -> String -> String
(+&+) a ('?':b) = a ++ "&" ++ b
(+&+) a b       = a ++ "&" ++ b

qPacVersion :: Maybe Version -> String
qPacVersion Nothing  = ""
qPacVersion (Just v) = "?package_version=" ++ showVersion v

qVersion :: String
qVersion = "?client_version=" ++ showVersion version

qArchive :: Maybe String -> String
qArchive Nothing     = ""
qArchive (Just name) = "?archive=" ++ name

data ArchiveEndpoint = ArchiveEndpoint {
      _aeServerURL   :: String
    , _aeArchiveName :: Maybe String
}

data ServerApiReturnType = ServerApiReturnType {
    _apiMessages :: [String],
    _apiResponse :: Maybe ApiReturnData
}

instance ToJSON ServerApiReturnType where
    toJSON (ServerApiReturnType messages response) =
        object [
            "serverMessages" .= messages,
            "serverResponse" .= response
        ]

instance FromJSON ServerApiReturnType where
    parseJSON = withObject "ServerApiReturnType" $ \v -> ServerApiReturnType
            <$> v .: "serverMessages"
            <*> v .: "serverResponse"

data PackageInfo = PackageInfo
    { pTitle         :: String
    , pVersion       :: Maybe Version
    , pPosVersion    :: Version
    , pDescription   :: Maybe String
    , pLastModified  :: Maybe Day
    , pNrIndividuals :: Int
    } deriving (Eq)

instance HasNameAndVersion PackageInfo where
    getPacName = pTitle
    getPacVersion = pVersion

instance ToJSON PackageInfo where
    toJSON (PackageInfo title pacVersion posVersion description lastModified nrIndividuals) =
        object [
            "packageTitle"    .= title,
            "packageVersion"  .= pacVersion,
            "poseidonVersion" .= posVersion,
            "description"     .= description,
            "lastModified"    .= lastModified,
            "nrIndividuals"   .= nrIndividuals
        ]

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
            <$> v .: "packageTitle"
            <*> v .: "packageVersion"
            <*> v .: "poseidonVersion"
            <*> v .: "description"
            <*> v .: "lastModified"
            <*> v .: "nrIndividuals"

data GroupInfo = GroupInfo
    { gName          :: String
    , gPackage       :: PacNameAndVersion
    , gNrIndividuals :: Int
    }

instance ToJSON GroupInfo where
    toJSON (GroupInfo name (PacNameAndVersion pacTitle pacVersion) nrIndividuals) =
        object [
            "groupName"      .= name,
            "packageTitle"   .= pacTitle,
            "packageVersion" .= pacVersion,
            "nrIndividuals"  .= nrIndividuals
        ]

instance FromJSON GroupInfo where
    parseJSON = withObject "GroupInfo" $ \v -> do
        groupName      <- v .: "groupName"
        packageTitle   <- v .: "packageTitle"
        packageVersion <- v .: "packageVersion"
        nrIndividuals  <- v .: "nrIndividuals"
        return $ GroupInfo groupName (PacNameAndVersion packageTitle packageVersion) nrIndividuals

data ApiReturnData = ApiReturnPackageInfo [PackageInfo]
                   | ApiReturnGroupInfo [GroupInfo]
                   | ApiReturnIndividualInfo [IndividualInfo]
                   | ApiReturnJanno [(String, JannoRows)] deriving (Generic)

instance ToJSON ApiReturnData where
    toJSON (ApiReturnPackageInfo pacInfo) =
        object [
            "constructor" .= String "ApiReturnPackageInfo",
            "packageInfo" .= pacInfo
        ]
    toJSON (ApiReturnGroupInfo groupInfo) =
        object [
            "constructor" .= String "ApiReturnGroupInfo",
            "groupInfo" .= groupInfo
        ]
    toJSON (ApiReturnIndividualInfo indInfo) =
        object [
            "constructor" .= String "ApiReturnExtIndividualInfo", -- we use the term "Ext" in here for historical and compatibility reasons. But actually this is just IndividualInfo
            "extIndInfo" .= indInfo
        ]
    toJSON (ApiReturnJanno janno) =
        object [
            "constructor" .= String "ApiReturnJanno",
            "janno" .= janno
        ]

instance FromJSON ApiReturnData where
    parseJSON = withObject "ApiReturnData" $ \v -> do
        constr <- v .: "constructor"
        case constr of
            "ApiReturnPackageInfo" -> ApiReturnPackageInfo <$> v .: "packageInfo"
            "ApiReturnGroupInfo" -> ApiReturnGroupInfo <$> v .: "groupInfo"
            "ApiReturnExtIndividualInfo" -> ApiReturnIndividualInfo <$> v .: "extIndInfo" -- see above, term "ext" for historical reasons.
            "ApiReturnJanno" -> ApiReturnJanno <$> v .: "janno"
            _ -> error $ "cannot parse ApiReturnType with constructor " ++ constr

processApiResponse :: String -> Bool -> PoseidonIO ApiReturnData
processApiResponse url quiet = do
    remoteData <- liftIO $ catch (simpleHttp url) (throwIO . PoseidonHttpExceptionForward)
    ServerApiReturnType messages maybeReturn <- case eitherDecode' remoteData of
        Left err  -> liftIO . throwIO $ PoseidonRemoteJSONParsingException err
        Right sam -> return sam
    unless (null messages || quiet) $
        forM_ messages (\msg -> logInfo $ "Message from the Server: " ++ msg)
    case maybeReturn of
        Just apiReturn -> return apiReturn
        Nothing -> do
            logError "The server request was unsuccessful"
            liftIO . throwIO . PoseidonServerCommunicationException $ "Server error upon URL " ++ url
