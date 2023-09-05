{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.ServerClient (
    ServerApiReturnType(..),
    ApiReturnData(..),
    processApiResponse,
    ArchiveEndpoint(..),
    PackageInfo (..), GroupInfo (..), ExtendedIndividualInfo(..),
    qDefault, qArchive, qPacVersion, (+&+)
) where

import           Paths_poseidon_hs      (version)
import           Poseidon.EntityTypes   (HasNameAndVersion (..),
                                         PacNameAndVersion (..))
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

data ApiReturnData = ApiReturnPackageInfo [PackageInfo]
                   | ApiReturnGroupInfo [GroupInfo]
                   | ApiReturnExtIndividualInfo [ExtendedIndividualInfo]
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
    toJSON (ApiReturnExtIndividualInfo indInfo) =
        object [
            "constructor" .= String "ApiReturnExtIndividualInfo",
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
            "ApiReturnPackageInfo"       -> ApiReturnPackageInfo       <$> v .: "packageInfo"
            "ApiReturnGroupInfo"         -> ApiReturnGroupInfo         <$> v .: "groupInfo"
            "ApiReturnExtIndividualInfo" -> ApiReturnExtIndividualInfo <$> v .: "extIndInfo"
            "ApiReturnJanno"             -> ApiReturnJanno             <$> v .: "janno"
            _                            -> error $ "cannot parse ApiReturnType with constructor " ++ constr


data PackageInfo = PackageInfo
    { pPac         :: PacNameAndVersion
    , pPosVersion    :: Version
    , pDescription   :: Maybe String
    , pLastModified  :: Maybe Day
    , pNrIndividuals :: Int
    } deriving (Eq)

instance HasNameAndVersion PackageInfo where
    getPacName = getPacName . pPac
    getPacVersion = getPacVersion . pPac
    getPacIsLatest = getPacIsLatest . pPac
    setPacIsLatest a = let pac = pPac a in a {pPac = setPacIsLatest pac}

instance ToJSON PackageInfo where
    toJSON (PackageInfo (PacNameAndVersion n v l) posVersion description lastModified nrIndividuals) =
        object [
            "packageTitle"    .= n,
            "packageVersion"  .= v,
            "isLatest"        .= l,
            "poseidonVersion" .= posVersion,
            "description"     .= description,
            "lastModified"    .= lastModified,
            "nrIndividuals"   .= nrIndividuals
        ]

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
            <$> (PacNameAndVersion <$> (v .: "packageTitle") <*> (v .: "packageVersion") <*> (v .: "isLatest"))
            <*> v .: "posieonVersion"
            <*> v .: "description"
            <*> v .: "lastModified"
            <*> v .: "nrIndividuals"

data GroupInfo = GroupInfo
    { gName          :: String
    , gPackage       :: PacNameAndVersion
    , gNrIndividuals :: Int
    }

instance ToJSON GroupInfo where
    toJSON (GroupInfo name (PacNameAndVersion pacTitle pacVersion isLatest) nrIndividuals) =
        object [
            "groupName"       .= name,
            "packageTitle"    .= pacTitle,
            "packageVersion"  .= pacVersion,
            "packageIsLatest" .= isLatest,
            "nrIndividuals"   .= nrIndividuals
        ]

instance FromJSON GroupInfo where
    parseJSON = withObject "GroupInfo" $ \v -> do
        groupName      <- v .: "groupName"
        packageTitle   <- v .: "packageTitle"
        packageVersion <- v .: "packageVersion"
        nrIndividuals  <- v .: "nrIndividuals"
        isLatest       <- v .: "isLatest"
        return $ GroupInfo groupName (PacNameAndVersion packageTitle packageVersion isLatest) nrIndividuals

data ExtendedIndividualInfo = ExtendedIndividualInfo
    {
      extIndInfoName      :: String
    , extIndInfoGroups    :: [String]
    , extIndInfoPac       :: PacNameAndVersion
    , extIndInfoAddCols   :: [(String, Maybe String)]
    } deriving (Eq)

instance HasNameAndVersion ExtendedIndividualInfo where
    getPacName = getPacName . extIndInfoPac
    getPacVersion = getPacVersion . extIndInfoPac
    getPacIsLatest = getPacIsLatest . extIndInfoPac
    setPacIsLatest a = let pac = extIndInfoPac a in a {extIndInfoPac = setPacIsLatest pac}

instance ToJSON ExtendedIndividualInfo where
    toJSON e =
        object [
            "poseidonID"             .= extIndInfoName e,
            "groupNames"             .= extIndInfoGroups e,
            "packageTitle"           .= (getPacName     . extIndInfoPac $ e),
            "packageVersion"         .= (getPacVersion  . extIndInfoPac $ e),
            "isLatest"               .= (getPacIsLatest . extIndInfoPac $ e),
            "additionalJannoColumns" .= extIndInfoAddCols e]

instance FromJSON ExtendedIndividualInfo where
    parseJSON = withObject "ExtendedIndividualInfo" $ \v -> ExtendedIndividualInfo
            <$> v .: "poseidonID"
            <*> v .: "groupNames"
            <*> (PacNameAndVersion <$> (v .: "packageTitle") <*> (v .: "packageVersion") <*> (v .: "isLatest"))
            <*> v .: "additionalJannoColumns"

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
