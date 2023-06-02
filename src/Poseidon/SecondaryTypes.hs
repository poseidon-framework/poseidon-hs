{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Poseidon.SecondaryTypes (
    poseidonVersionParser,
    ContributorSpec (..),
    contributorSpecParser,
    IndividualInfo (..),
    GroupInfo(..),
    VersionComponent (..),
    PackageInfo(..),
    P.runParser,
    ORCID (..),
    ServerApiReturnType(..),
    ApiReturnData(..),
    ExtendedIndividualInfo(..),
    processApiResponse,
    makeNameWithVersion,
    HasNameAndVersion (..),
    PacNameAndVersion(..)
) where

import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logError, logInfo)

import           Control.Exception      (catch, throwIO)
import           Control.Monad          (forM_, guard, mzero, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON (..), Value (String),
                                         eitherDecode', object, parseJSON,
                                         toJSON, withObject, (.:), (.:?), (.=))
import           Data.Char              (digitToInt)
import           Data.List              (intercalate)
import           Data.Text              (pack, unpack)
import           Data.Time              (Day)
import           Data.Version           (Version (..), makeVersion, showVersion)
import           GHC.Generics           (Generic)
import           Network.HTTP.Conduit   (simpleHttp)
import           Poseidon.Janno         (JannoRows)
import qualified Text.Parsec            as P
import qualified Text.Parsec.String     as P

---  Client Server Communication types and functions

class HasNameAndVersion a where
    getPacName :: a -> String
    getPacVersion :: a -> Maybe Version

makeNameWithVersion :: (HasNameAndVersion a) => a -> String
makeNameWithVersion a = case getPacVersion a of
    Nothing -> getPacName a
    Just v  -> getPacName a ++ "-" ++ showVersion v

newtype PacNameAndVersion = PacNameAndVersion (String, Maybe Version) deriving (Eq)

instance HasNameAndVersion PacNameAndVersion where
    getPacName (PacNameAndVersion (n, _)) = n
    getPacVersion (PacNameAndVersion (_, v)) = v

instance ToJSON PacNameAndVersion where
    toJSON (PacNameAndVersion (n, v)) = object ["packageTitle" .= n, "packageVersion" .= v]

instance FromJSON PacNameAndVersion where
    parseJSON = withObject "PacNameAndVersion" $ \v -> do
        n <- v .: "packageTitle"
        vr <- v .: "packageVersion"
        return $ PacNameAndVersion (n, vr)

data ExtendedIndividualInfo = ExtendedIndividualInfo
    {
      extIndInfoName    :: String
    , extIndInfoGroups  :: [String]
    , extIndInfoPacName :: String
    , extIndInfoVersion :: Maybe Version
    , extIndInfoAddCols :: [(String, Maybe String)]
    }

instance HasNameAndVersion ExtendedIndividualInfo where
    getPacName = extIndInfoPacName
    getPacVersion = extIndInfoVersion

instance ToJSON ExtendedIndividualInfo where
    toJSON e =
        object [
            "Poseidon_ID" .= extIndInfoName e, -- following Janno column names
            "Group_Names" .= extIndInfoGroups e,
            "packageTitle" .= extIndInfoPacName e, -- following mostly the Poseidon YAML definition where possible
            "packageVersion" .= extIndInfoVersion e,
            "additionalJannoColumns" .= extIndInfoAddCols e]

instance FromJSON ExtendedIndividualInfo where
    parseJSON = withObject "ExtendedIndividualInfo" $ \v -> ExtendedIndividualInfo
            <$> v .: "Poseidon_ID"
            <*> v .: "Group_Names"
            <*> v .: "packageTitle"
            <*> v .: "packageVersion"
            <*> v .: "additionalJannoColumns"

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
    toJSON (PackageInfo title version posVersion description lastModified nrIndividuals) =
        object [
            "title" .= title,
            "packageVersion" .= version,
            "poseidonVersion" .= posVersion,
            "description" .= description,
            "lastModified" .= lastModified,
            "nrIndividuals" .= nrIndividuals
        ]

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
            <$> v .: "title"
            <*> v .: "packageVersion"
            <*> v .: "poseidonVersion"
            <*> v .: "description"
            <*> v .: "lastModified"
            <*> v .: "nrIndividuals"

data GroupInfo = GroupInfo
    { gName          :: String
    , gPackageNames  :: [PacNameAndVersion]
    , gNrIndividuals :: Int
    }

instance ToJSON GroupInfo where
    toJSON (GroupInfo name pacNames nrIndividuals) =
        object [
            "groupName" .= name,
            "packageNames" .= pacNames,
            "nrIndividuals" .= nrIndividuals
        ]

instance FromJSON GroupInfo where
    parseJSON = withObject "GroupInfo" $ \v -> GroupInfo
            <$> v .: "groupName"
            <*> v .: "packageNames"
            <*> v .: "nrIndividuals"

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
    toJSON (ApiReturnExtIndividualInfo extIndInfo) =
        object [
            "constructor" .= String "ApiReturnExtIndividualInfo",
            "extIndInfo" .= extIndInfo
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
            "ApiReturnExtIndividualInfo" -> ApiReturnExtIndividualInfo <$> v .: "extIndInfo"
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


--- Other types and functions not exclusively used for Client-Server Comm
data VersionComponent = Major
    | Minor
    | Patch
    deriving Show

data IndividualInfo = IndividualInfo
    { indInfoName    :: String
    , indInfoGroups  :: [String]
    , indInfoPacName :: String
    } deriving (Show, Ord, Generic)

instance Eq IndividualInfo where
    (==) (IndividualInfo a1 b1 c1) (IndividualInfo a2 b2 c2) = a1 == a2 && head b1 == head b2 && c1 == c2


poseidonVersionParser :: P.Parser Version
poseidonVersionParser = do
    major <- read <$> P.many1 P.digit
    _ <- P.oneOf "."
    minor <- read <$> P.many1 P.digit
    _ <- P.oneOf "."
    patch <- read <$> P.many1 P.digit
    return (makeVersion [major, minor, patch])


-- | A data type to represent a contributor
data ContributorSpec = ContributorSpec
    { contributorName  :: String -- ^ the name of a contributor
    , contributorEmail :: String -- ^ the email address of a contributor
    , contributorORCID :: Maybe ORCID -- ^ the ORCID of a contributor
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of ContributorSpec from JSON files
instance FromJSON ContributorSpec where
    parseJSON = withObject "contributor" $ \v -> ContributorSpec
        <$> v .:  "name"
        <*> v .:  "email"
        <*> v .:? "orcid"

instance ToJSON ContributorSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
          "name"  .= contributorName x
        , "email" .= contributorEmail x
        , "orcid" .= contributorORCID x
        ]

contributorSpecParser :: P.Parser [ContributorSpec]
contributorSpecParser = P.try (P.sepBy oneContributorSpecParser (P.char ';' <* P.spaces))

oneContributorSpecParser :: P.Parser ContributorSpec
oneContributorSpecParser = do
    name <- P.between (P.char '[') (P.char ']') (P.manyTill P.anyChar (P.lookAhead (P.char ']')))
    email <- P.between (P.char '(') (P.char ')') (P.manyTill P.anyChar (P.lookAhead (P.char ')')))
    -- TODO: add option to add ORCID here
    return (ContributorSpec name email Nothing)

-- | A data type to represent an ORCID
-- see https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier
data ORCID = ORCID
    { _orcidNums     :: [Char]
    , _orcidChecksum :: Char
    }
    deriving (Show, Eq)

instance FromJSON ORCID where
    parseJSON (String s) = case P.runParser parseORCID () "" (unpack s) of
        Left err -> fail $ show err
        Right x  -> pure x
    parseJSON _          = mzero

instance ToJSON ORCID where
    toJSON x = String $ pack $ renderORCID x

parseORCID :: P.Parser ORCID
parseORCID = do
    orcid <- (\a b c d e -> ORCID (concat [a,b,c,d]) e) <$>
            fourBlock <* m
        <*> fourBlock <* m
        <*> fourBlock <* m
        <*> threeBlock <*> checksumDigit <* P.eof
    guard (validateORCID orcid) P.<?> "ORCID is not valid"
    return orcid
  where
      fourBlock = P.count 4 P.digit
      m = P.oneOf "-"
      threeBlock = P.count 3 P.digit
      checksumDigit = P.digit P.<|> P.char 'X'

validateORCID :: ORCID -> Bool
validateORCID (ORCID nums check) =
    let numsInt = map digitToInt nums
        total = makeTotal 0 numsInt
        remainder = total `mod` 11
        result = (12 - remainder) `mod` 11
        checkInt = if check == 'X' then 10 else digitToInt check
    in result == checkInt
    where
        makeTotal :: Int -> [Int] -> Int
        makeTotal a []     = a
        makeTotal a (x:xs) = makeTotal ((a + x) * 2) xs

renderORCID :: ORCID -> String
renderORCID (ORCID nums check) =
    intercalate "-" (chunks 4 nums) ++ [check]
    where
        chunks :: Int -> [a] -> [[a]]
        chunks _ [] = []
        chunks n xs =
            let (ys, zs) = splitAt n xs
            in  ys : chunks n zs

