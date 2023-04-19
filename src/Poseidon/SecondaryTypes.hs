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
    ApiReturnData(..)
) where

import           Control.Monad      (guard, mzero)
import           Data.Aeson         (FromJSON, Key, KeyValue, ToJSON (..),
                                     Value (String), defaultOptions,
                                     genericToEncoding, object, parseJSON,
                                     toJSON, withObject, (.:), (.:?), (.=))
import           Data.Char          (digitToInt)
import           Data.List          (intercalate)
import           Data.Maybe         (catMaybes)
import           Data.Text          (pack, unpack)
import           Data.Time          (Day)
import           Data.Version       (Version (..), makeVersion)
import           GHC.Generics       (Generic)
import           Poseidon.Janno     (JannoRows)
import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P

data VersionComponent = Major
    | Minor
    | Patch
    deriving Show

data IndividualInfo = IndividualInfo
    { indInfoName    :: String
    , indInfoGroups  :: [String]
    , indInfoPacName :: String
    } deriving (Show, Ord)

instance Eq IndividualInfo where
    (==) (IndividualInfo a1 b1 c1) (IndividualInfo a2 b2 c2) = a1 == a2 && head b1 == head b2 && c1 == c2

instance ToJSON IndividualInfo where
    toJSON x = object [
        "name"    .= indInfoName x,
        "group"   .= indInfoGroups x,
        "pacName" .= indInfoPacName x]

instance FromJSON IndividualInfo where
    parseJSON = withObject "IndividualInfo" $ \v -> IndividualInfo
        <$> v .:   "name"
        <*> v .:   "group"
        <*> v .:  "pacName"

-- | Minimal package representation on Poseidon servers
data PackageInfo = PackageInfo
    { pTitle         :: String
    , pVersion       :: Maybe Version
    , pPosVersion    :: Version
    , pDescription   :: Maybe String
    , pLastModified  :: Maybe Day
    , pNrIndividuals :: Int
    }
    deriving (Show)

instance ToJSON PackageInfo where
    toJSON x = object . catMaybes [
        "title"           .=! pTitle x,
        "version"         .=? pVersion x,
        "poseidonVersion" .=! pPosVersion x,
        "description"     .=? pDescription x,
        "lastModified"    .=? pLastModified x,
        "nrIndividuals"   .=! pNrIndividuals x
        ]

-- the following are just two quick wrappers around Aesons (.=) which return Maybes. They make omitting Nothings easy, using catMaybes, see above.
(.=?) :: (ToJSON v, KeyValue kv) => Key -> Maybe v -> Maybe kv
(.=?) key maybeVal = case maybeVal of
    Nothing  -> Nothing
    Just val -> Just (key .= val)
infixr 8 .=?

(.=!) :: (ToJSON v, KeyValue kv) => Key -> v -> Maybe kv
(.=!) key val = Just (key .= val)
infixr 8 .=!

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
        <$> v .:   "title"
        <*> v .:?  "version"
        <*> v .:   "poseidonVersion"
        <*> v .:?  "description"
        <*> v .:?  "lastModified"
        <*> v .:   "nrIndividuals"

data GroupInfo = GroupInfo
    { gName          :: String
    , gPackageNames  :: [String]
    , gNrIndividuals :: Int
    }

instance ToJSON GroupInfo where
    toJSON x = object [
        "name"          .= gName x,
        "packages"      .= gPackageNames x,
        "nrIndividuals" .= gNrIndividuals x]

instance FromJSON GroupInfo where
    parseJSON = withObject "GroupInfo" $ \v -> GroupInfo
        <$> v .: "name"
        <*> v .: "packages"
        <*> v .: "nrIndividuals"

data ServerApiReturnType = ServerApiReturnType {
    _apiMessages :: [String],
    _apiResponse :: Maybe ApiReturnData
} deriving (Generic)

instance ToJSON ServerApiReturnType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerApiReturnType

data ApiReturnData = ApiReturnPackageInfo [PackageInfo]
                   | ApiReturnGroupInfo [GroupInfo]
                   | ApiReturnIndividualInfo [IndividualInfo]
                   | ApiReturnJanno [(String, JannoRows)] deriving (Generic)

instance ToJSON ApiReturnData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ApiReturnData

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
