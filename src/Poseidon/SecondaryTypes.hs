{-# LANGUAGE OverloadedStrings #-}

module Poseidon.SecondaryTypes (
    poseidonVersionParser,
    ContributorSpec (..),
    contributorSpecParser,
    IndividualInfo (..),
    GroupInfo(..),
    VersionComponent (..),
    PackageInfo(..),
    P.runParser
) where

import           Data.Aeson         (FromJSON, ToJSON, object, parseJSON,
                                     toJSON, withObject, (.:), (.:?), (.=))
import           Data.Time          (Day)
import           Data.Version       (Version (..), makeVersion)
import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P


data VersionComponent = Major
    | Minor
    | Patch
    deriving Show

data IndividualInfo = IndividualInfo
    { indInfoName    :: String
    , indInfoGroups   :: [String]
    , indInfoPacName :: String
    } deriving Show

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
    , pDescription   :: Maybe String
    , pLastModified  :: Maybe Day
    , pNrIndividuals :: Int
    }
    deriving (Show)

instance ToJSON PackageInfo where
    toJSON x = object [
        "title"         .= pTitle x,
        "version"       .= pVersion x,
        "description"   .= pDescription x,
        "lastModified"  .= pLastModified x,
        "nrIndividuals" .= pNrIndividuals x
        ]

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
        <$> v .:   "title"
        <*> v .:   "version"
        <*> v .:?  "description"
        <*> v .:   "lastModified"
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
    -- ^ the email address of a contributor
    , contributorEmail :: String -- ^ the email address of a contributor
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of ContributorSpec from JSON files
instance FromJSON ContributorSpec where
    parseJSON = withObject "contributor" $ \v -> ContributorSpec
        <$> v .: "name"
        <*> v .: "email"

instance ToJSON ContributorSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "name" .= contributorName x,
        "email" .= contributorEmail x
        ]

contributorSpecParser :: P.Parser [ContributorSpec]
contributorSpecParser = P.try (P.sepBy oneContributorSpecParser (P.char ';' <* P.spaces))

oneContributorSpecParser :: P.Parser ContributorSpec
oneContributorSpecParser = do
    name <- P.between (P.char '[') (P.char ']') (P.manyTill P.anyChar (P.lookAhead (P.char ']')))
    email <- P.between (P.char '(') (P.char ')') (P.manyTill P.anyChar (P.lookAhead (P.char ')')))
    return (ContributorSpec name email)
