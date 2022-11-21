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
    ORCID (..)
) where

import           Control.Monad      (mzero)
import           Data.Aeson         (FromJSON, ToJSON, object, parseJSON,
                                     toJSON, withObject, (.:), (.:?), (.=),
                                     Value (String), pairs)
import           Data.List          (intercalate, splitAt)
import           Data.Text          (unpack, pack)
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
    , indInfoGroups  :: [String]
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

-- | A data type to represent an ORCID
data ORCID = ORCID
    { _orcidNums :: [Char]
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

-- TODO: implemented not just ORCID parsing, but also validation
parseORCID :: P.Parser ORCID
parseORCID = do
  (\a b c d e -> ORCID (concat [a,b,c,d]) e) <$>
        fourBlock <* m
    <*> fourBlock <* m
    <*> fourBlock <* m
    <*> threeBlock <*> checksumDigit <* P.eof
  where
      fourBlock = P.count 4 P.digit
      m = P.oneOf "-"
      threeBlock = P.count 3 P.digit
      checksumDigit = P.digit P.<|> P.char 'X'

renderORCID :: ORCID -> String
renderORCID (ORCID nums check) =
    intercalate "-" (chunks 4 nums) ++ [check] 
    where
        chunks :: Int -> [a] -> [[a]]
        chunks _ [] = []
        chunks n xs =
            let (ys, zs) = splitAt n xs
            in  ys : chunks n zs

contributorSpecParser :: P.Parser [ContributorSpec]
contributorSpecParser = P.try (P.sepBy oneContributorSpecParser (P.char ';' <* P.spaces))

oneContributorSpecParser :: P.Parser ContributorSpec
oneContributorSpecParser = do
    name <- P.between (P.char '[') (P.char ']') (P.manyTill P.anyChar (P.lookAhead (P.char ']')))
    email <- P.between (P.char '(') (P.char ')') (P.manyTill P.anyChar (P.lookAhead (P.char ')')))
    -- TODO: add option to add ORCID here
    return (ContributorSpec name email Nothing)
