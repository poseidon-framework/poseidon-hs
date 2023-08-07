{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntityTypes (
    IndividualInfo (..), getIndName, PackageInfo (..), GroupInfo (..), ExtendedIndividualInfo (..),
    makeNameWithVersion,
    HasNameAndVersion (..),
    PacNameAndVersion(..), PoseidonIndividual(..)) where

import           Data.Aeson   (FromJSON (..), KeyValue ((.=)), ToJSON (..),
                               object, withObject, (.:))
import           Data.Time    (Day)
import           Data.Version (Version, showVersion)
import           GHC.Generics (Generic)

-- Entity definiting data types

-- | A datatype to represent a package, a group or an individual
data PoseidonIndividual =
      SimpleInd String
    | SpecificInd IndividualInfo
    deriving (Eq, Ord)

getIndName :: PoseidonIndividual -> String
getIndName (SimpleInd n)                        = n
getIndName (SpecificInd (IndividualInfo n _ _)) = n

instance Show PoseidonIndividual where
    show (SimpleInd   i) = "<" ++ i ++ ">"
    show (SpecificInd i) = show i

data IndividualInfo = IndividualInfo
    { indInfoName   :: String
    , indInfoGroups :: [String]
    , indInfoPac    :: PacNameAndVersion
    } deriving (Ord, Generic)

instance Eq IndividualInfo where
    (==) (IndividualInfo a1 b1 c1) (IndividualInfo a2 b2 c2) = a1 == a2 && head b1 == head b2 && c1 == c2

instance Show IndividualInfo where
    show (IndividualInfo i g p) = "<" ++ show p ++ ":" ++ (head g) ++ ":" ++ i ++ ">"

class HasNameAndVersion a where
    getPacName :: a -> String
    getPacVersion :: a -> Maybe Version

makeNameWithVersion :: (HasNameAndVersion a) => a -> String
makeNameWithVersion a = case getPacVersion a of
    Nothing -> getPacName a
    Just v  -> getPacName a ++ "-" ++ showVersion v

newtype PacNameAndVersion = PacNameAndVersion (String, Maybe Version) deriving (Eq, Ord)

instance Show PacNameAndVersion where
    show = makeNameWithVersion

instance HasNameAndVersion PacNameAndVersion where
    getPacName (PacNameAndVersion (n, _)) = n
    getPacVersion (PacNameAndVersion (_, v)) = v

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
            "poseidonID" .= extIndInfoName e, -- following Janno column names
            "groupNames" .= extIndInfoGroups e,
            "packageTitle" .= extIndInfoPacName e, -- following mostly the Poseidon YAML definition where possible
            "packageVersion" .= extIndInfoVersion e,
            "additionalJannoColumns" .= extIndInfoAddCols e]

instance FromJSON ExtendedIndividualInfo where
    parseJSON = withObject "ExtendedIndividualInfo" $ \v -> ExtendedIndividualInfo
            <$> v .: "poseidonID"
            <*> v .: "groupNames"
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
    toJSON (PackageInfo title pacVersion posVersion description lastModified nrIndividuals) =
        object [
            "packageTitle" .= title,
            "packageVersion" .= pacVersion,
            "poseidonVersion" .= posVersion,
            "description" .= description,
            "lastModified" .= lastModified,
            "nrIndividuals" .= nrIndividuals
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
    , gPackageNames  :: PacNameAndVersion
    , gNrIndividuals :: Int
    }

instance ToJSON GroupInfo where
    toJSON (GroupInfo name (PacNameAndVersion (pacTitle, pacVersion)) nrIndividuals) =
        object [
            "groupName" .= name,
            "packageTitle" .= pacTitle,
            "packageVersion" .= pacVersion,
            "nrIndividuals" .= nrIndividuals
        ]

instance FromJSON GroupInfo where
    parseJSON = withObject "GroupInfo" $ \v -> do
        groupName <- v .: "groupName"
        packageTitle <- v .: "packageTitle"
        packageVersion <- v .: "packageVersion"
        nrIndividuals <- v .: "nrIndividuals"
        return $ GroupInfo groupName (PacNameAndVersion (packageTitle, packageVersion)) nrIndividuals
