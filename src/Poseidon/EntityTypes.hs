{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntityTypes (
    IndividualInfo (..),
    renderNameWithVersion,
    HasNameAndVersion (..),
    PoseidonEntity(..),
    SignedEntity(..),
    hasVersion, EntitiesList, SignedEntitiesList,
    setPacVersionLatest,
    PacNameAndVersion(..), makePacNameAndVersion) where

import           Data.Aeson   (FromJSON (..), KeyValue ((.=)), ToJSON (..),
                               object, withObject, (.:))
import           Data.List    (groupBy, sort, nub)
import           Data.Maybe   (isJust)
import           Data.Version (Version, showVersion)
import           GHC.Generics (Generic)

-- | A class to represent a package-identifying property
class HasNameAndVersion a where
    getPacName    :: a -> String        -- ^ a name property
    getPacVersion :: a -> Maybe Version -- ^ a version property

-- | a convenience function
hasVersion :: (HasNameAndVersion a) => a -> Bool
hasVersion = isJust . getPacVersion

-- | universal rendering of package names and version
renderNameWithVersion :: (HasNameAndVersion a) => a -> String
renderNameWithVersion a = case getPacVersion a of
    Nothing -> getPacName a
    Just v  -> getPacName a ++ "-" ++ showVersion v

-- | The minimal instance of HasNameAndVersion
data PacNameAndVersion = PacNameAndVersion {
      panavName    :: String
    , panavVersion :: Maybe Version
    }
    deriving (Ord, Eq)

instance HasNameAndVersion PacNameAndVersion where
    getPacName    = panavName
    getPacVersion = panavVersion

instance Show PacNameAndVersion where
    show a = "*" ++ renderNameWithVersion a ++ "*"

-- | a function to normalise any instance of HasNameAndVersion to the minimal concrete type PacNameAndVersion
makePacNameAndVersion :: (HasNameAndVersion a) => a -> PacNameAndVersion
makePacNameAndVersion a = PacNameAndVersion (getPacName a) (getPacVersion a)

-- | A datatype to represent a requested package, group or individual
data PoseidonEntity =
      Pac PacNameAndVersion -- ^ all individuals in a package. A version can be specified, if not implicitly request the latest
    | Group String          -- ^ all individuals with a given group, in all of the latest packages
    | Ind String            -- ^ all individuals with the given name, in all of the latest packages
    | SpecificInd String String PacNameAndVersion -- ^ the individual specified by its name, group and package. If not versioned, then take the latest version.
    deriving (Eq, Ord)

-- | A show instance for rendering entities in forgescript
instance Show PoseidonEntity where
    show (Pac   p) = show p
    show (Group g) = g
    show (Ind   n) = "<" ++ n ++ ">"
    show (SpecificInd n g p) = "<" ++ renderNameWithVersion p ++ ":" ++ g ++ ":" ++ n ++ ">"

type EntitiesList = [PoseidonEntity]

-- | a signed entity specification, denoting inclusion or exclusion of an entity
data SignedEntity =
      Include PoseidonEntity
    | Exclude PoseidonEntity
    deriving (Eq, Ord)

instance Show SignedEntity where
    show (Include a) = show a
    show (Exclude a) = "-" ++ show a

type SignedEntitiesList = [SignedEntity]

-- | a minimal datatype representing an individual in a collection of packages
data IndividualInfo = IndividualInfo
    { indInfoName     :: String -- ^ the name of the individual, corresponding to jPoseidonID in Janno
    , indInfoGroups   :: [String] -- ^ the groups associated with the individual, corresponding to jGroupName in Janno
    , indInfoPac      :: PacNameAndVersion -- ^ the package the individual is in.
    , indInfoIsLatest :: Bool -- ^ whether this package is the latest package in the collection
    , indInfoAddCols  :: [(String, Maybe String)] -- ^ additional key-value pairs obtained from the Janno. Needed for server-communication
    } deriving (Show, Eq, Ord, Generic)

instance HasNameAndVersion IndividualInfo where
    getPacName    = getPacName . indInfoPac
    getPacVersion = getPacVersion . indInfoPac

-- these JSON instances are required for the Server-Client communication
instance ToJSON IndividualInfo where
    toJSON e =
        object [
            -- following Janno column names
            "poseidonID"             .= indInfoName e,
            "groupNames"             .= indInfoGroups e,
             -- following mostly the Poseidon YAML definition where possible
            "packageTitle"           .= (panavName . indInfoPac $ e),
            "packageVersion"         .= (panavVersion . indInfoPac $ e),
            "additionalJannoColumns" .= indInfoAddCols e
            -- we skip indInfoIsLatest for backwards-compatibility reasons. It can be simply computed after the fact.
            ]

instance FromJSON IndividualInfo where
    parseJSON = withObject "IndividualInfo" $ \v -> IndividualInfo
            <$> v .: "poseidonID"
            <*> v .: "groupNames"
            <*> (PacNameAndVersion <$> (v .: "packageTitle") <*> (v .: "packageVersion"))
            <*> pure False -- we set isLatest by default to False. It needs to be set by setPacVersionLatest
            <*> v .: "additionalJannoColumns"

setPacVersionLatest :: [IndividualInfo] -> [IndividualInfo]
setPacVersionLatest indInfos =
    let allLatestPacs = map last . groupBy (\a b -> panavName a == panavName b) . sort . nub . map indInfoPac $ indInfos
    in  do -- loop over ret
            indInfo <- indInfos
            let isLatest = indInfoPac indInfo `elem` allLatestPacs
            return $ indInfo {indInfoIsLatest = isLatest}

