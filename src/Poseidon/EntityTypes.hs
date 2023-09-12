{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntityTypes (
    IndividualInfo (..),
    renderNameWithVersion,
    HasNameAndVersion (..),
    PoseidonEntity(..),
    SignedEntity(..),
    hasVersion, EntitiesList, SignedEntitiesList,
    PacNameAndVersion(..), makePacNameAndVersion, isLatestInCollection) where

import           Data.Maybe   (isJust)
import           Data.Version (Version, showVersion)
import           GHC.Generics (Generic)

-- | A class to represent a package-identifying property
class Eq a => HasNameAndVersion a where
    getPacName     :: a -> String        -- ^ a name property
    getPacVersion  :: a -> Maybe Version -- ^ a version property

-- | a convenience function
hasVersion :: (HasNameAndVersion a) => a -> Bool
hasVersion = isJust . getPacVersion

-- | universal rendering of package names and version
renderNameWithVersion :: (HasNameAndVersion a) => a -> String
renderNameWithVersion a = case getPacVersion a of
    Nothing -> getPacName a
    Just v  -> getPacName a ++ "-" ++ showVersion v

-- | a function to check whether a given package is the latest within a collection
isLatestInCollection :: (HasNameAndVersion a) => [a] -> a -> Bool
isLatestInCollection collection a =
    let n = getPacName a
        latest = maximum . filter ((==n) . getPacName) . map makePacNameAndVersion $ collection
    in  makePacNameAndVersion a == latest

-- | The minimal instance of HasNameAndVersion
data PacNameAndVersion = PacNameAndVersion {
      panavName    :: String
    , panavVersion :: Maybe Version
    }
    deriving (Ord, Eq)

instance HasNameAndVersion PacNameAndVersion where
    getPacName     = panavName
    getPacVersion  = panavVersion

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
    { indInfoName   :: String -- ^ the name of the individual, corresponding to jPoseidonID in Janno
    , indInfoGroups :: [String] -- ^ the groups associated with the individual, corresponding to jGroupName in Janno
    , indInfoPac    :: PacNameAndVersion -- ^ the package the individual is in.
    } deriving (Show, Eq, Ord, Generic)

instance HasNameAndVersion IndividualInfo where
    getPacName       = getPacName . indInfoPac
    getPacVersion    = getPacVersion . indInfoPac
