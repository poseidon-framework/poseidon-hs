{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntityTypes (
    IndividualInfo (..),
    renderNameWithVersion,
    HasNameAndVersion (..),
    PoseidonEntity(..),
    SignedEntity(..),
    hasVersion, EntitiesList, SignedEntitiesList,
    PacNameAndVersion(..), makePacNameAndVersion,
    setIsLatestInList) where

import           Data.List    (groupBy, sortBy)
import           Data.Maybe   (isJust)
import           Data.Version (Version, showVersion)
import           GHC.Generics (Generic)

-- | A class to represent a package-identifying property
class Eq a => HasNameAndVersion a where
    getPacName     :: a -> String        -- ^ a name property
    getPacVersion  :: a -> Maybe Version -- ^ a version property
    getPacIsLatest :: a -> Bool          -- ^ whether that package is the latest of its kind in a given collection
    setPacIsLatest :: a -> a             -- ^ a setter for the isLatest property

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
      panavName     :: String
    , panavVersion  :: Maybe Version
    , panavIsLatest :: Bool
    }
    deriving (Ord, Eq)

instance HasNameAndVersion PacNameAndVersion where
    getPacName     = panavName
    getPacVersion  = panavVersion
    getPacIsLatest = panavIsLatest
    setPacIsLatest a = a {panavIsLatest = True}

instance Show PacNameAndVersion where
    show a = "*" ++ renderNameWithVersion a ++ "*"

-- | a function to normalise any instance of HasNameAndVersion to the minimal concrete type PacNameAndVersion
makePacNameAndVersion :: (HasNameAndVersion a) => a -> PacNameAndVersion
makePacNameAndVersion a = PacNameAndVersion (getPacName a) (getPacVersion a) (getPacIsLatest a)

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
    } deriving (Show, Eq, Ord, Generic)

instance HasNameAndVersion IndividualInfo where
    getPacName       = getPacName . indInfoPac
    getPacVersion    = getPacVersion . indInfoPac
    getPacIsLatest   = getPacIsLatest . indInfoPac
    setPacIsLatest a = let pac = indInfoPac a in a {indInfoPac = setPacIsLatest pac}

setIsLatestInList :: (HasNameAndVersion a) => [a] -> [a]
setIsLatestInList as = 
    let allLatestPacs = map last . groupBy (\a b -> (getPacName a, getPacVersion a) == (getPacName b, getPacVersion b)) . sortBy (\a b -> compare (getPacName a, getPacVersion a) (getPacName b, getPacVersion b)) $ as
    in  do -- loop over ret
            a <- as
            let isLatest = a `elem` allLatestPacs
            if isLatest then return . setPacIsLatest $ a else return a
