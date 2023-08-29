{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntityTypes (
    IndividualInfo (..), getIndName,
    renderNameWithVersion,
    HasNameAndVersion (..),
    PacNameAndVersion(..), PoseidonIndividual(..), makePacNameAndVersion) where

import           Data.Aeson   (FromJSON (..), KeyValue ((.=)), ToJSON (..),
                               object, withObject, (.:))
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
    show (IndividualInfo i g p) = "<" ++ renderNameWithVersion p ++ ":" ++ (head g) ++ ":" ++ i ++ ">"

instance HasNameAndVersion IndividualInfo where
    getPacName    = getPacName . indInfoPac
    getPacVersion = getPacVersion . indInfoPac

class HasNameAndVersion a where
    getPacName    :: a -> String
    getPacVersion :: a -> Maybe Version

renderNameWithVersion :: (HasNameAndVersion a) => a -> String
renderNameWithVersion a = case getPacVersion a of
    Nothing -> getPacName a
    Just v  -> getPacName a ++ "-" ++ showVersion v

data PacNameAndVersion = PacNameAndVersion {
      panavName    :: String
    , panavVersion :: Maybe Version
    }
    deriving (Ord)

instance Eq PacNameAndVersion where
    (==) (PacNameAndVersion n1 Nothing) (PacNameAndVersion n2 Nothing) = n1 == n2
    (==) (PacNameAndVersion n1 Nothing) (PacNameAndVersion n2 (Just _)) = n1 == n2
    (==) (PacNameAndVersion n1 (Just _)) (PacNameAndVersion n2 Nothing) = n1 == n2
    (==) (PacNameAndVersion n1 (Just v1)) (PacNameAndVersion n2 (Just v2)) = n1 == n2 && v1 == v2

instance Show PacNameAndVersion where
    show a = "*" ++ renderNameWithVersion a ++ "*"

makePacNameAndVersion :: (HasNameAndVersion a) => a -> PacNameAndVersion
makePacNameAndVersion a = PacNameAndVersion (getPacName a) (getPacVersion a)

instance HasNameAndVersion PacNameAndVersion where
    getPacName    (PacNameAndVersion n _) = n
    getPacVersion (PacNameAndVersion _ v) = v
