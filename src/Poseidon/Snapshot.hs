{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Snapshot where

import Poseidon.SecondaryTypes (ContributorSpec)

import Data.Version (Version)
import Data.Time (Day)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.!=), (.:), (.:?), (.=))
import           Data.Yaml                  (decodeEither')
import           Data.Yaml.Pretty.Extras    (ToPrettyYaml (..),
                                             encodeFilePretty)

data SnapshotYamlStruct = SnapshotYamlStruct
    { snapYamlTitle           :: Maybe String
    , snapYamlDescription     :: Maybe String
    , snapYamlContributor     :: [ContributorSpec]
    , snapYamlSnapshotVersion :: Maybe Version
    , snapYamlLastModified    :: Maybe Day
    , snapYamlPackages        :: [PackageStateSpec]
    }
    deriving (Show, Eq)

instance FromJSON SnapshotYamlStruct where
    parseJSON = withObject "PoseidonYamlStruct" $ \v -> SnapshotYamlStruct
        <$> v .:? "title"
        <*> v .:? "description"
        <*> v .:? "contributor" .!= []
        <*> v .:? "snapshotVersion"
        <*> v .:? "lastModified"
        <*> v .:? "packages" .!= []

instance ToJSON SnapshotYamlStruct where
    toJSON x = object $ [
        "title"            .= snapYamlTitle x,
        "description"      .= snapYamlDescription x] ++
        (if not $ null (snapYamlContributor x) then ["contributor" .= snapYamlContributor x] else []) ++
        ["snapshotVersion" .= snapYamlSnapshotVersion x,
        "lastModified"     .= snapYamlLastModified x] ++
        (if not $ null (snapYamlPackages x) then ["packages" .= snapYamlPackages x] else [])

instance ToPrettyYaml SnapshotYamlStruct where
    fieldOrder = const [
        "title",
        "description",
        "contributor",
        "name",
        "email",
        "orcid",
        "snapshotVersion",
        "lastModified",
        "packages",
        "title",
        "version",
        "commit"
        ]

-- | A data type to represent a package state
data PackageStateSpec = PackageStateSpec
    { pacVerTitle   :: String -- ^ the title of the package
    , pacVerVersion :: Version -- ^ the version of the package
    , pacVerCommit  :: Maybe String -- ^ the hash of a relevant commit where a package can be accessed in this version
                                    -- (only relevant) for our server-client architecture
    }
    deriving (Show, Eq)

instance FromJSON PackageStateSpec where
    parseJSON = withObject "packages" $ \v -> PackageStateSpec
        <$> v .:  "title"
        <*> v .:  "version"
        <*> v .:? "commit"

instance ToJSON PackageStateSpec where
    toJSON x = object [
          "title"    .= pacVerTitle x
        , "version" .= pacVerVersion x
        , "commit"  .= pacVerCommit x
        ]