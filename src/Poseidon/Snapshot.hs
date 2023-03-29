{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Snapshot where

import Poseidon.SecondaryTypes (ContributorSpec)
import Poseidon.Package (PoseidonPackage (..), dummyContributor)
import Poseidon.Utils (PoseidonIO, logDebug, logWarning)

import Data.Version (Version, makeVersion)
import Data.Time (Day, UTCTime (..), getCurrentTime)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.!=), (.:), (.:?), (.=))
import           Data.Yaml                  (decodeEither')
import           Data.Yaml.Pretty.Extras    (ToPrettyYaml (..),
                                             encodeFilePretty)
import GitHash (getGitInfo, giHash)
import           Control.Monad.IO.Class     (liftIO)

data PoseidonPackageSnapshot = PoseidonPackageSnapshot
    { snapYamlTitle           :: Maybe String
    , snapYamlDescription     :: Maybe String
    , snapYamlContributor     :: [ContributorSpec]
    , snapYamlSnapshotVersion :: Maybe Version
    , snapYamlLastModified    :: Maybe Day
    , snapYamlPackages        :: [PackageState]
    }
    deriving (Show, Eq)

instance FromJSON PoseidonPackageSnapshot where
    parseJSON = withObject "PoseidonYamlStruct" $ \v -> PoseidonPackageSnapshot
        <$> v .:? "title"
        <*> v .:? "description"
        <*> v .:? "contributor" .!= []
        <*> v .:? "snapshotVersion"
        <*> v .:? "lastModified"
        <*> v .:? "packages" .!= []

instance ToJSON PoseidonPackageSnapshot where
    toJSON x = object $ [
        "title"            .= snapYamlTitle x,
        "description"      .= snapYamlDescription x] ++
        (if not $ null (snapYamlContributor x) then ["contributor" .= snapYamlContributor x] else []) ++
        ["snapshotVersion" .= snapYamlSnapshotVersion x,
        "lastModified"     .= snapYamlLastModified x] ++
        (if not $ null (snapYamlPackages x) then ["packages" .= snapYamlPackages x] else [])

instance ToPrettyYaml PoseidonPackageSnapshot where
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
data PackageState = PackageState
    { pacStateTitle   :: String -- ^ the title of the package
    , pacStateVersion :: Maybe Version -- ^ the version of the package
    , pacStateCommit  :: Maybe String -- ^ the hash of a relevant commit where a package can be accessed in this version
                                    -- (only relevant) for our server-client architecture
    }
    deriving (Show, Eq)

instance FromJSON PackageState where
    parseJSON = withObject "packages" $ \v -> PackageState
        <$> v .:  "title"
        <*> v .:? "version"
        <*> v .:? "commit"

instance ToJSON PackageState where
    toJSON x = object [
          "title"   .= pacStateTitle x
        , "version" .= pacStateVersion x
        , "commit"  .= pacStateCommit x
        ]

data SnapshotMode = Simple | WithGit

makeSnapshot :: SnapshotMode -> [PoseidonPackage] -> PoseidonIO PoseidonPackageSnapshot
makeSnapshot snapMode pacs = do
    snap <- makeMinimalSnapshot snapMode pacs
    (UTCTime today _) <- liftIO getCurrentTime
    return $ snap {
      snapYamlTitle           = Just "Snapshot title"
    , snapYamlDescription     = Just "Snapshot description"
    , snapYamlContributor     = [dummyContributor]
    , snapYamlSnapshotVersion = Just $ makeVersion [0, 1, 0]
    , snapYamlLastModified    = Just today
    }

makeMinimalSnapshot :: SnapshotMode -> [PoseidonPackage] -> PoseidonIO PoseidonPackageSnapshot
makeMinimalSnapshot snapMode pacs = do
    pacSnapshots <- snapshotPackages snapMode pacs
    return $ PoseidonPackageSnapshot {
      snapYamlTitle           = Nothing
    , snapYamlDescription     = Nothing
    , snapYamlContributor     = []
    , snapYamlSnapshotVersion = Nothing
    , snapYamlLastModified    = Nothing
    , snapYamlPackages        = pacSnapshots
    }

snapshotPackages :: SnapshotMode -> [PoseidonPackage] -> PoseidonIO [PackageState]
snapshotPackages snapMode = mapM snapOne
    where
        snapOne :: PoseidonPackage -> PoseidonIO PackageState
        snapOne pac = do
            commit <- case snapMode of
                Simple -> do return Nothing
                WithGit -> do getGitCommitHash $ posPacBaseDir pac
            return $ PackageState {
                pacStateTitle   = posPacTitle pac,
                pacStateVersion = posPacPackageVersion pac,
                pacStateCommit  = commit
            }
        getGitCommitHash :: FilePath -> PoseidonIO (Maybe String)
        getGitCommitHash p = do
            eitherCommit <- liftIO $ getGitInfo p
            case eitherCommit of
                Left e -> do
                    logWarning $ show e
                    return Nothing
                Right info -> do
                    return $ Just $ giHash info

