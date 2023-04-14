{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Snapshot where

import           Poseidon.Package        (PoseidonPackage (..),
                                          dummyContributor)
import           Poseidon.SecondaryTypes (ContributorSpec,
                                          VersionComponent (..),
                                          updateThreeComponentVersion)
import           Poseidon.Utils          (PoseidonException (..), PoseidonIO,
                                          logWarning)

import           Control.Monad.Catch     (throwM)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (FromJSON, ToJSON, object, parseJSON,
                                          toJSON, withObject, (.!=), (.:),
                                          (.:?), (.=))
import qualified Data.ByteString         as B
import           Data.Function           (on)
import           Data.List               (elemIndex)
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as S
import           Data.Time               (Day, UTCTime (..), getCurrentTime)
import           Data.Version            (Version, makeVersion)
import           Data.Yaml               (decodeEither')
import           Data.Yaml.Pretty        (defConfig, encodePretty,
                                          setConfCompare, setConfDropNull)
import           GitHash                 (getGitInfo, giHash)
import           System.Directory        (makeAbsolute)
import           System.FilePath         (takeDirectory)

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

-- | A data type to represent a package state
data PackageState = PackageState
    { pacStateTitle   :: String -- ^ the title of the package
    , pacStateVersion :: Maybe Version -- ^ the version of the package
    , pacStateCommit  :: Maybe String -- ^ the hash of a relevant commit where a package can be accessed in this version
                                    -- (only relevant) for our server-client architecture
    }
    deriving (Show)

instance Eq PackageState where
    (PackageState t1 v1 _) == (PackageState t2 v2 _) = (t1 == t2) && (v1 == v2)

instance Ord PackageState where
    (PackageState t1 v1 _) `compare` (PackageState t2 v2 _) = (t1,v1) `compare` (t2,v2)

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

data SnapshotMode = SimpleSnapshot | SnapshotWithGit

updateSnapshot :: PoseidonPackageSnapshot -> PoseidonPackageSnapshot -> PoseidonPackageSnapshot
updateSnapshot oldSnapshot newSnapshot =
    let oldPackageSet = S.fromList $ snapYamlPackages oldSnapshot
        newPackageSet = S.fromList $ snapYamlPackages newSnapshot
        updatedPacSet = updatePackageSet oldPackageSet newPackageSet
        oldVersion = snapYamlSnapshotVersion oldSnapshot
    in PoseidonPackageSnapshot {
      snapYamlTitle           = snapYamlTitle oldSnapshot
    , snapYamlDescription     = snapYamlDescription oldSnapshot
    , snapYamlContributor     = snapYamlContributor oldSnapshot
    , snapYamlSnapshotVersion = if updatedPacSet /= oldPackageSet
                                then case oldVersion of
                                    Just v -> Just $ updateThreeComponentVersion Minor v
                                    Nothing -> Nothing
                                else oldVersion
    , snapYamlLastModified    = snapYamlLastModified newSnapshot
    , snapYamlPackages        = S.toList updatedPacSet
    }
    where
        -- note that package comparison ignores git commits
        updatePackageSet :: S.Set PackageState -> S.Set PackageState -> S.Set PackageState
        updatePackageSet oldPacs newPacs =
            -- this implementation makes sure that the entries for old packages are kept around
            let oldNotInNew = oldPacs S.\\ newPacs
                goodOld = oldPacs S.\\ oldNotInNew
                newNotInOld = newPacs S.\\ goodOld
            in goodOld <> oldNotInNew <> newNotInOld

readSnapshot :: FilePath -> PoseidonIO PoseidonPackageSnapshot
readSnapshot p = do
    bs <- liftIO $ B.readFile p
    case decodeEither' bs of
        Left err   -> throwM $ PoseidonYamlParseException p err
        Right snap -> return snap

writeSnapshot :: FilePath -> PoseidonPackageSnapshot -> PoseidonIO ()
writeSnapshot p snapShot = liftIO $ B.writeFile p (encodePretty opts snapShot)
    where
        opts = setConfDropNull True $ setConfCompare (compare `on` fieldIndex) defConfig
        fieldIndex s = fromMaybe (length fields) $ s `elemIndex` fields
        fields = [
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
                SimpleSnapshot  -> do return Nothing
                SnapshotWithGit -> do getGitCommitHash $ posPacBaseDir pac -- doesn't really work yet: has to crawl up to find .git dir
            return $ PackageState {
                pacStateTitle   = posPacTitle pac,
                pacStateVersion = posPacPackageVersion pac,
                pacStateCommit  = commit
            }
        getGitCommitHash :: FilePath -> PoseidonIO (Maybe String)
        getGitCommitHash p = do
            eitherCommit <- liftIO $ getGitInfo p
            case eitherCommit of
                Left _ -> do
                    pAbsolute <- liftIO $ makeAbsolute p
                    let oneLevelUp = takeDirectory pAbsolute
                    if oneLevelUp == takeDirectory oneLevelUp
                    then do
                        logWarning $ "Did not find .git directory in or above " ++ show p
                        return Nothing
                    else getGitCommitHash oneLevelUp
                Right info -> do
                    return $ Just $ giHash info

