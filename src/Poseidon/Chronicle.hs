{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Chronicle where

import           Poseidon.Package        (PoseidonPackage (..))
import           Poseidon.SecondaryTypes (HasNameAndVersion (..),
                                          VersionComponent (..),
                                          updateThreeComponentVersion)
import           Poseidon.Utils          (PoseidonException (..), PoseidonIO)

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
import           System.Directory        (createDirectoryIfMissing,
                                          makeAbsolute)
import           System.FilePath         (takeDirectory)

data PoseidonPackageChronicle = PoseidonPackageChronicle
    { snapYamlTitle            :: String
    , snapYamlDescription      :: Maybe String
    , snapYamlChronicleVersion :: Version
    , snapYamlLastModified     :: Day
    , snapYamlPackages         :: S.Set PackageIteration
    }
    deriving (Show, Eq)

instance FromJSON PoseidonPackageChronicle where
    parseJSON = withObject "PoseidonYamlStruct" $ \v -> PoseidonPackageChronicle
        <$> v .:  "title"
        <*> v .:? "description"
        <*> v .:  "chronicleVersion"
        <*> v .:  "lastModified"
        <*> v .:? "packages" .!= S.empty

instance ToJSON PoseidonPackageChronicle where
    toJSON x = object $ [
        "title"            .= snapYamlTitle x,
        "description"      .= snapYamlDescription x,
        "chronicleVersion" .= snapYamlChronicleVersion x,
        "lastModified"     .= snapYamlLastModified x] ++
        if not $ null (snapYamlPackages x) then ["packages" .= snapYamlPackages x] else []

-- | A data type to represent a package state
data PackageIteration = PackageIteration
    { pacStateTitle   :: String   -- ^ the title of the package
    , pacStateVersion :: Version  -- ^ the version of the package
    , pacStateCommit  :: String   -- ^ the hash of a relevant commit where a package can be accessed in this version
    , pacStatePath    :: FilePath -- ^ the file path where the POSEIDON.yml file is stored
    }
    deriving (Show)

instance Eq PackageIteration where
    (PackageIteration t1 v1 _ _) == (PackageIteration t2 v2 _ _) = (t1 == t2) && (v1 == v2)

instance Ord PackageIteration where
    (PackageIteration t1 v1 _ _) `compare` (PackageIteration t2 v2 _ _) = (t1,v1) `compare` (t2,v2)

instance FromJSON PackageIteration where
    parseJSON = withObject "packages" $ \v -> PackageIteration
        <$> v .: "title"
        <*> v .: "version"
        <*> v .: "commit"
        <*> v .: "path"

instance ToJSON PackageIteration where
    toJSON x = object [
          "title"   .= pacStateTitle x
        , "version" .= pacStateVersion x
        , "commit"  .= pacStateCommit x
        , "path"    .= pacStatePath x
        ]

instance HasNameAndVersion PackageIteration where
    getPacName (PackageIteration t _ _ _) = t
    getPacVersion (PackageIteration _ v _ _) = Just v

updateChronicle :: PoseidonPackageChronicle -> PoseidonPackageChronicle -> PoseidonPackageChronicle
updateChronicle oldChronicle newChronicle =
    let oldPackageSet = snapYamlPackages oldChronicle
        newPackageSet = snapYamlPackages newChronicle
        mergedPacSet = S.union oldPackageSet newPackageSet
        oldChronicleVersion = snapYamlChronicleVersion oldChronicle
    in PoseidonPackageChronicle {
      snapYamlTitle            = snapYamlTitle oldChronicle
    , snapYamlDescription      = snapYamlDescription oldChronicle
    , snapYamlChronicleVersion = if mergedPacSet /= oldPackageSet
                                 then updateThreeComponentVersion Minor oldChronicleVersion
                                 else oldChronicleVersion
    , snapYamlLastModified     = snapYamlLastModified newChronicle
    , snapYamlPackages         = mergedPacSet
    }

readChronicle :: FilePath -> PoseidonIO PoseidonPackageChronicle
readChronicle p = do
    bs <- liftIO $ B.readFile p
    case decodeEither' bs of
        Left err   -> throwM $ PoseidonYamlParseException p err
        Right snap -> return snap

writeChronicle :: FilePath -> PoseidonPackageChronicle -> PoseidonIO ()
writeChronicle p snapShot = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory p
    liftIO $ B.writeFile p (encodePretty opts snapShot)
    where
        opts = setConfDropNull True $ setConfCompare (compare `on` fieldIndex) defConfig
        fieldIndex s = fromMaybe (length fields) $ s `elemIndex` fields
        fields = [
            "title",
            "description",
            "chronicleVersion",
            "lastModified",
            "packages",
            "title",
            "version",
            "commit"
         ]

makeChronicle :: Bool -> [PoseidonPackage] -> PoseidonIO PoseidonPackageChronicle
makeChronicle testMode pacs = do
    pacChronicles <- chroniclePackages testMode pacs
    (UTCTime today _) <- liftIO getCurrentTime
    return $ PoseidonPackageChronicle {
      snapYamlTitle            = "Chronicle title"
    , snapYamlDescription      = Just "Chronicle description"
    , snapYamlChronicleVersion = makeVersion [0, 1, 0]
    , snapYamlLastModified     = today
    , snapYamlPackages         = pacChronicles
    }

chroniclePackages :: Bool -> [PoseidonPackage] -> PoseidonIO (S.Set PackageIteration)
chroniclePackages testMode pacs = do
    pacStateList <- mapM snapOne pacs
    return $ S.fromList pacStateList
    where
        snapOne :: PoseidonPackage -> PoseidonIO PackageIteration
        snapOne pac = do
            version <- getPackageVersion testMode pac
            commit <- liftIO $ getGitCommitHash testMode $ posPacBaseDir pac
            return $ PackageIteration {
                pacStateTitle   = posPacTitle pac,
                pacStateVersion = version,
                pacStateCommit  = commit,
                pacStatePath    = posPacBaseDir pac
            }

getPackageVersion :: Bool -> PoseidonPackage -> PoseidonIO Version
getPackageVersion testMode pac =
    case posPacPackageVersion pac of
        Just v -> return v
        Nothing -> do
            if testMode
            then return $ makeVersion [0, 0, 0]
            else do
                throwM $ PoseidonChronicleException $
                    "Package " ++ show (posPacTitle pac) ++ " has no version."

getGitCommitHash :: Bool -> FilePath -> IO String
getGitCommitHash testMode p =
    if testMode
    then return "MyGitCommitHash"
    else do
        eitherGit <- liftIO $ getGitInfo p
        case eitherGit of
            Left _ -> do
                pAbsolute <- liftIO $ makeAbsolute p
                let oneLevelUp = takeDirectory pAbsolute
                if oneLevelUp == takeDirectory oneLevelUp
                then do throwM $ PoseidonGitException p
                else getGitCommitHash False oneLevelUp
            Right info -> do
                return $ giHash info
