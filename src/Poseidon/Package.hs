{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PoseidonPackage(..),
    ContributorSpec(..),
    PoseidonException(..),
    readPoseidonPackage,
    findPoseidonPackages,
    filterDuplicatePackages,
    loadPoseidonPackages,
    loadPoseidonPackagesIgnoreChecksums,
    maybeLoadJannoFiles,
    maybeLoadBibTeXFiles,
    getJointGenotypeData,
    getIndividuals,
    newPackageTemplate,
    updateChecksumsInPackage
) where

import           Poseidon.BibFile           (loadBibTeXFile)
import           Poseidon.Checksums         (ChecksumListSpec (..), makeChecksumList, renderCheckSumComparison)
import           Poseidon.GenotypeData      (GenotypeDataSpec(..), loadIndividuals, loadJointGenotypeData)
import           Poseidon.Janno             (PoseidonSample (..), loadJannoFile)
import           Poseidon.Utils             (PoseidonException (..))

import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, forM_)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.:?), (.=))
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
import           Data.List                  (groupBy, nub, sortOn)
import           Data.Maybe                 (isNothing, catMaybes, isJust, fromJust)
import           Data.Time                  (Day, UTCTime (..), getCurrentTime)
import           Data.Version               (Version, makeVersion)
import           Data.Yaml                  (decodeEither')
import           GHC.Generics               (Generic)
import           Pipes                      (Producer)
import           Pipes.Safe                 (MonadSafe)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoLine)
import           System.Directory           (doesDirectoryExist,
                                             listDirectory)
import           System.FilePath.Posix      (takeDirectory, takeFileName, (</>))
import           System.IO                  (hPutStrLn, stderr)
import           Text.CSL.Reference         (Reference (..))

-- | A data type to represent a Poseidon Package
data PoseidonPackage = PoseidonPackage
    { posPacPoseidonVersion :: Version
    -- ^ The version of the package
    , posPacTitle           :: String
    -- ^ The title of the package
    , posPacDescription     :: Maybe String
    -- ^ the optional description string of the package
    , posPacContributor     :: [ContributorSpec]
    -- ^ the contributor(s) of the package
    , posPacPackageVersion  :: Maybe Version
    -- ^ the optional version of the package
    , posPacLastModified    :: Maybe Day
    -- ^ the optional date of last update
    , posPacBibFile         :: Maybe FilePath
    -- ^ the optional path to the bibliography file
    , posPacGenotypeData    :: GenotypeDataSpec
    -- ^ the paths to the genotype files
    , posPacJannoFile       :: Maybe FilePath
    -- ^ the path to the janno file
    , posPacChecksumList    :: Maybe ChecksumListSpec
    }
    deriving (Show, Eq, Generic)

-- | The FromJSON instance for the PoseidonPackage data type. Necessary to facilitate automatic reading from JSON files
instance FromJSON PoseidonPackage where
    parseJSON = withObject "PoseidonPackage" $ \v -> PoseidonPackage
        <$> v .:   "poseidonVersion"
        <*> v .:   "title"
        <*> v .:?  "description"
        <*> v .:   "contributor"
        <*> v .:?  "packageVersion"
        <*> v .:?  "lastModified"
        <*> v .:?  "bibFile"
        <*> v .:   "genotypeData"
        <*> v .:   "jannoFile"
        <*> v .:?  "checksums"

instance ToJSON PoseidonPackage where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "poseidonVersion" .= posPacPoseidonVersion x,
        "title" .= posPacTitle x,
        "description" .= posPacDescription x,
        "contributor" .= posPacContributor x,
        "packageVersion" .= posPacPackageVersion x,
        "lastModified" .= posPacLastModified x,
        "bibFile" .= posPacBibFile x,
        "genotypeData" .= posPacGenotypeData x,
        "jannoFile" .= posPacJannoFile x,
        "checksums" .= posPacChecksumList x
        ]

-- | A data type to represent a contributor
data ContributorSpec = ContributorSpec
    { contributorName  :: String -- ^ the name of a contributor
    -- ^ the email address of a contributor
    , contributorEmail :: String -- ^ the email address of a contributor
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of ContributorSpec from JSON files
instance FromJSON ContributorSpec where
    parseJSON = withObject "contributor" $ \v -> ContributorSpec
        <$> v .: "name"
        <*> v .: "email"

instance ToJSON ContributorSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "name" .= contributorName x,
        "email" .= contributorEmail x
        ]

-- | A helper function to add a base directory path to all file paths in a poseidon package.
-- By using the (</>) operator from System.FilePath.Posix, this automatically ensures that paths are only
-- added if the given paths in the Poseidon package are in fact relative. If they are absolute (which would be bad practice
-- but anyway), the (</>) operator would simply return the second argument, so it wouldn't attach the base path.
addFullPaths :: FilePath -- ^ the base file path to use as prefix for relative paths in the package
             -> PoseidonPackage -- ^ the original package
             -> PoseidonPackage -- ^ the new package with prefixed paths
addFullPaths baseDir pac =
    let bibFileFullPath                      = (baseDir </>) <$> posPacBibFile pac
        jannoFileFullPath                    = (baseDir </>) <$> posPacJannoFile pac
        GenotypeDataSpec format_ geno snp ind = posPacGenotypeData pac
        genotypeDataFullPath                 =
            GenotypeDataSpec format_ (baseDir </> geno) (baseDir </> snp) (baseDir </> ind)
    in  pac {
            posPacBibFile      = bibFileFullPath,
            posPacJannoFile    = jannoFileFullPath,
            posPacGenotypeData = genotypeDataFullPath
        }

-- | A function to read in a poseidon package from a YAML file. Note that this function calls the addFullPaths function to
-- make paths absolute.
readPoseidonPackage :: FilePath -- ^ the file path to the yaml file
                    -> IO PoseidonPackage -- ^ the returning package returned in the IO monad.
readPoseidonPackage yamlPath = do
    let baseDir = takeDirectory yamlPath
    bs <- B.readFile yamlPath
    fromJSON <- case decodeEither' bs of
        Left err  -> throwIO $ PoseidonYamlParseException yamlPath err
        Right pac -> return pac
    return $ addFullPaths baseDir fromJSON

-- | a helper function to return all poseidon packages, found by recursively searching a directory tree.
-- If a package is encountered that throws a parsing error, it will be skipped and a warning will be issued.
findPoseidonPackages :: FilePath -- ^ the base directory to search from
                     -> IO [PoseidonPackage] -- ^ the returned list of poseidon packages.
findPoseidonPackages baseDir = do
    posFiles <- findAllPOSEIDONymlFiles baseDir
    findPoseidonPackagesFromPosList posFiles

findPoseidonPackagesFromPosList :: [FilePath] -> IO [PoseidonPackage]
findPoseidonPackagesFromPosList posFiles = do
    posPac   <- mapM tryReadPoseidonPackage $ posFiles
    forM_ (lefts posPac) $ (\e -> case e of
        PoseidonYamlParseException fp err ->
            putStrLn ("Can't read package at " ++ fp ++ " due to YAML parsing error: " ++ show err)
        _ -> error "this should never happen")
    return $ rights posPac
  where
    tryReadPoseidonPackage :: FilePath -> IO (Either PoseidonException PoseidonPackage)
    tryReadPoseidonPackage = try . readPoseidonPackage

findAllPOSEIDONymlFiles :: FilePath -> IO [FilePath]
findAllPOSEIDONymlFiles baseDir = do
    entries <- listDirectory baseDir
    let posFiles = map (baseDir </>) $ filter (=="POSEIDON.yml") $ map takeFileName entries
    subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    morePosFiles <- fmap concat . mapM findAllPOSEIDONymlFiles $ subDirs
    return $ posFiles ++ morePosFiles

loadPoseidonPackagesIgnoreChecksums :: [FilePath]
                     -> IO [PoseidonPackage]
loadPoseidonPackagesIgnoreChecksums dirs = do
    allPackages <- concat <$> mapM findPoseidonPackages dirs
    let dupliChecked = filterDuplicatePackages allPackages
    forM_ (lefts dupliChecked) $ \(PoseidonPackageException err) ->
        hPutStrLn stderr err
    return $ rights dupliChecked

-- | a utility function to load all poseidon packages found recursively in multiple base directories.
loadPoseidonPackages :: [FilePath] -- ^ A list of base directories where to search in
                     -> IO [PoseidonPackage] -- ^ A list of returned poseidon packages.
loadPoseidonPackages dirs = do
    allPackages <- concat <$> mapM findPoseidonPackages dirs
    let dupliChecked = filterDuplicatePackages allPackages
    forM_ (lefts dupliChecked) $ \(PoseidonPackageException err) ->
        hPutStrLn stderr err
    checksumChecked <- filterPackagesWithWrongChecksums $ rights dupliChecked
    forM_ (lefts checksumChecked) $ \(PoseidonPackageException err) ->
        hPutStrLn stderr err
    return $ rights checksumChecked

-- | A helper function to detect packages with duplicate names and select the most up-to-date ones.
filterDuplicatePackages :: [PoseidonPackage] -- ^ a list of Poseidon packages with potential duplicates.
                        -> [Either PoseidonException PoseidonPackage] -- ^ a cleaned up list with duplicates removed. If there are ambiguities about which package to remove, for example because last Update fields are missing or ambiguous themselves, then a Left value with an exception is returned. If successful, a Right value with the clean up list is returned.
filterDuplicatePackages = map checkDuplicatePackages . groupBy titleEq . sortOn posPacTitle
  where
    titleEq :: PoseidonPackage -> PoseidonPackage -> Bool
    titleEq = (\p1 p2 -> posPacTitle p1 == posPacTitle p2)
    checkDuplicatePackages :: [PoseidonPackage] -> Either PoseidonException PoseidonPackage
    checkDuplicatePackages pacs =
        if length pacs == 1
        then return (head pacs)
        else
            let maybeVersions = map posPacPackageVersion pacs
            in  if (length . nub . catMaybes) maybeVersions == length maybeVersions -- all dates need to be given and be unique
                then
                    return . last . sortOn posPacPackageVersion $ pacs
                else
                    let t   = posPacTitle (head pacs)
                        msg = "duplicate package with missing packageVersion field: " ++ t
                    in  Left $ PoseidonPackageException msg

filterPackagesWithWrongChecksums :: [PoseidonPackage] -> IO [Either PoseidonException PoseidonPackage]
filterPackagesWithWrongChecksums pacs = do mapM checkPackageChecksums pacs
  where
    checkPackageChecksums :: PoseidonPackage -> IO (Either PoseidonException PoseidonPackage)
    checkPackageChecksums pac = do
        let encodedChecksums = posPacChecksumList pac
        actualChecksums <- makeChecksumListForPackage pac
        if isNothing encodedChecksums || encodedChecksums == actualChecksums
        then return $ Right pac
        else return $ Left $ PoseidonPackageException $ posPacTitle pac ++
          ": Checksums do not match (left: POSEIDON.yml, right: actual checksum)\n" ++
          if isJust encodedChecksums && isJust actualChecksums
          then renderCheckSumComparison (fromJust encodedChecksums) (fromJust actualChecksums)
          else ""

makeChecksumListForPackage :: PoseidonPackage -> IO (Maybe ChecksumListSpec)
makeChecksumListForPackage pac = do
    let genoFile' = genoFile $ posPacGenotypeData pac
    let snpFiles' = snpFile $ posPacGenotypeData pac
    let indFiles' = indFile $ posPacGenotypeData pac
    let jannoFile = posPacJannoFile pac
    let bibFile   = posPacBibFile pac
    makeChecksumList (Just genoFile') (Just snpFiles') (Just indFiles') jannoFile bibFile

-- | A function to return a list of all individuals in the genotype files of a package.
getIndividuals :: PoseidonPackage -- ^ the Poseidon package
               -> IO [EigenstratIndEntry] -- ^ the returned list of EigenstratIndEntries.
getIndividuals = loadIndividuals . posPacGenotypeData

-- | A function to read genotype data jointly from multiple packages
getJointGenotypeData :: (MonadSafe m) => [PoseidonPackage] -- ^ A list of poseidon packages.
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
getJointGenotypeData = loadJointGenotypeData . map posPacGenotypeData

-- | A function to create a dummy POSEIDON.yml file
newPackageTemplate :: String -> GenotypeDataSpec -> FilePath -> FilePath -> IO PoseidonPackage
newPackageTemplate n (GenotypeDataSpec format geno snp ind) janno bib = do
    (UTCTime today _) <- getCurrentTime
    checksums <- makeChecksumList (Just geno) (Just snp) (Just ind) Nothing Nothing
    return PoseidonPackage {
        posPacPoseidonVersion = makeVersion [2, 0, 1],
        posPacTitle = n,
        posPacDescription = Just "Empty package template. Please add a description",
        posPacContributor = [ContributorSpec "John Doe" "john@doe.net"],
        posPacPackageVersion = Just $ makeVersion [0, 1, 0],
        posPacLastModified = Just today,
        posPacBibFile = Just bib,
        posPacGenotypeData = GenotypeDataSpec format geno snp ind,
        posPacJannoFile = Just janno,
        posPacChecksumList = checksums
    }

updateChecksumsInPackage :: PoseidonPackage -> IO PoseidonPackage
updateChecksumsInPackage pac = do
    newChecksumList <- makeChecksumListForPackage pac
    return PoseidonPackage {
        posPacPoseidonVersion = posPacPoseidonVersion pac,
        posPacTitle = posPacTitle pac,
        posPacDescription = posPacDescription pac,
        posPacContributor = posPacContributor pac,
        posPacPackageVersion = posPacPackageVersion pac,
        posPacLastModified = posPacLastModified pac,
        posPacBibFile = posPacBibFile pac,
        posPacGenotypeData = posPacGenotypeData pac,
        posPacJannoFile = posPacJannoFile pac,
        posPacChecksumList = newChecksumList
    } 

-- Janno file loading

maybeLoadJannoFiles :: [PoseidonPackage] -> IO [Either PoseidonException [Either PoseidonException PoseidonSample]]
maybeLoadJannoFiles = mapM (try . maybeLoadJannoFile)

maybeLoadJannoFile :: PoseidonPackage -> IO [Either PoseidonException PoseidonSample]
maybeLoadJannoFile pac = case posPacJannoFile pac of
    Nothing ->
        throwIO $ PoseidonFileExistenceException
            (posPacTitle pac ++ ": Can't find .janno file path in the POSEIDON.yml")
    Just x  -> loadJannoFile x

-- BibFile file loading

maybeLoadBibTeXFiles :: [PoseidonPackage] -> IO [Either PoseidonException [Reference]]
maybeLoadBibTeXFiles = mapM (try . maybeLoadBibTeXFile)

maybeLoadBibTeXFile :: PoseidonPackage -> IO [Reference]
maybeLoadBibTeXFile pac = case posPacBibFile pac of
    Nothing -> do
        throwIO $ PoseidonFileExistenceException
            (posPacTitle pac ++ ": Can't find .bib file path in the POSEIDON.yml")
    Just x  -> loadBibTeXFile x

