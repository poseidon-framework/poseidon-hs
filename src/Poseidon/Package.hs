{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PoseidonPackage(..),
    ContributorSpec(..),
    PoseidonException(..),
    filterDuplicatePackages,
    readPoseidonPackageCollection,
    poseidonJannoFile,
    poseidonBibFile,
    getJointGenotypeData,
    getIndividuals,
    newPackageTemplate,
    updateChecksumsInPackage,
    writePoseidonPackage,
    getChecksum
) where

import           Poseidon.BibFile           (loadBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec(..), loadIndividuals, loadJointGenotypeData)
import           Poseidon.Janno             (PoseidonSample (..), loadJannoFile)
import           Poseidon.Utils             (PoseidonException (..), renderPoseidonException)

import           Control.Applicative        (Alternative((<|>)), ZipList (..))
import           Control.Exception          (throwIO, try)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad              (when, filterM, forM_, forM)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.:?), (.=))
import           Data.Bifunctor             (Bifunctor(second))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.Digest.Pure.MD5       (md5)
import           Data.Either                (lefts, rights, isRight, Either(..))
import           Data.List                  (intercalate, groupBy, nub, sortOn)
import           Data.Maybe                 (isNothing, catMaybes, isJust, fromJust)
import           Data.Time                  (Day, UTCTime (..), getCurrentTime)
import           Data.Version               (Version, makeVersion)
import           Data.Yaml                  (decodeEither')
import           Data.Yaml.Pretty.Extras    (ToPrettyYaml (..), encodeFilePretty)
import           GHC.Generics               (Generic)
import           Pipes                      (Producer)
import           Pipes.Safe                 (MonadSafe)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoLine)
import           System.Directory           (doesDirectoryExist,
                                             listDirectory,
                                             doesFileExist)
import           System.FilePath.Posix      (takeDirectory, takeFileName, (</>), joinPath, 
                                             splitDirectories, dropDrive, makeRelative)
import           System.IO                  (hPutStrLn, stderr)
import           Text.CSL.Reference         (Reference (..))

{-   ######################### POSEIDONYAMLSTRUCT: Internal structure for YAML loading only ######################### -}
data PoseidonYamlStruct = PoseidonYamlStruct
    { _posYamlPoseidonVersion :: Version
    , _posYamlTitle           :: String
    , _posYamlDescription     :: Maybe String
    , _posYamlContributor     :: [ContributorSpec]
    , _posYamlPackageVersion  :: Maybe Version
    , _posYamlLastModified    :: Maybe Day
    , _posYamlBibFile         :: Maybe FilePath
    , _posYamlBibFileChkSum   :: Maybe String
    , _posYamlGenotypeData    :: GenotypeDataSpec
    , _posYamlJannoFile       :: Maybe FilePath
    , _posYamlJannoFileChkSum :: Maybe String
}

instance FromJSON PoseidonYamlStruct where
    parseJSON = withObject "PoseidonYamlStruct" $ \v -> PoseidonYamlStruct
        <$> v .:   "poseidonVersion"
        <*> v .:   "title"
        <*> v .:?  "description"
        <*> v .:   "contributor"
        <*> v .:?  "packageVersion"
        <*> v .:?  "lastModified"
        <*> v .:?  "bibFile"
        <*> v .:?  "bibFileChkSum"
        <*> v .:   "genotypeData"
        <*> v .:?  "jannoFile"
        <*> v .:?  "jannoFileChkSum"

instance ToJSON PoseidonYamlStruct where
    toJSON x = object [
        "poseidonVersion" .= _posYamlPoseidonVersion x,
        "title"           .= _posYamlTitle x,
        "description"     .= _posYamlDescription x,
        "contributor"     .= _posYamlContributor x,
        "packageVersion"  .= _posYamlPackageVersion x,
        "lastModified"    .= _posYamlLastModified x,
        "bibFile"         .= _posYamlBibFile x,
        "bibFileChkSum"   .= _posYamlBibFileChkSum x,
        "genotypeData"    .= _posYamlGenotypeData x,
        "jannoFile"       .= _posYamlJannoFile x,
        "jannoFileChkSum" .= _posYamlJannoFileChkSum x
        ]

instance ToPrettyYaml PoseidonYamlStruct where
    fieldOrder = const [
        "poseidonVersion",
        "title",
        "description",
        "contributor",
        "name",
        "email",
        "packageVersion",
        "lastModified",
        "genotypeData",
        "format",
        "genoFile",
        "genoFileChkSum",
        "snpFile",
        "snpFileChkSum",
        "indFile",
        "indFileChkSum",
        "jannoFile",
        "jannoFileChkSum",
        "bibFile",
        "bibFileChkSum"
        ]


{- ######################### MAIN PUBLIC POSEIDON-PACKAGE DATA STRUCTURE #########################-}

-- | A data type to represent a Poseidon Package
data PoseidonPackage = PoseidonPackage
    { posPacBaseDir         :: FilePath
    -- ^ The base directory of the YAML file. 
    , posPacPoseidonVersion :: Version
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
    , posPacBibFileChkSum   :: Maybe String
    -- ^ the optional bibfile chksum
    , posPacGenotypeData    :: GenotypeDataSpec
    -- ^ the paths to the genotype files
    , posPacJannoFile       :: Maybe FilePath
    -- ^ the path to the janno file
    , posPacJannoFileChkSum :: Maybe String
    -- ^ the optional jannofile checksum
    }
    deriving (Show, Eq, Generic)

{- These are computed properties -}
-- TODO -> Make sure these functions are used in all clients using Janno and Bib files
poseidonJannoFile :: PoseidonPackage -> Maybe FilePath
poseidonJannoFile pac = (posPacBaseDir pac </>) <$> posPacJannoFile pac

poseidonBibFile :: PoseidonPackage -> Maybe FilePath
poseidonBibFile pac = (posPacBaseDir pac </>) <$> posPacBibFile pac

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


-- | A function to read in a poseidon package from a YAML file. Note that this function calls the addFullPaths function to
-- make paths absolute.
readPoseidonPackage :: Bool -- ^ whether to ignore missing genotype files, useful for developer use cases
                    -> FilePath -- ^ the file path to the yaml file
                    -> IO PoseidonPackage -- ^ the returning package returned in the IO monad.
readPoseidonPackage ignoreGenotypeFilesMissing yamlPath = do
    let baseDir = takeDirectory yamlPath
    bs <- B.readFile yamlPath
    (PoseidonYamlStruct ver tit des con pacVer mod bibF bibFC geno janno jannoC) <- case decodeEither' bs of
        Left err  -> throwIO $ PoseidonYamlParseException yamlPath err
        Right pac -> return pac
    let pac = PoseidonPackage baseDir ver tit des con pacVer mod bibF bibFC geno janno jannoC
    checkFiles ignoreGenotypeFilesMissing pac
    return pac

-- throws exception if any checksum isn't correct
checkFiles :: Bool -> PoseidonPackage -> IO ()
checkFiles ignoreGenotypeFilesMissing pac = do
    -- Check Bib File
    case (poseidonBibFile pac) of
        Nothing -> return ()
        Just fn -> checkFile fn (posPacBibFileChkSum pac)
    -- Check Janno File
    case (poseidonJannoFile pac) of
        Nothing -> return ()
        Just fn -> checkFile fn (posPacJannoFileChkSum pac)
    -- Check Genotype files
    when (not ignoreGenotypeFilesMissing) $ do
        let gd = posPacGenotypeData pac
            d = posPacBaseDir pac
        checkFile (d </> genoFile gd) (genoFileChkSum gd)
        checkFile (d </> snpFile gd) (snpFileChkSum gd)
        checkFile (d </> indFile gd) (indFileChkSum gd)

checkFile :: FilePath -> Maybe String -> IO ()
checkFile fn maybeChkSum = do
    fe <- doesFileExist fn
    if (not fe)
    then throwM (PoseidonFileExistenceException fn)
    else
        case maybeChkSum of
            Nothing -> return ()
            Just chkSum -> do
                fnChkSum <- getChecksum fn
                when (fnChkSum /= chkSum) $ throwM (PoseidonFileChecksumException fn)

getChecksum :: FilePath -> IO String
getChecksum f = do
    fileContent <- LB.readFile f
    let md5Digest = md5 fileContent
    return $ show md5Digest

-- | a utility function to load all poseidon packages found recursively in multiple base directories. 
-- This also takes care of smart filtering and duplication checks. Exceptions lead to skipping packages and outputting
-- warnings
readPoseidonPackageCollection :: Bool -- ^ whether to ignore missing genotype files
                              -> [FilePath] -- ^ A list of base directories where to search in
                              -> IO [PoseidonPackage] -- ^ A list of returned poseidon packages.
readPoseidonPackageCollection ignoreGenotypeFilesMissing dirs = do
    posFiles <- concat <$> mapM findAllPoseidonYmlFiles dirs
    eitherPackages <- mapM tryDecodePoseidonPackage posFiles
    -- notifying the users of package problems
    when (not . null . lefts $ eitherPackages) $ do
        hPutStrLn stderr "Some packages were skipped due to issues:"
        forM_ (lefts eitherPackages) $ \e -> hPutStrLn stderr (renderPoseidonException e)
    let loadedPackages = rights eitherPackages
    -- duplication check. This will throw if packages come with same versions and titles (see filterDuplicates)
    finalPackageList <- filterDuplicatePackages loadedPackages
    -- report number of valid packages
    hPutStrLn stderr $ (show . length $ finalPackageList) ++ " Poseidon packages loaded"
    -- return package list
    return finalPackageList
  where
    tryDecodePoseidonPackage :: FilePath -> IO (Either PoseidonException PoseidonPackage)
    tryDecodePoseidonPackage = try . (readPoseidonPackage ignoreGenotypeFilesMissing)

findAllPoseidonYmlFiles :: FilePath -> IO [FilePath]
findAllPoseidonYmlFiles baseDir = do
    entries <- listDirectory baseDir
    let posFiles = map (baseDir </>) $ filter (=="POSEIDON.yml") $ map takeFileName entries
    subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    morePosFiles <- fmap concat . mapM findAllPoseidonYmlFiles $ subDirs
    return $ posFiles ++ morePosFiles

-- | A helper function to detect packages with duplicate names and select the most up-to-date ones.
filterDuplicatePackages :: (MonadThrow m) => [PoseidonPackage] -- ^ a list of Poseidon packages with potential duplicates.
                        -> m [PoseidonPackage] -- ^ a cleaned up list with duplicates removed. If there are ambiguities about which package to remove, for example because last Update fields are missing or ambiguous themselves, then a Left value with an exception is returned. If successful, a Right value with the clean up list is returned.
filterDuplicatePackages pacs = mapM checkDuplicatePackages $ groupBy titleEq $ sortOn posPacTitle pacs
  where
    titleEq :: PoseidonPackage -> PoseidonPackage -> Bool
    titleEq = (\p1 p2 -> posPacTitle p1 == posPacTitle p2)
    checkDuplicatePackages :: (MonadThrow m) => [PoseidonPackage] -> m PoseidonPackage
    checkDuplicatePackages [pac] = return pac
    checkDuplicatePackages pacs =
        let maybeVersions = map posPacPackageVersion pacs
        in  if (length . nub . catMaybes) maybeVersions == length maybeVersions -- all versions need to be given and be unique
            then
                return . last . sortOn posPacPackageVersion $ pacs
            else
                let t   = posPacTitle $ head pacs
                    msg = "duplicate package with missing packageVersion field: " ++ t
                in  throwM $ PoseidonPackageException msg

 
-- | A function to return a list of all individuals in the genotype files of a package.
getIndividuals :: PoseidonPackage -- ^ the Poseidon package
               -> IO [EigenstratIndEntry] -- ^ the returned list of EigenstratIndEntries.
getIndividuals pac = loadIndividuals (posPacBaseDir pac) (posPacGenotypeData pac)

-- | A function to read genotype data jointly from multiple packages
getJointGenotypeData :: (MonadSafe m) => Bool -- ^ whether to show all warnings
                     -> [PoseidonPackage] -- ^ A list of poseidon packages.
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
getJointGenotypeData showAllWarnings pacs =
    loadJointGenotypeData showAllWarnings [(posPacBaseDir pac, posPacGenotypeData pac) | pac <- pacs]

-- | A function to create a dummy POSEIDON.yml file
-- This will take only the filenames of the provided files, so it assumes that the files will be copied into 
-- the directory into which the YAML file will be written
newPackageTemplate :: FilePath -> String -> GenotypeDataSpec -> FilePath -> FilePath -> IO PoseidonPackage
newPackageTemplate baseDir n (GenotypeDataSpec format geno _ snp _ ind _) janno bib = do
    (UTCTime today _) <- getCurrentTime
    return PoseidonPackage {
        posPacBaseDir = baseDir,
        posPacPoseidonVersion = makeVersion [2, 0, 1],
        posPacTitle = n,
        posPacDescription = Just "Empty package template. Please add a description",
        posPacContributor = [ContributorSpec "John Doe" "john@doe.net"],
        posPacPackageVersion = Just $ makeVersion [0, 1, 0],
        posPacLastModified = Just today,
        posPacBibFile = Just (takeFileName bib),
        posPacBibFileChkSum = Nothing,
        posPacGenotypeData = GenotypeDataSpec format (takeFileName geno) Nothing (takeFileName snp) Nothing (takeFileName ind) Nothing,
        posPacJannoFile = Just (takeFileName janno),
        posPacJannoFileChkSum = Nothing
    }

updateChecksumsInPackage :: PoseidonPackage -> IO PoseidonPackage
updateChecksumsInPackage pac = do
    jannoChkSum <- case poseidonJannoFile pac of
        Nothing -> return Nothing
        Just fn -> Just <$> getChecksum fn
    bibChkSum <- case poseidonBibFile pac of
        Nothing -> return Nothing
        Just fn -> Just <$> getChecksum fn
    let gd = posPacGenotypeData pac
        d = posPacBaseDir pac
    genoChkSum <- Just <$> getChecksum (d </> genoFile gd)
    snpChkSum <- Just <$> getChecksum (d </> snpFile gd)
    indChkSum <- Just <$> getChecksum (d </> indFile gd)
    return $ pac {
        posPacBibFileChkSum = bibChkSum,
        posPacJannoFileChkSum = jannoChkSum,
        posPacGenotypeData = gd {
            genoFileChkSum = genoChkSum,
            snpFileChkSum = snpChkSum,
            indFileChkSum = indChkSum
        }
    } 

writePoseidonPackage :: PoseidonPackage -> IO ()
writePoseidonPackage (PoseidonPackage baseDir ver tit des con pacVer mod bibF bibFC geno janno jannoC) = do
    let yamlPac = PoseidonYamlStruct ver tit des con pacVer mod bibF bibFC geno janno jannoC
        outF = baseDir </> "POSEIDON.yml"
    encodeFilePretty outF yamlPac