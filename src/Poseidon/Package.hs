{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PackageInfo (..),
    PoseidonYamlStruct (..),
    PoseidonPackage(..),
    ContributorSpec(..),
    PoseidonException(..),
    filterDuplicatePackages,
    findAllPoseidonYmlFiles,
    readPoseidonPackageCollection,
    getChecksum,
    getJointGenotypeData,
    getIndividuals,
    newPackageTemplate,
    renderMismatch,
    zipWithPadding,
    updateChecksumsInPackage,
    writePoseidonPackage
) where

import           Poseidon.BibFile           (readBibTeXFile, BibTeX (..))
import           Poseidon.GenotypeData      (GenotypeDataSpec(..), loadIndividuals, loadJointGenotypeData)
import           Poseidon.Janno             (JannoRow(..), readJannoFile, createMinimalJanno)
import           Poseidon.Utils             (PoseidonException (..), renderPoseidonException)

import           Control.Applicative        (Alternative((<|>)), ZipList (..))
import           Control.Exception          (throwIO, try, throw)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad              (when, filterM, forM_, forM, unless)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.:?), (.=))
import           Data.Bifunctor             (Bifunctor(second))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.Digest.Pure.MD5       (md5)
import           Data.Either                (lefts, rights, isRight, Either(..))
import           Data.List                  (intercalate, groupBy, nub, sortOn, (\\))
import           Data.Maybe                 (mapMaybe, fromMaybe, isNothing, catMaybes, isJust, fromJust)
import           Data.Text                  (unpack)
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
import           System.Console.ANSI        (hClearLine, hSetCursorColumn)
import           System.Directory           (doesDirectoryExist,
                                             listDirectory,
                                             doesFileExist)
import           System.FilePath.Posix      (takeDirectory, takeFileName, (</>), joinPath, 
                                             splitDirectories, dropDrive, makeRelative)
import           System.IO                  (hPrint, hPutStr, hPutStrLn, stderr, hFlush)
import           Text.CSL.Reference         (Reference (..), refId, unLiteral)

{-   ######################### PACKAGEINFO: Minimal package representation on Poseidon servers ######################### -}
data PackageInfo = PackageInfo
    { pTitle        :: String
    , pVersion      :: Maybe Version
    , pDescription  :: Maybe String
    , pLastModified :: Maybe Day
    } deriving (Show)

instance ToJSON PackageInfo where
    toJSON x = object [
        "title"        .= pTitle x,
        "version"      .= pVersion x,
        "description"  .= pDescription x,
        "lastModified" .= pLastModified x
        ]

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
        <$> v .:   "title"
        <*> v .:   "version"
        <*> v .:?  "description"
        <*> v .:   "lastModified"

{-   ######################### POSEIDONYAMLSTRUCT: Internal structure for YAML loading only ######################### -}
data PoseidonYamlStruct = PoseidonYamlStruct
    { _posYamlPoseidonVersion :: Version
    , _posYamlTitle           :: String
    , _posYamlDescription     :: Maybe String
    , _posYamlContributor     :: [ContributorSpec]
    , _posYamlPackageVersion  :: Maybe Version
    , _posYamlLastModified    :: Maybe Day
    , _posYamlGenotypeData    :: GenotypeDataSpec
    , _posYamlJannoFile       :: Maybe FilePath
    , _posYamlJannoFileChkSum :: Maybe String
    , _posYamlBibFile         :: Maybe FilePath
    , _posYamlBibFileChkSum   :: Maybe String
} deriving (Show, Eq, Generic)

poseidonJannoFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonJannoFilePath baseDir yml = (baseDir </>) <$> _posYamlJannoFile yml

poseidonBibFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonBibFilePath baseDir yml = (baseDir </>) <$> _posYamlBibFile yml


instance FromJSON PoseidonYamlStruct where
    parseJSON = withObject "PoseidonYamlStruct" $ \v -> PoseidonYamlStruct
        <$> v .:   "poseidonVersion"
        <*> v .:   "title"
        <*> v .:?  "description"
        <*> v .:   "contributor"
        <*> v .:?  "packageVersion"
        <*> v .:?  "lastModified"
        <*> v .:   "genotypeData"
        <*> v .:?  "jannoFile"
        <*> v .:?  "jannoFileChkSum"
        <*> v .:?  "bibFile"
        <*> v .:?  "bibFileChkSum"

instance ToJSON PoseidonYamlStruct where
    toJSON x = object [
        "poseidonVersion" .= _posYamlPoseidonVersion x,
        "title"           .= _posYamlTitle x,
        "description"     .= _posYamlDescription x,
        "contributor"     .= _posYamlContributor x,
        "packageVersion"  .= _posYamlPackageVersion x,
        "lastModified"    .= _posYamlLastModified x,
        "genotypeData"    .= _posYamlGenotypeData x,
        "jannoFile"       .= _posYamlJannoFile x,
        "jannoFileChkSum" .= _posYamlJannoFileChkSum x,
        "bibFile"         .= _posYamlBibFile x,
        "bibFileChkSum"   .= _posYamlBibFileChkSum x
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
    -- ^ the base directory of the YAML file
    , posPacPoseidonVersion :: Version
    -- ^ the version of the package
    , posPacTitle           :: String
    -- ^ the title of the package
    , posPacDescription     :: Maybe String
    -- ^ the optional description string of the package
    , posPacContributor     :: [ContributorSpec]
    -- ^ the contributor(s) of the package
    , posPacPackageVersion  :: Maybe Version
    -- ^ the optional version of the package
    , posPacLastModified    :: Maybe Day
    -- ^ the optional date of last update
    , posPacGenotypeData    :: GenotypeDataSpec
    -- ^ the paths to the genotype files
    , posPacJannoFile       :: Maybe FilePath
    -- ^ the path to the janno file
    , posPacJanno           :: [JannoRow]
    -- ^ the loaded janno file
    , posPacJannoFileChkSum :: Maybe String
    -- ^ the optional jannofile checksum
    , posPacBibFile         :: Maybe FilePath
    -- ^ the path to the BibTeX file
    , posPacBib             :: BibTeX
    -- ^ the loaded bibliography file
    , posPacBibFileChkSum   :: Maybe String
    -- ^ the optional bibfile chksum
    , posPacDuplicate       :: Int
    -- ^ how many packages of this name exist in the current collection
    }
    deriving (Show, Eq, Generic)

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
readPoseidonPackage :: Bool -- ^ whether to ignore all checksums
                    -> Bool -- ^ whether to ignore missing genotype files, useful for developer use cases
                    -> FilePath -- ^ the file path to the yaml file
                    -> IO PoseidonPackage -- ^ the returning package returned in the IO monad.
readPoseidonPackage ignoreChecksums ignoreGenotypeFilesMissing ymlPath = do
    let baseDir = takeDirectory ymlPath
    bs <- B.readFile ymlPath
    -- read yml files
    (PoseidonYamlStruct ver tit des con pacVer mod geno jannoF jannoC bibF bibC) <- case decodeEither' bs of
        Left err  -> throwIO $ PoseidonYamlParseException ymlPath err
        Right pac -> return pac
    let yml = PoseidonYamlStruct ver tit des con pacVer mod geno jannoF jannoC bibF bibC
    -- file existence and checksum test
    checkFiles baseDir ignoreChecksums ignoreGenotypeFilesMissing yml
    -- read janno (or fill with empty dummy object)
    indEntries <- loadIndividuals baseDir geno
    janno <- case poseidonJannoFilePath baseDir yml of
        Nothing -> do
            return $ createMinimalJanno indEntries
        Just p -> do
            loadedJanno <- readJannoFile p
            checkJannoIndConsistency tit loadedJanno indEntries
            return loadedJanno
    -- read bib (or fill with empty list)
    bib <- case poseidonBibFilePath baseDir yml of
        Nothing -> return ([] :: BibTeX)
        Just p -> do 
            loadedBib <- readBibTeXFile p
            checkJannoBibConsistency tit janno loadedBib
            return loadedBib
    -- create PoseidonPackage
    let pac = PoseidonPackage baseDir ver tit des con pacVer mod geno jannoF janno jannoC bibF bib bibC 1
    return pac

-- throws exception if any checksum isn't correct
checkFiles :: FilePath -> Bool -> Bool -> PoseidonYamlStruct -> IO ()
checkFiles baseDir ignoreChecksums ignoreGenotypeFilesMissing yml = do
    -- Check Bib File
    case poseidonJannoFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> if ignoreChecksums
                   then checkFile fn Nothing
                   else checkFile fn $ _posYamlJannoFileChkSum yml
    -- Check Janno File
    case poseidonBibFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> if ignoreChecksums
                   then checkFile fn Nothing
                   else checkFile fn $ _posYamlBibFileChkSum yml
    -- Check Genotype files
    unless ignoreGenotypeFilesMissing $ do
        let gd = _posYamlGenotypeData yml
            d = baseDir
        if ignoreChecksums
        then do
            checkFile (d </> genoFile gd) Nothing
            checkFile (d </> snpFile gd) Nothing
            checkFile (d </> indFile gd) Nothing
        else do
            checkFile (d </> genoFile gd) $ genoFileChkSum gd
            checkFile (d </> snpFile gd) $ snpFileChkSum gd
            checkFile (d </> indFile gd) $ indFileChkSum gd

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

checkJannoIndConsistency :: String -> [JannoRow] -> [EigenstratIndEntry] -> IO ()
checkJannoIndConsistency pacName janno indEntries = do
    let genoIDs         = [ x | EigenstratIndEntry  x _ _ <- indEntries]
        genoSexs        = [ x | EigenstratIndEntry  _ x _ <- indEntries]
        genoGroups      = [ x | EigenstratIndEntry  _ _ x <- indEntries]
    let jannoIDs        = map jIndividualID janno
        jannoSexs       = map jGeneticSex janno
        jannoGroups     = map (head . jGroupName) janno
    let idMis           = genoIDs /= jannoIDs
        sexMis          = genoSexs /= jannoSexs
        groupMis        = genoGroups /= jannoGroups
        anyJannoGenoMis = idMis || sexMis || groupMis
    when idMis $ throwM $ PoseidonCrossFileConsistencyException pacName $
        "Individual ID mismatch between genotype data (left) and .janno files (right): " ++
        renderMismatch genoIDs jannoIDs
    when sexMis $ throwM $ PoseidonCrossFileConsistencyException pacName $
        "Individual Sex mismatch between genotype data (left) and .janno files (right): " ++
        renderMismatch (map show genoSexs) (map show jannoSexs)
    when groupMis $ throwM $ PoseidonCrossFileConsistencyException pacName $
        "Individual GroupID mismatch between genotype data (left) and .janno files (right): " ++
        renderMismatch genoGroups jannoGroups

renderMismatch :: [String] -> [String] -> String
renderMismatch a b =
    let misMatchList = map (\ (x, y) -> "(" ++ x ++ " = " ++ y ++ ")")
                       (filter (\ (x, y) -> x /= y) $ zipWithPadding "?" "?" a b)
    in if length misMatchList > 5
       then intercalate ", " (take 5 misMatchList) ++ ", ..."
       else intercalate ", " misMatchList

zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
zipWithPadding a _ []     ys     = zip (repeat a) ys
zipWithPadding _ b xs     []     = zip xs (repeat b)

checkJannoBibConsistency :: String -> [JannoRow] -> BibTeX -> IO ()
checkJannoBibConsistency pacName janno bibtex = do
    -- Cross-file consistency
    let literatureInJanno = nub $ mapMaybe jPublication janno
        literatureInBib = nub $ map (unpack . unLiteral . refId) bibtex
        literatureNotInBibButInJanno = literatureInJanno \\ literatureInBib
    unless (null literatureNotInBibButInJanno) $ throwM $ PoseidonCrossFileConsistencyException pacName $ 
        "The following papers lack BibTeX entries: " ++
        intercalate ", " literatureNotInBibButInJanno

-- | a utility function to load all poseidon packages found recursively in multiple base directories. 
-- This also takes care of smart filtering and duplication checks. Exceptions lead to skipping packages and outputting
-- warnings
readPoseidonPackageCollection :: Bool -- ^ whether to ignore all checksums
                              -> Bool -- ^ whether to ignore missing genotype files, useful for developer use cases
                              -> [FilePath] -- ^ A list of base directories where to search in
                              -> IO [PoseidonPackage] -- ^ A list of returned poseidon packages.
readPoseidonPackageCollection ignoreChecksums ignoreGenotypeFilesMissing dirs = do
    hPutStr stderr $ "Searching POSEIDON.yml files... "
    posFiles <- concat <$> mapM findAllPoseidonYmlFiles dirs
    hPutStrLn stderr $ show (length posFiles) ++ " found"
    hPutStr stderr "Initializing packages... "
    eitherPackages <- mapM tryDecodePoseidonPackage $ zip [1..] posFiles
    hPutStrLn stderr ""
    -- notifying the users of package problems
    when (not . null . lefts $ eitherPackages) $ do
        hPutStrLn stderr "Some packages were skipped due to issues:"
        forM_ (lefts eitherPackages) $ \e -> hPutStrLn stderr (renderPoseidonException e)
    let loadedPackages = rights eitherPackages
    -- package duplication check 
    -- This will throw if packages come with same versions and titles (see filterDuplicates)
    finalPackageList <- filterDuplicatePackages loadedPackages
    when (length loadedPackages > length finalPackageList) $ do
        hPutStrLn stderr "Some packages were skipped as duplicates:"
        forM_ (map posPacBaseDir loadedPackages \\ map posPacBaseDir finalPackageList) $
            \x -> hPrint stderr x
    -- individual duplication check
    individuals <- mapM (uncurry loadIndividuals . \x -> (posPacBaseDir x, posPacGenotypeData x)) finalPackageList
    checkIndividualsUnique $ concat individuals
    -- report number of valid packages
    hPutStrLn stderr $ "Packages loaded: " ++ (show . length $ finalPackageList)
    -- return package list
    return finalPackageList
  where
    tryDecodePoseidonPackage :: (Integer, FilePath) -> IO (Either PoseidonException PoseidonPackage)
    tryDecodePoseidonPackage (numberPackage, path) = do
        hClearLine stderr
        hSetCursorColumn stderr 0
        hPutStr stderr $ "Initializing packages... " ++ show numberPackage ++ " initialized"
        hFlush stderr
        try . readPoseidonPackage ignoreChecksums ignoreGenotypeFilesMissing $ path

checkIndividualsUnique :: [EigenstratIndEntry] -> IO ()
checkIndividualsUnique indEntries = do
    let genoIDs = [ x | EigenstratIndEntry  x _ _ <- indEntries]
    when (length genoIDs /= length (nub genoIDs)) $
        throwM $ PoseidonCollectionException $
            "Duplicate individuals (" ++
            intercalate ", " (genoIDs \\ nub genoIDs) ++
            ")"

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
    checkDuplicatePackages dupliPacs =
        let pacs = map (\x -> x { posPacDuplicate = length dupliPacs }) dupliPacs
            maybeVersions = map posPacPackageVersion pacs
        in  if (length . nub . catMaybes) maybeVersions == length maybeVersions -- all versions need to be given and be unique
            then
                return . last . sortOn posPacPackageVersion $ pacs
            else
                let t   = posPacTitle $ head pacs
                    msg = "Multiple packages with the title " ++ t ++ " and all with missing or identical version numbers"
                in  throwM $ PoseidonPackageException msg
 
-- | A function to return a list of all individuals in the genotype files of a package.
getIndividuals :: PoseidonPackage -- ^ the Poseidon package
               -> IO [EigenstratIndEntry] -- ^ the returned list of EigenstratIndEntries.
getIndividuals pac = loadIndividuals (posPacBaseDir pac) (posPacGenotypeData pac)

-- | A function to read genotype data jointly from multiple packages
getJointGenotypeData :: (MonadSafe m) => Bool -- ^ whether to show all warnings
                     -> Bool -- ^ whether to generate an intersection instead of union of input sites
                     -> [PoseidonPackage] -- ^ A list of poseidon packages.
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
getJointGenotypeData showAllWarnings intersect pacs =
    loadJointGenotypeData showAllWarnings intersect [(posPacBaseDir pac, posPacGenotypeData pac) | pac <- pacs]

-- | A function to create a dummy POSEIDON.yml file
-- This will take only the filenames of the provided files, so it assumes that the files will be copied into 
-- the directory into which the YAML file will be written
newPackageTemplate :: FilePath -> String -> GenotypeDataSpec -> Maybe [EigenstratIndEntry] -> Maybe [JannoRow] -> Maybe BibTeX -> IO PoseidonPackage
newPackageTemplate baseDir name (GenotypeDataSpec format geno _ snp _ ind _) inds janno bib = do
    (UTCTime today _) <- getCurrentTime
    return PoseidonPackage {
        posPacBaseDir = baseDir
    ,   posPacPoseidonVersion = makeVersion [2, 1, 0]
    ,   posPacTitle = name
    ,   posPacDescription = Just "Empty package template. Please add a description"
    ,   posPacContributor = [ContributorSpec "John Doe" "john@doe.net"]
    ,   posPacPackageVersion = Just $ makeVersion [0, 1, 0]
    ,   posPacLastModified = Just today
    ,   posPacGenotypeData = GenotypeDataSpec format (takeFileName geno) Nothing (takeFileName snp) Nothing (takeFileName ind) Nothing
    ,   posPacJannoFile = Just $ name ++ ".janno"
    -- TODO: This is not a good solution. Maybe we need pattern matching with 
    -- two different implementations of newPackageTemplate depending on whether
    -- the input janno object is Nothing or not
    ,   posPacJanno =
            case janno of 
                Nothing -> case inds of 
                    Nothing -> throw $ PoseidonNewPackageConstructionException "Missing Individual- and Group IDs. This should never happen"
                    Just a -> createMinimalJanno a
                Just a -> a
    ,   posPacJannoFileChkSum = Nothing
    ,   posPacBibFile = Just $ name ++ ".bib"
    ,   posPacBib =
            case bib of 
                Nothing -> [] :: BibTeX
                Just a -> a
    ,   posPacBibFileChkSum = Nothing
    ,   posPacDuplicate = 1
    }

updateChecksumsInPackage :: PoseidonPackage -> IO PoseidonPackage
updateChecksumsInPackage pac = do
    let d = posPacBaseDir pac
    jannoChkSum <- case posPacJannoFile pac of
        Nothing -> return Nothing
        Just fn -> Just <$> getChecksum (d </> fn)
    bibChkSum <- case posPacBibFile pac of
        Nothing -> return Nothing
        Just fn -> Just <$> getChecksum (d </> fn)
    let gd = posPacGenotypeData pac
    genoExists <- doesFileExist (d </> genoFile gd)
    genoChkSum <- if genoExists
                  then Just <$> getChecksum (d </> genoFile gd)
                  else return Nothing
    snpExists <-  doesFileExist (d </> snpFile gd)
    snpChkSum <-  if snpExists
                  then Just <$> getChecksum (d </> snpFile gd)
                  else return Nothing
    indExists <-  doesFileExist (d </> indFile gd)
    indChkSum <-  if indExists
                  then Just <$> getChecksum (d </> indFile gd)
                  else return Nothing
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
writePoseidonPackage (PoseidonPackage baseDir ver tit des con pacVer mod geno jannoF _ jannoC bibF _ bibFC _) = do
    let yamlPac = PoseidonYamlStruct ver tit des con pacVer mod geno jannoF jannoC bibF bibFC
        outF = baseDir </> "POSEIDON.yml"
    encodeFilePretty outF yamlPac