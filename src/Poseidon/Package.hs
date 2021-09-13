{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PackageInfo (..),
    PoseidonYamlStruct (..),
    PoseidonPackage(..),
    PoseidonException(..),
    PackageReadOptions (..),
    filterDuplicatePackages,
    findAllPoseidonYmlFiles,
    readPoseidonPackageCollection,
    getChecksum,
    getJointGenotypeData,
    getIndividuals,
    newPackageTemplate,
    renderMismatch,
    zipWithPadding,
    writePoseidonPackage,
    defaultPackageReadOptions
) where

import           Poseidon.BibFile           (BibEntry (..), BibTeX,
                                             readBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             loadIndividuals,
                                             loadJointGenotypeData)
import           Poseidon.Janno             (JannoList (..), JannoRow (..),
                                             JannoSex (..), createMinimalJanno,
                                             readJannoFile)
import           Poseidon.PoseidonVersion   (validPoseidonVersions, showPoseidonVersion, latestPoseidonVersion, asVersion)
import           Poseidon.SecondaryTypes    (ContributorSpec (..))
import           Poseidon.Utils             (PoseidonException (..),
                                             renderPoseidonException)

import           Control.Exception          (throw, throwIO, try)
import           Control.Monad              (filterM, forM_, unless, when)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.:?), (.=))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.Digest.Pure.MD5       (md5)
import           Data.Either                (lefts, rights)
import           Data.List                  (groupBy, intercalate, nub, sortOn,
                                             (\\), elemIndex)
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Time                  (Day, UTCTime (..), getCurrentTime)
import           Data.Version               (Version (..), makeVersion)
import           Data.Yaml                  (decodeEither')
import           Data.Yaml.Pretty.Extras    (ToPrettyYaml (..),
                                             encodeFilePretty)
import           GHC.Generics               (Generic)
import           Pipes                      (Producer, runEffect, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (MonadSafe, runSafeT)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..), GenoLine)
import           System.Console.ANSI        (hClearLine, hSetCursorColumn)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             listDirectory)
import           System.FilePath            (takeDirectory, takeFileName, (</>))
import           System.IO                  (hFlush, hPrint, hPutStr, hPutStrLn,
                                             stderr, withFile, IOMode (ReadMode), hGetContents)
import Data.Char (isSpace)

{-   ######################### PACKAGEINFO: Minimal package representation on Poseidon servers ######################### -}
data PackageInfo = PackageInfo
    { pTitle        :: String
    , pVersion      :: Maybe Version
    , pDescription  :: Maybe String
    , pLastModified :: Maybe Day
    }
    deriving (Show)

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
    , _posYamlReadmeFile      :: Maybe String
    , _posYamlChangelogFile   :: Maybe String
    }
    deriving (Show, Eq, Generic)

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
        <*> v .:?  "readmeFile"
        <*> v .:?  "changelogFile"

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
        "bibFileChkSum"   .= _posYamlBibFileChkSum x,
        "readmeFile"      .= _posYamlReadmeFile x,
        "changelogFile"   .= _posYamlChangelogFile x
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
        "snpSet",
        "jannoFile",
        "jannoFileChkSum",
        "bibFile",
        "bibFileChkSum",
        "readmeFile",
        "changelogFile"
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
    , posPacReadmeFile      :: Maybe FilePath
    -- ^ the path to the README file
    , posPacChangelogFile   :: Maybe FilePath
    -- ^ the path to the CHANGELOG file
    , posPacDuplicate       :: Int
    -- ^ how many packages of this name exist in the current collection
    }
    deriving (Show, Eq, Generic)

data PackageReadOptions = PackageReadOptions
    { _readOptVerbose          :: Bool -- whether to print verbose output
    -- whether to stop on duplicated individuals
    , _readOptStopOnDuplicates :: Bool -- whether to stop on duplicated individuals
    -- whether to ignore all checksums
    , _readOptIgnoreChecksums  :: Bool -- whether to ignore all checksums
    -- whether to ignore missing genotype files, useful for developer use cases
    , _readOptIgnoreGeno       :: Bool -- whether to ignore missing genotype files, useful for developer use cases
    -- whether to check the first 100 SNPs of the genotypes
    , _readOptGenoCheck        :: Bool -- whether to check the first 100 SNPs of the genotypes
    }

defaultPackageReadOptions :: PackageReadOptions
defaultPackageReadOptions = PackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

-- | a utility function to load all poseidon packages found recursively in multiple base directories.
-- This also takes care of smart filtering and duplication checks. Exceptions lead to skipping packages and outputting
-- warnings
readPoseidonPackageCollection :: PackageReadOptions
                              -> [FilePath] -- ^ A list of base directories where to search in
                              -> IO [PoseidonPackage] -- ^ A list of returned poseidon packages.
readPoseidonPackageCollection opts dirs = do
    hPutStr stderr "Searching POSEIDON.yml files... "
    posFilesAllVersions <- concat <$> mapM findAllPoseidonYmlFiles dirs
    hPutStrLn stderr $ show (length posFilesAllVersions) ++ " found"
    hPutStrLn stderr "Checking Poseidon versions... "
    posFiles <- filterByPoseidonVersion posFilesAllVersions
    hPutStrLn stderr "Initializing packages... "
    eitherPackages <- mapM (tryDecodePoseidonPackage (_readOptVerbose opts)) $ zip [1..] posFiles
    hPutStrLn stderr ""
    -- notifying the users of package problems
    when (not . null . lefts $ eitherPackages) $ do
        hPutStrLn stderr "Some packages were skipped due to issues:"
        forM_ (zip posFiles eitherPackages) $ \(posF, epac) -> do
            case epac of
                Left e -> do
                    hPutStrLn stderr ("In the package described in " ++ posF ++ ":")
                    hPutStrLn stderr (renderPoseidonException e)
                _ -> return ()
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
    checkIndividualsUnique (_readOptStopOnDuplicates opts) $ concat individuals
    -- report number of valid packages
    hPutStrLn stderr $ "Packages loaded: " ++ (show . length $ finalPackageList)
    -- return package list
    return finalPackageList
  where
    filterByPoseidonVersion :: [FilePath] -> IO [FilePath]
    filterByPoseidonVersion posFiles = do
        eitherPaths <- mapM isInVersionRange posFiles
        mapM_ (hPutStrLn stderr . renderPoseidonException) $ lefts eitherPaths
        return $ rights eitherPaths
        where 
            isInVersionRange :: FilePath -> IO (Either PoseidonException FilePath)
            isInVersionRange posFile = do
                content <- readFile' posFile
                let posLines = lines content
                case elemIndex "poseidonVersion:" (map (take 16) posLines) of
                    Nothing -> return $ Left $ PoseidonPackageVersionException posFile "Unknown"
                    Just n -> do
                        let versionLine = posLines !! n
                            versionString = filter (not . isSpace) $ drop 16 versionLine
                        if versionString `elem` map showPoseidonVersion validPoseidonVersions
                        then return $ Right posFile
                        else return $ Left $ PoseidonPackageVersionException posFile versionString
            readFile' :: FilePath -> IO String
            readFile' filename = withFile filename ReadMode $ \handle -> do
                theContent <- hGetContents handle
                mapM return theContent
    tryDecodePoseidonPackage :: Bool -> (Integer, FilePath) -> IO (Either PoseidonException PoseidonPackage)
    tryDecodePoseidonPackage False (numberPackage, path) = do
        hClearLine stderr
        hSetCursorColumn stderr 0
        hPutStr stderr $ "> " ++ show numberPackage ++ " "
        hFlush stderr
        try . readPoseidonPackage opts $ path
    tryDecodePoseidonPackage True (numberPackage, path) = do
        hPutStrLn stderr $ "> " ++ show numberPackage ++ ": " ++ path
        try . readPoseidonPackage opts $ path

-- | A function to read in a poseidon package from a YAML file. Note that this function calls the addFullPaths function to
-- make paths absolute.
readPoseidonPackage :: PackageReadOptions
                    -> FilePath -- ^ the file path to the yaml file
                    -> IO PoseidonPackage -- ^ the returning package returned in the IO monad.
readPoseidonPackage opts ymlPath = do
    let baseDir = takeDirectory ymlPath
    bs <- B.readFile ymlPath
    -- read yml files
    (PoseidonYamlStruct ver tit des con pacVer mod_ geno jannoF jannoC bibF bibC readF changeF) <- case decodeEither' bs of
        Left err  -> throwIO $ PoseidonYamlParseException ymlPath err
        Right pac -> return pac
    let yml = PoseidonYamlStruct ver tit des con pacVer mod_ geno jannoF jannoC bibF bibC readF changeF
    -- file existence and checksum test
    checkFiles baseDir (_readOptIgnoreChecksums opts) (_readOptIgnoreGeno opts) yml
    -- read janno (or fill with empty dummy object)
    indEntries <- loadIndividuals baseDir geno
    janno <- case poseidonJannoFilePath baseDir yml of
        Nothing -> do
            return $ createMinimalJanno indEntries
        Just p -> do
            loadedJanno <- readJannoFile (_readOptVerbose opts) p
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
    when (not (_readOptIgnoreGeno opts) && (_readOptGenoCheck opts)) . runSafeT $ do
        -- we're using loadJointGenotypeData here on a single package to check for SNP consistency
        -- since that check is only implemented in the jointLoading function, not in the per-package loading
        (_, eigenstratProd) <- loadJointGenotypeData False False [(baseDir, geno)]
        runEffect $ eigenstratProd >-> P.take 100 >-> P.drain
    let pac = PoseidonPackage baseDir ver tit des con pacVer mod_ geno jannoF janno jannoC bibF bib bibC readF changeF 1
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
        jannoSexs       = map (sfSex . jGeneticSex) janno
        jannoGroups     = map (head . getJannoList . jGroupName) janno
    let idMis           = genoIDs /= jannoIDs
        sexMis          = genoSexs /= jannoSexs
        groupMis        = genoGroups /= jannoGroups
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
    let literatureInJanno = nub . concat . map getJannoList . mapMaybe jPublication $ janno
        literatureInBib = nub $ map bibEntryId bibtex
        literatureNotInBibButInJanno = literatureInJanno \\ literatureInBib
    unless (null literatureNotInBibButInJanno) $ throwM $ PoseidonCrossFileConsistencyException pacName $
        "The following papers lack BibTeX entries: " ++
        intercalate ", " literatureNotInBibButInJanno

checkIndividualsUnique :: Bool -> [EigenstratIndEntry] -> IO ()
checkIndividualsUnique stopOnDuplicates indEntries = do
    let genoIDs = [ x | EigenstratIndEntry  x _ _ <- indEntries]
    when (length genoIDs /= length (nub genoIDs)) $ do
        if stopOnDuplicates
        then do
            throwM $ PoseidonCollectionException $
                "Duplicate individuals (" ++
                intercalate ", " (genoIDs \\ nub genoIDs) ++
                ")"
        else do
            hPutStrLn stderr $
                "Warning: Duplicate individuals (" ++
                intercalate ", " (take 3 $ genoIDs \\ nub genoIDs) ++
                if length (genoIDs \\ nub genoIDs) > 3
                then ", ...)"
                else ")"

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
        let pacs_ = map (\x -> x { posPacDuplicate = length dupliPacs }) dupliPacs
            maybeVersions = map posPacPackageVersion pacs_
        in  if (length . nub . catMaybes) maybeVersions == length maybeVersions -- all versions need to be given and be unique
            then
                return . last . sortOn posPacPackageVersion $ pacs_
            else
                let t   = posPacTitle $ head pacs_
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
newPackageTemplate baseDir name (GenotypeDataSpec format_ geno _ snp _ ind _ snpSet_) inds janno bib = do
    (UTCTime today _) <- getCurrentTime
    return PoseidonPackage {
        posPacBaseDir = baseDir
    ,   posPacPoseidonVersion = asVersion latestPoseidonVersion 
    ,   posPacTitle = name
    ,   posPacDescription = Just "Empty package template. Please add a description"
    ,   posPacContributor = [ContributorSpec "John Doe" "john@doe.net"]
    ,   posPacPackageVersion = Just $ makeVersion [0, 1, 0]
    ,   posPacLastModified = Just today
    ,   posPacGenotypeData = GenotypeDataSpec format_ (takeFileName geno) Nothing (takeFileName snp) Nothing (takeFileName ind) Nothing snpSet_
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
                Just a  -> a
    ,   posPacBibFileChkSum = Nothing
    ,   posPacReadmeFile = Nothing
    ,   posPacChangelogFile = Nothing
    ,   posPacDuplicate = 1
    }

writePoseidonPackage :: PoseidonPackage -> IO ()
writePoseidonPackage (PoseidonPackage baseDir ver tit des con pacVer mod_ geno jannoF _ jannoC bibF _ bibFC readF changeF _) = do
    let yamlPac = PoseidonYamlStruct ver tit des con pacVer mod_ geno jannoF jannoC bibF bibFC readF changeF
        outF = baseDir </> "POSEIDON.yml"
    encodeFilePretty outF yamlPac
