{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PoseidonYamlStruct (..),
    PoseidonPackage(..),
    PoseidonException(..),
    PackageReadOptions (..),
    filterDuplicatePackages,
    findAllPoseidonYmlFiles,
    readPoseidonPackageCollection,
    getJointGenotypeData,
    getJointJanno,
    getJointIndividualInfo,
    newMinimalPackageTemplate,
    newPackageTemplate,
    renderMismatch,
    zipWithPadding,
    writePoseidonPackage,
    defaultPackageReadOptions,
    readPoseidonPackage,
    makePseudoPackageFromGenotypeData
) where

import           Poseidon.BibFile           (BibEntry (..), BibTeX,
                                             readBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), joinEntries,
                                             loadGenotypeData, loadIndividuals)
import           Poseidon.Janno             (JannoList (..), JannoRow (..),
                                             JannoSex (..), concatJannos,
                                             createMinimalJanno, readJannoFile)
import           Poseidon.PoseidonVersion   (asVersion, latestPoseidonVersion,
                                             showPoseidonVersion,
                                             validPoseidonVersions)
import           Poseidon.SecondaryTypes    (ContributorSpec (..),
                                             IndividualInfo (..), ORCID (..))
import           Poseidon.Utils             (LogEnv, PoseidonException (..),
                                             PoseidonLogIO, checkFile, logDebug,
                                             logInfo, logWarning, logWithEnv,
                                             renderPoseidonException)

import           Control.Exception          (catch, throwIO)
import           Control.Monad              (filterM, forM_, unless, void, when)
import           Control.Monad.Catch        (MonadThrow, throwM, try)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (ask)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString            as B
import           Data.Char                  (isSpace)
import           Data.Either                (lefts, rights)
import           Data.List                  (elemIndex, groupBy, intercalate,
                                             nub, sortOn, (\\))
import           Data.Maybe                 (catMaybes, isNothing, mapMaybe)
import           Data.Time                  (Day, UTCTime (..), getCurrentTime)
import qualified Data.Vector                as V
import           Data.Version               (Version (..), makeVersion)
import           Data.Yaml                  (decodeEither')
import           Data.Yaml.Pretty.Extras    (ToPrettyYaml (..),
                                             encodeFilePretty)
import           GHC.Generics               (Generic)
import           Pipes                      (Pipe, Producer, cat, for,
                                             runEffect, yield, (>->))
import           Pipes.OrderedZip           (orderCheckPipe, orderedZip,
                                             orderedZipAll)
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (MonadSafe, runSafeT)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine,
                                             readEigenstratSnpFile)
import           SequenceFormats.Plink      (readBimFile)
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            (takeBaseName, takeDirectory,
                                             takeExtension, takeFileName, (</>))
import           System.IO                  (IOMode (ReadMode), hGetContents,
                                             withFile)

-- | Internal structure for YAML loading only
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
    , _posYamlReadmeFile      :: Maybe FilePath
    , _posYamlChangelogFile   :: Maybe FilePath
    }
    deriving (Show, Eq, Generic)

poseidonJannoFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonJannoFilePath baseDir yml = (baseDir </>) <$> _posYamlJannoFile yml
poseidonBibFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonBibFilePath baseDir yml = (baseDir </>) <$> _posYamlBibFile yml
poseidonReadmeFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonReadmeFilePath baseDir yml = (baseDir </>) <$> _posYamlReadmeFile yml
poseidonChangelogFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonChangelogFilePath baseDir yml = (baseDir </>) <$> _posYamlChangelogFile yml

instance FromJSON PoseidonYamlStruct where
    parseJSON = withObject "PoseidonYamlStruct" $ \v -> PoseidonYamlStruct
        <$> v .:   "poseidonVersion"
        <*> v .:   "title"
        <*> v .:?  "description"
        <*> v .:?  "contributor" .!= []
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
    toJSON x = object $ [
        "poseidonVersion" .= _posYamlPoseidonVersion x,
        "title"           .= _posYamlTitle x,
        "description"     .= _posYamlDescription x] ++
        (if not $ null (_posYamlContributor x) then ["contributor" .= _posYamlContributor x] else []) ++
        ["packageVersion"  .= _posYamlPackageVersion x,
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
        "orcid",
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
    { _readOptStopOnDuplicates :: Bool
    -- ^ whether to stop on duplicated individuals
    , _readOptIgnoreChecksums  :: Bool
    -- ^ whether to ignore all checksums
    , _readOptIgnoreGeno       :: Bool
    -- ^ whether to ignore missing genotype files, useful for developer use cases
    , _readOptGenoCheck        :: Bool
    -- ^ whether to check the first 100 SNPs of the genotypes
    , _readOptIgnorePosVersion :: Bool
    -- ^ whether to ignore the Poseidon version of an input package.
    }

defaultPackageReadOptions :: PackageReadOptions
defaultPackageReadOptions = PackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    , _readOptIgnorePosVersion = False
    }

-- | a utility function to load all poseidon packages found recursively in multiple base directories.
-- This also takes care of smart filtering and duplication checks. Exceptions lead to skipping packages and outputting
-- warnings
readPoseidonPackageCollection :: PackageReadOptions
                              -> [FilePath] -- ^ A list of base directories where to search in
                              -> PoseidonLogIO [PoseidonPackage] -- ^ A list of returned poseidon packages.
readPoseidonPackageCollection opts dirs = do
    logInfo "Searching POSEIDON.yml files... "
    posFilesAllVersions <- liftIO $ concat <$> mapM findAllPoseidonYmlFiles dirs
    logInfo $ show (length posFilesAllVersions) ++ " found"
    posFiles <- if _readOptIgnorePosVersion opts
                then return posFilesAllVersions
                else do
                    logInfo "Checking Poseidon versions... "
                    filterByPoseidonVersion posFilesAllVersions
    logInfo "Initializing packages... "
    eitherPackages <- mapM tryDecodePoseidonPackage $ zip [1..] posFiles
    -- notifying the users of package problems
    unless (null . lefts $ eitherPackages) $ do
        logWarning "Some packages were skipped due to issues:"
        forM_ (zip posFiles eitherPackages) $ \(posF, epac) -> do
            case epac of
                Left e -> do
                    logWarning $ "In the package described in " ++ posF ++ ":"
                    logWarning $ renderPoseidonException e
                _ -> return ()
    let loadedPackages = rights eitherPackages
    -- package duplication check
    -- This will throw if packages come with same versions and titles (see filterDuplicates)
    finalPackageList <- liftIO $ filterDuplicatePackages loadedPackages
    when (length loadedPackages > length finalPackageList) $ do
        logWarning "Some packages were skipped as duplicates:"
        forM_ (map posPacBaseDir loadedPackages \\ map posPacBaseDir finalPackageList) $
            \x -> logWarning x
    -- individual duplication check
    individuals <- liftIO $ mapM (uncurry loadIndividuals . \x -> (posPacBaseDir x, posPacGenotypeData x)) finalPackageList
    checkIndividualsUnique (_readOptStopOnDuplicates opts) $ concat individuals
    -- report number of valid packages
    logInfo $ "Packages loaded: " ++ (show . length $ finalPackageList)
    -- return package list
    return finalPackageList
  where
    filterByPoseidonVersion :: [FilePath] -> PoseidonLogIO [FilePath]
    filterByPoseidonVersion posFiles = do
        eitherPaths <- liftIO $ mapM isInVersionRange posFiles
        mapM_ (logWarning . renderPoseidonException) $ lefts eitherPaths
        return $ rights eitherPaths
        where
            isInVersionRange :: FilePath -> IO (Either PoseidonException FilePath)
            isInVersionRange posFile = do
                content <- readFile' posFile
                let posLines = lines content
                -- This implementation only works with a true YAML file.
                -- But technically also JSON is YAML. If somebody prepares
                -- a POSEIDON.yml file in JSON format, a wrong version
                -- can not be caught.
                case elemIndex "poseidonVersion:" (map (take 16) posLines) of
                    Nothing -> return $ Left $ PoseidonPackageMissingVersionException posFile
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
    tryDecodePoseidonPackage :: (Integer, FilePath) -> PoseidonLogIO (Either PoseidonException PoseidonPackage)
    tryDecodePoseidonPackage (numberPackage, path) = do
        logDebug $ "Package " ++ show numberPackage ++ ": " ++ path
        try . readPoseidonPackage opts $ path

-- | A function to read in a poseidon package from a YAML file. Note that this function calls the addFullPaths function to
-- make paths absolute.
readPoseidonPackage :: PackageReadOptions
                    -> FilePath -- ^ the file path to the yaml file
                    -> PoseidonLogIO PoseidonPackage -- ^ the returning package returned in the IO monad.
readPoseidonPackage opts ymlPath = do
    let baseDir = takeDirectory ymlPath
    bs <- liftIO $ B.readFile ymlPath

    -- read yml files
    yml@(PoseidonYamlStruct ver tit des con pacVer mod_ geno jannoF jannoC bibF bibC readF changeF) <- case decodeEither' bs of
        Left err  -> throwM $ PoseidonYamlParseException ymlPath err
        Right pac -> return pac

    -- file existence and checksum test
    liftIO $ checkFiles baseDir (_readOptIgnoreChecksums opts) (_readOptIgnoreGeno opts) yml

    -- read janno (or fill with empty dummy object)
    indEntries <- liftIO $ loadIndividuals baseDir geno
    janno <- case poseidonJannoFilePath baseDir yml of
        Nothing -> do
            return $ createMinimalJanno indEntries
        Just p -> do
            loadedJanno <- readJannoFile p
            liftIO $ checkJannoIndConsistency tit loadedJanno indEntries
            return loadedJanno

    -- read bib (or fill with empty list)
    bib <- case poseidonBibFilePath baseDir yml of
        Nothing -> return ([] :: BibTeX)
        Just p  -> liftIO $ readBibTeXFile p
    liftIO $ checkJannoBibConsistency tit janno bib

    -- create PoseidonPackage
    let pac = PoseidonPackage baseDir ver tit des con pacVer mod_ geno jannoF janno jannoC bibF bib bibC readF changeF 1
    logEnv <- ask
    liftIO $ catch (
        when (not (_readOptIgnoreGeno opts) && _readOptGenoCheck opts) . runSafeT $ do
            -- we're using getJointGenotypeData here on a single package to check for SNP consistency
            -- since that check is only implemented in the jointLoading function, not in the per-package loading
            (_, eigenstratProd) <- getJointGenotypeData logEnv False [pac] Nothing
            runEffect $ eigenstratProd >-> P.take 100 >-> P.drain
        ) (throwIO . PoseidonGenotypeExceptionForward)
    return pac

-- throws exception if any file is missing or checksum is incorrect
checkFiles :: FilePath -> Bool -> Bool -> PoseidonYamlStruct -> IO ()
checkFiles baseDir ignoreChecksums ignoreGenotypeFilesMissing yml = do
    -- Check README File
    case poseidonReadmeFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> checkFile fn Nothing
    -- Check README File
    case poseidonChangelogFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> checkFile fn Nothing
    -- Check Bib File
    case poseidonBibFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> if ignoreChecksums
                   then checkFile fn Nothing
                   else checkFile fn $ _posYamlBibFileChkSum yml
    -- Check Janno File
    case poseidonJannoFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> if ignoreChecksums
                   then checkFile fn Nothing
                   else checkFile fn $ _posYamlJannoFileChkSum yml
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

checkJannoIndConsistency :: String -> [JannoRow] -> [EigenstratIndEntry] -> IO ()
checkJannoIndConsistency pacName janno indEntries = do
    let genoIDs         = [ x | EigenstratIndEntry  x _ _ <- indEntries]
        genoSexs        = [ x | EigenstratIndEntry  _ x _ <- indEntries]
        genoGroups      = [ x | EigenstratIndEntry  _ _ x <- indEntries]
    let jannoIDs        = map jPoseidonID janno
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
                       (filter (uncurry (/=)) $ zipWithPadding "?" "?" a b)
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
    let literatureInJanno = nub . concatMap getJannoList . mapMaybe jPublication $ janno
        literatureInBib = nub $ map bibEntryId bibtex
        literatureNotInBibButInJanno = literatureInJanno \\ literatureInBib
    unless (null literatureNotInBibButInJanno) $ throwM $ PoseidonCrossFileConsistencyException pacName $
        "The following papers lack BibTeX entries: " ++
        intercalate ", " literatureNotInBibButInJanno

checkIndividualsUnique :: Bool -> [EigenstratIndEntry] -> PoseidonLogIO ()
checkIndividualsUnique stopOnDuplicates indEntries = do
    let genoIDs = [ x | EigenstratIndEntry  x _ _ <- indEntries]
    when (length genoIDs /= length (nub genoIDs)) $ do
        if stopOnDuplicates
        then do
            liftIO $ throwIO $ PoseidonCollectionException $
                "Duplicate individuals in package collection (" ++
                intercalate ", " (genoIDs \\ nub genoIDs) ++
                ")"
        else do
            logWarning $
                "Duplicate individuals in package collection (" ++
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
    titleEq = \p1 p2 -> posPacTitle p1 == posPacTitle p2
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

-- | A function to read genotype data jointly from multiple packages
getJointGenotypeData :: MonadSafe m =>
                        LogEnv -- ^ how messages should be logged
                     -> Bool -- ^ whether to generate an intersection instead of union of input sites
                     -> [PoseidonPackage] -- ^ A list of poseidon packages.
                     -> Maybe FilePath -- ^ a genotype file to select SNPs from
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
getJointGenotypeData logEnv intersect pacs maybeSnpFile = do
    genotypeTuples <- sequence [loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac) | pac <- pacs]
    let indEntries      = map fst genotypeTuples
        jointIndEntries = concat indEntries
        nrInds          = map length indEntries
        pacNames        = map posPacTitle pacs
        prod            = (orderedZipAll compFunc . map snd) genotypeTuples >->
                                P.filter filterUnionOrIntersection >-> joinEntryPipe logEnv nrInds pacNames
    jointProducer <- case maybeSnpFile of
        Nothing -> do
            return prod
        Just fn -> do
            let snpProd = loadBimOrSnpFile fn >-> orderCheckPipe compFunc3
            return $ (orderedZip compFunc2 snpProd prod >> return [()]) >-> selectSnps (sum nrInds)
    return (jointIndEntries, void jointProducer)
  where
    compFunc :: (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine) -> Ordering
    compFunc (EigenstratSnpEntry c1 p1 _ _ _ _, _) (EigenstratSnpEntry c2 p2 _ _ _ _, _) = compare (c1, p1) (c2, p2)
    compFunc2 :: EigenstratSnpEntry -> (EigenstratSnpEntry, GenoLine) -> Ordering
    compFunc2 (EigenstratSnpEntry c1 p1 _ _ _ _) (EigenstratSnpEntry c2 p2 _ _ _ _, _) = compare (c1, p1) (c2, p2)
    compFunc3 :: EigenstratSnpEntry -> EigenstratSnpEntry -> Ordering
    compFunc3 (EigenstratSnpEntry c1 p1 _ _ _ _) (EigenstratSnpEntry c2 p2 _ _ _ _) = compare (c1, p1) (c2, p2)
    filterUnionOrIntersection :: [Maybe (EigenstratSnpEntry, GenoLine)] -> Bool
    filterUnionOrIntersection maybeTuples = not intersect || not (any isNothing maybeTuples)
    selectSnps :: (Monad m) => Int -> Pipe (Maybe EigenstratSnpEntry, Maybe (EigenstratSnpEntry, GenoLine)) (EigenstratSnpEntry, GenoLine) m r
    selectSnps n = for cat $ \case
        (Just _, Just (es, gl)) -> yield (es, gl)
        (Just snp, Nothing) -> unless intersect $ yield (snp, V.replicate n Missing)
        _ ->  return ()

getJointJanno :: [PoseidonPackage] -> [JannoRow]
getJointJanno pacs = concatJannos $ map posPacJanno pacs

getJointIndividualInfo :: [PoseidonPackage] -> [IndividualInfo]
getJointIndividualInfo packages = do
    pac <- packages
    jannoRow <- posPacJanno pac
    return $ IndividualInfo (jPoseidonID jannoRow) ((getJannoList . jGroupName) jannoRow) (posPacTitle pac)

-- | A pipe to merge the genotype entries from multiple packages.
-- Uses the `joinEntries` function and catches exceptions to skip the respective SNPs.
joinEntryPipe :: (MonadIO m) => LogEnv -> [Int] -> [String] -> Pipe [Maybe (EigenstratSnpEntry, GenoLine)] (EigenstratSnpEntry, GenoLine) m r
joinEntryPipe logEnv nrInds pacNames = for cat $ \maybeEntries -> do
    eitherJE <- liftIO . try $ joinEntries logEnv nrInds pacNames maybeEntries
    case eitherJE of
        Left (PoseidonGenotypeException err) ->
            logWithEnv logEnv . logDebug $ "Skipping SNP due to " ++ err
        Left e -> liftIO . throwIO $ e
        Right (eigenstratSnpEntry, genoLine) -> yield (eigenstratSnpEntry, genoLine)

loadBimOrSnpFile :: (MonadSafe m) => FilePath -> Producer EigenstratSnpEntry m ()
loadBimOrSnpFile fn
    | takeExtension fn == ".snp" = readEigenstratSnpFile fn
    | takeExtension fn == ".bim" = readBimFile fn
    | otherwise                  = throwM (PoseidonGenotypeException "option snpFile requires snp or bim file endings")

-- | A function to create a minimal POSEIDON package
newMinimalPackageTemplate :: FilePath -> String -> GenotypeDataSpec -> PoseidonPackage
newMinimalPackageTemplate baseDir name (GenotypeDataSpec format_ geno _ snp _ ind _ snpSet_) =
    PoseidonPackage {
        posPacBaseDir = baseDir
    ,   posPacPoseidonVersion = asVersion latestPoseidonVersion
    ,   posPacTitle = name
    ,   posPacDescription = Nothing
    ,   posPacContributor = []
    ,   posPacPackageVersion = Nothing
    ,   posPacLastModified = Nothing
    ,   posPacGenotypeData = GenotypeDataSpec format_ (takeFileName geno) Nothing (takeFileName snp) Nothing (takeFileName ind) Nothing snpSet_
    ,   posPacJannoFile = Nothing
    ,   posPacJanno = []
    ,   posPacJannoFileChkSum = Nothing
    ,   posPacBibFile = Nothing
    ,   posPacBib = [] :: BibTeX
    ,   posPacBibFileChkSum = Nothing
    ,   posPacReadmeFile = Nothing
    ,   posPacChangelogFile = Nothing
    ,   posPacDuplicate = 1
    }

makePseudoPackageFromGenotypeData :: GenotypeDataSpec -> IO PoseidonPackage
makePseudoPackageFromGenotypeData (GenotypeDataSpec format_ genoFile_ _ snpFile_ _ indFile_ _ snpSet_) = do
    let baseDir      = getBaseDir genoFile_ snpFile_ indFile_
        outInd       = takeFileName indFile_
        outSnp       = takeFileName snpFile_
        outGeno      = takeFileName genoFile_
        genotypeData = GenotypeDataSpec format_ outGeno Nothing outSnp Nothing outInd Nothing snpSet_
        pacName      = takeBaseName genoFile_
    inds <- loadIndividuals baseDir genotypeData
    newPackageTemplate baseDir pacName genotypeData (Just (Left inds)) []
    where
        getBaseDir :: FilePath -> FilePath -> FilePath -> FilePath
        getBaseDir g s i =
            let baseDirGeno = takeDirectory genoFile_
                baseDirSnp = takeDirectory snpFile_
                baseDirInd = takeDirectory indFile_
            in if baseDirGeno == baseDirSnp && baseDirSnp == baseDirInd
               then baseDirGeno
               else throwM $ PoseidonUnequalBaseDirException g s i

-- | A function to create a more complete POSEIDON package
-- This will take only the filenames of the provided files, so it assumes that the files will be copied into
-- the directory into which the YAML file will be written
newPackageTemplate :: FilePath -> String -> GenotypeDataSpec -> Maybe (Either [EigenstratIndEntry] [JannoRow]) -> BibTeX -> IO PoseidonPackage
newPackageTemplate baseDir name genoData indsOrJanno bib = do
    (UTCTime today _) <- getCurrentTime
    let minimalTemplate = newMinimalPackageTemplate baseDir name genoData
        fluffedUpTemplate = minimalTemplate {
            posPacDescription = Just "Empty package template. Please add a description"
        ,   posPacContributor = [
                ContributorSpec
                    "Josiah Carberry"
                    "carberry@brown.edu"
                    (Just $ ORCID {_orcidNums = "000000021825009", _orcidChecksum = '7'})
                ]
        ,   posPacPackageVersion = Just $ makeVersion [0, 1, 0]
        ,   posPacLastModified = Just today
        }
        jannoFilledTemplate = completeJannoSpec name indsOrJanno fluffedUpTemplate
        bibFilledTemplate = completeBibSpec name bib jannoFilledTemplate
    return bibFilledTemplate
    where
        completeJannoSpec _ Nothing inTemplate = inTemplate
        completeJannoSpec name_ (Just (Left a)) inTemplate =
            inTemplate {
                posPacJannoFile = Just $ name_ ++ ".janno",
                posPacJanno = createMinimalJanno a
            }
        completeJannoSpec name_ (Just (Right b)) inTemplate =
            inTemplate {
                posPacJannoFile = Just $ name_ ++ ".janno",
                posPacJanno = b
            }
        completeBibSpec _ [] inTemplate = inTemplate
        completeBibSpec name_ xs inTemplate =
            inTemplate {
                posPacBibFile = Just $ name_ ++ ".bib",
                posPacBib = xs
            }

writePoseidonPackage :: PoseidonPackage -> IO ()
writePoseidonPackage (PoseidonPackage baseDir ver tit des con pacVer mod_ geno jannoF _ jannoC bibF _ bibFC readF changeF _) = do
    let yamlPac = PoseidonYamlStruct ver tit des con pacVer mod_ geno jannoF jannoC bibF bibFC readF changeF
        outF = baseDir </> "POSEIDON.yml"
    encodeFilePretty outF yamlPac
