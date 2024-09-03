{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PoseidonYamlStruct (..),
    PoseidonPackage(..),
    PoseidonException(..),
    PackageReadOptions (..),
    findAllPoseidonYmlFiles,
    readPoseidonPackageCollection,
    readPoseidonPackageCollectionWithSkipIndicator,
    getJointGenotypeData,
    getJointJanno,
    getJointIndividualInfo,
    getExtendedIndividualInfo,
    newMinimalPackageTemplate,
    newPackageTemplate,
    renderMismatch,
    zipWithPadding,
    writePoseidonPackage,
    defaultPackageReadOptions,
    readPoseidonPackage,
    makePseudoPackageFromGenotypeData,
    getJannoRowsFromPac,
    packagesToPackageInfos,
    getAllGroupInfo,
    validateGeno,
    filterToRelevantPackages
) where

import           Poseidon.BibFile           (BibEntry (..), BibTeX,
                                             readBibTeXFile)
import           Poseidon.Contributor       (ContributorSpec (..))
import           Poseidon.EntityTypes       (EntitySpec, HasNameAndVersion (..),
                                             IndividualInfo (..),
                                             IndividualInfoCollection,
                                             PacNameAndVersion (..),
                                             determineRelevantPackages,
                                             isLatestInCollection,
                                             makePacNameAndVersion,
                                             renderNameWithVersion)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), joinEntries,
                                             loadGenotypeData, loadIndividuals,
                                             printSNPCopyProgress)
import           Poseidon.Janno             (GeneticSex (..),
                                             JannoLibraryBuilt (..),
                                             JannoList (..), JannoRow (..),
                                             JannoRows (..), JannoUDG (..),
                                             createMinimalJanno,
                                             getMaybeJannoList,
                                             jannoHeaderString, readJannoFile)
import           Poseidon.PoseidonVersion   (asVersion, latestPoseidonVersion,
                                             showPoseidonVersion,
                                             validPoseidonVersions)
import           Poseidon.SequencingSource  (SSFLibraryBuilt (..), SSFUDG (..),
                                             SeqSourceRow (..),
                                             SeqSourceRows (..),
                                             readSeqSourceFile)
import           Poseidon.ServerClient      (AddJannoColSpec (..),
                                             ExtendedIndividualInfo (..),
                                             GroupInfo (..), PackageInfo (..))
import           Poseidon.Utils             (LogA, PoseidonException (..),
                                             PoseidonIO, checkFile,
                                             envErrorLength, envInputPlinkMode,
                                             envLogAction, logDebug, logError,
                                             logInfo, logWarning, logWithEnv,
                                             renderPoseidonException)

import           Control.DeepSeq            (($!!))
import           Control.Exception          (catch, throwIO)
import           Control.Monad              (filterM, forM, forM_, unless, void,
                                             when)
import           Control.Monad.Catch        (MonadThrow, throwM, try)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BSC
import           Data.Char                  (isSpace)
import           Data.Csv                   (toNamedRecord)
import           Data.Either                (lefts, rights)
import           Data.Function              (on)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (elemIndex, group, groupBy,
                                             intercalate, nub, sort, sortOn,
                                             (\\))
import           Data.Maybe                 (catMaybes, fromMaybe, isNothing,
                                             mapMaybe)
import           Data.Time                  (Day, UTCTime (..), getCurrentTime)
import qualified Data.Vector                as V
import           Data.Version               (Version (..), makeVersion)
import           Data.Yaml                  (decodeEither')
import           Data.Yaml.Pretty           (defConfig, encodePretty,
                                             setConfCompare, setConfDropNull)
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
import           SequenceFormats.Plink      (PlinkPopNameMode (..), readBimFile)
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            (takeBaseName, takeDirectory,
                                             takeExtension, takeFileName, (</>))
import           System.IO                  (IOMode (ReadMode), hGetContents,
                                             withFile)

-- | Internal structure for YAML loading only
data PoseidonYamlStruct = PoseidonYamlStruct
    { _posYamlPoseidonVersion     :: Version
    , _posYamlTitle               :: String
    , _posYamlDescription         :: Maybe String
    , _posYamlContributor         :: [ContributorSpec]
    , _posYamlPackageVersion      :: Maybe Version
    , _posYamlLastModified        :: Maybe Day
    , _posYamlGenotypeData        :: GenotypeDataSpec
    , _posYamlJannoFile           :: Maybe FilePath
    , _posYamlJannoFileChkSum     :: Maybe String
    , _posYamlSeqSourceFile       :: Maybe FilePath
    , _posYamlSeqSourceFileChkSum :: Maybe String
    , _posYamlBibFile             :: Maybe FilePath
    , _posYamlBibFileChkSum       :: Maybe String
    , _posYamlReadmeFile          :: Maybe FilePath
    , _posYamlChangelogFile       :: Maybe FilePath
    }
    deriving (Show, Eq, Generic)

poseidonJannoFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonJannoFilePath baseDir yml = (baseDir </>) <$> _posYamlJannoFile yml
poseidonSeqSourceFilePath :: FilePath -> PoseidonYamlStruct -> Maybe FilePath
poseidonSeqSourceFilePath baseDir yml = (baseDir </>) <$> _posYamlSeqSourceFile yml
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
        <*> v .:?  "sequencingSourceFile"
        <*> v .:?  "sequencingSourceFileChkSum"
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
        "sequencingSourceFile"       .= _posYamlSeqSourceFile x,
        "sequencingSourceFileChkSum" .= _posYamlSeqSourceFileChkSum x,
        "bibFile"         .= _posYamlBibFile x,
        "bibFileChkSum"   .= _posYamlBibFileChkSum x,
        "readmeFile"      .= _posYamlReadmeFile x,
        "changelogFile"   .= _posYamlChangelogFile x
        ]

instance HasNameAndVersion PoseidonYamlStruct where
    getPacName     = _posYamlTitle
    getPacVersion  = _posYamlPackageVersion

-- | A data type to represent a Poseidon Package
data PoseidonPackage = PoseidonPackage
    { posPacBaseDir             :: FilePath
    -- ^ the base directory of the YAML file
    , posPacPoseidonVersion     :: Version
    -- ^ the version of the package
    , posPacNameAndVersion      :: PacNameAndVersion
    -- ^ the title and version of the package
    , posPacDescription         :: Maybe String
    -- ^ the optional description string of the package
    , posPacContributor         :: [ContributorSpec]
    -- ^ the contributor(s) of the package
    , posPacLastModified        :: Maybe Day
    -- ^ the optional date of last update
    , posPacGenotypeData        :: GenotypeDataSpec
    -- ^ the paths to the genotype files
    , posPacJannoFile           :: Maybe FilePath
    -- ^ the path to the janno file
    , posPacJanno               :: JannoRows
    -- ^ the loaded janno file
    , posPacJannoFileChkSum     :: Maybe String
    -- ^ the optional jannofile checksum
    , posPacSeqSourceFile       :: Maybe FilePath
    -- ^ the path to the seqSource file
    , posPacSeqSource           :: SeqSourceRows
    -- ^ the loaded seqSource file
    , posPacSeqSourceFileChkSum :: Maybe String
    -- ^ the optional seqSource file checksum
    , posPacBibFile             :: Maybe FilePath
    -- ^ the path to the BibTeX file
    , posPacBib                 :: BibTeX
    -- ^ the loaded bibliography file
    , posPacBibFileChkSum       :: Maybe String
    -- ^ the optional bibfile chksum
    , posPacReadmeFile          :: Maybe FilePath
    -- ^ the path to the README file
    , posPacChangelogFile       :: Maybe FilePath
    -- ^ the path to the CHANGELOG file
    }
    deriving (Show, Eq, Generic)

instance Ord PoseidonPackage where
    compare pacA pacB = compare (getPacName pacA, getPacVersion pacA) (getPacName pacB, getPacVersion pacB)

instance HasNameAndVersion PoseidonPackage where
    getPacName = getPacName . posPacNameAndVersion
    getPacVersion = getPacVersion . posPacNameAndVersion

data PackageReadOptions = PackageReadOptions
    { _readOptIgnoreChecksums  :: Bool
    -- ^ whether to ignore all checksums
    , _readOptIgnoreGeno       :: Bool
    -- ^ whether to ignore missing genotype files, useful for developer use cases
    , _readOptGenoCheck        :: Bool
    -- ^ whether to check the SNPs of the genotypes
    , _readOptFullGeno         :: Bool
    -- ^ whether to check all SNPs or only the first 100
    , _readOptIgnorePosVersion :: Bool
    -- ^ whether to ignore the Poseidon version of an input package.
    , _readOptOnlyLatest       :: Bool
    -- ^ whether to keep multiple versions of the same package (True) or just the latest one (False)
    }

-- Even though PlinkPopNameAsFamily is a sensible default, I would like to force the API to demand this explicitly
-- from the client caller, since we typically get this as an option from the CLI, and it would be wrong not to explicitly
-- pass it on to the package read system.
defaultPackageReadOptions :: PackageReadOptions
defaultPackageReadOptions = PackageReadOptions {
      _readOptIgnoreChecksums      = False
    , _readOptIgnoreGeno           = False
    , _readOptGenoCheck            = True
    , _readOptFullGeno             = False
    , _readOptIgnorePosVersion     = False
    , _readOptOnlyLatest           = False
    }

readPoseidonPackageCollection :: PackageReadOptions
                              -> [FilePath] -- ^ A list of base directories where to search in
                              -> PoseidonIO [PoseidonPackage] -- ^ A list of returned poseidon packages
readPoseidonPackageCollection opts baseDirs = fst <$> readPoseidonPackageCollectionWithSkipIndicator opts baseDirs

-- | a utility function to load all poseidon packages found recursively in multiple base directories.
-- This also takes care of smart filtering and duplication checks. Exceptions lead to skipping packages and outputting
-- warnings. A flag is returned for whether packages were skipped (needed for validate)
readPoseidonPackageCollectionWithSkipIndicator :: PackageReadOptions
                              -> [FilePath] -- ^ A list of base directories where to search in
                              -> PoseidonIO ([PoseidonPackage], Bool) -- ^ A list of returned poseidon packages and a flag for whether packages were skipped
readPoseidonPackageCollectionWithSkipIndicator opts baseDirs = do
    logInfo "Checking base directories... "
    goodDirs <- catMaybes <$> mapM checkIfBaseDirExists baseDirs
    logInfo "Searching POSEIDON.yml files... "
    posFilesAllVersions <- liftIO $ concat <$> mapM findAllPoseidonYmlFiles goodDirs
    logInfo $ show (length posFilesAllVersions) ++ " found"
    posFiles <- if _readOptIgnorePosVersion opts
                then return posFilesAllVersions
                else do
                    logInfo "Checking Poseidon versions... "
                    filterByPoseidonVersion posFilesAllVersions
    logInfo "Initializing packages... "
    eitherPackages <- mapM tryDecodePoseidonPackage $ zip [1..] posFiles
    -- notifying the users of package problems
    skipIndicator <- if (null . lefts $ eitherPackages) then return False else do
        logWarning "Some packages were skipped due to issues:"
        forM_ (zip posFiles eitherPackages) $ \(posF, epac) -> do
            case epac of
                Left e -> do
                    logWarning $ "In the package described in " ++ posF ++ ":"
                    logWarning $ renderPoseidonException e
                _ -> return ()
        return True
    let loadedPackages = rights eitherPackages
    -- filter the package list to only latest packages, if this is requested
    filteredPackageList <-
            if _readOptOnlyLatest opts
            then filterM (isLatestInCollection loadedPackages) loadedPackages
            else pure $ loadedPackages
    -- check if any packages appear more than once
    let duplicateGroups =   filter ((>1) . length)
                          . groupBy (\a b -> makePacNameAndVersion a == makePacNameAndVersion b)
                          . sortOn makePacNameAndVersion $ filteredPackageList
    unless (null duplicateGroups) $ do
        logError "There are duplicated packages in this package collection:"
        forM_ duplicateGroups $ \xs -> do
            logError $ "Duplicate package " ++ show (posPacNameAndVersion $ head xs) ++ " found at"
            forM_ xs $ \x -> do
                logError $ "  " ++ posPacBaseDir x
            throwM . PoseidonCollectionException $ "Detected duplicate packages."
    -- report number of valid packages
    let finalPackageList = sort filteredPackageList
    logInfo $ "Packages loaded: " ++ (show . length $ finalPackageList)
    -- return package list
    return (finalPackageList, skipIndicator)
  where
    checkIfBaseDirExists :: FilePath -> PoseidonIO (Maybe FilePath)
    checkIfBaseDirExists p = do
        exists <- liftIO $ doesDirectoryExist p
        if exists
        then return (Just p)
        else do
            logWarning $ "Base directory (-d) " ++ show p ++ " does not exist"
            return Nothing
    filterByPoseidonVersion :: [FilePath] -> PoseidonIO [FilePath]
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
    tryDecodePoseidonPackage :: (Integer, FilePath) -> PoseidonIO (Either PoseidonException PoseidonPackage)
    tryDecodePoseidonPackage (numberPackage, path) = do
        logDebug $ "Package " ++ show numberPackage ++ ": " ++ path
        try . readPoseidonPackage opts $ path

-- | A function to read in a poseidon package from a YAML file. Note that this function calls the addFullPaths function to
-- make paths absolute.
readPoseidonPackage :: PackageReadOptions
                    -> FilePath -- ^ the file path to the yaml file
                    -> PoseidonIO PoseidonPackage -- ^ the returning package returned in the IO monad.
readPoseidonPackage opts ymlPath = do
    let baseDir = takeDirectory ymlPath
    bs <- liftIO $ B.readFile ymlPath

    -- read yml files
    yml@(PoseidonYamlStruct ver tit des con pacVer mod_ geno jannoF jannoC seqSourceF seqSourceC bibF bibC readF changeF) <- case decodeEither' bs of
        Left err  -> throwM $ PoseidonYamlParseException ymlPath err
        Right pac -> return pac
    checkYML yml

    -- file existence and checksum test
    liftIO $ checkFiles baseDir (_readOptIgnoreChecksums opts) (_readOptIgnoreGeno opts) yml

    -- read janno (or fill with empty dummy object)
    indEntries <- loadIndividuals baseDir geno
    janno <- case poseidonJannoFilePath baseDir yml of
        Nothing -> do
            return $ createMinimalJanno indEntries
        Just p -> do
            loadedJanno <- readJannoFile p
            liftIO $ checkJannoIndConsistency tit loadedJanno indEntries
            return loadedJanno

    -- read seqSource
    seqSource <- case poseidonSeqSourceFilePath baseDir yml of
        Nothing -> return mempty
        Just p  -> readSeqSourceFile p
    checkSeqSourceJannoConsistency tit seqSource janno

    -- read bib (or fill with empty list)
    bib <- case poseidonBibFilePath baseDir yml of
        Nothing -> return ([] :: BibTeX)
        Just p  -> liftIO $ readBibTeXFile p
    liftIO $ checkJannoBibConsistency tit janno bib

    when (_readOptFullGeno opts) $ do
        logInfo $ "Trying to parse genotype data for package: " ++ tit

    -- create PoseidonPackage
    let pac = PoseidonPackage baseDir ver (PacNameAndVersion tit pacVer) des con mod_ geno jannoF janno jannoC seqSourceF seqSource seqSourceC bibF bib bibC readF changeF

    -- validate genotype data
    when (not (_readOptIgnoreGeno opts) && _readOptGenoCheck opts) $
        validateGeno pac (_readOptFullGeno opts)

    -- return complete, valid package
    return pac

checkYML :: PoseidonYamlStruct -> PoseidonIO ()
checkYML yml = do
    let contributors = _posYamlContributor yml
    when (null contributors) $ do
        logWarning $ "Contributor missing in POSEIDON.yml file of package " ++ renderNameWithVersion yml

validateGeno :: PoseidonPackage -> Bool -> PoseidonIO ()
validateGeno pac checkFullGeno = do
    logA <- envLogAction
    plinkMode <- envInputPlinkMode
    errLength <- envErrorLength
    --let jannoRows = getJannoRowsFromPac pac
    --let ploidyList = map jGenotypePloidy jannoRows
    --let indivNames = map jPoseidonID jannoRows
    liftIO $ catch (
        runSafeT $ do
            -- we're using getJointGenotypeData here on a single package to check for SNP consistency
            -- since that check is only implemented in the jointLoading function, not in the per-package loading
            (_, eigenstratProd) <- getJointGenotypeData logA False plinkMode [pac] Nothing
            -- check all or only the first 100 SNPs
            if checkFullGeno
            then do
                currentTime <- liftIO getCurrentTime
                --runEffect $ eigenstratProd >-> checkPloidy logA ploidyList indivNames >-> printSNPCopyProgress logA currentTime >-> P.drain
                runEffect $ eigenstratProd >-> printSNPCopyProgress logA currentTime >-> P.drain
            else
                --runEffect $ eigenstratProd >-> P.take 100 >-> checkPloidy logA ploidyList indivNames >-> P.drain
                runEffect $ eigenstratProd >-> P.take 100 >-> P.drain
        ) (throwIO . PoseidonGenotypeExceptionForward errLength)
  -- where
  --   checkPloidy logA ploidyList indivNames = for cat $ \(_, genoLine) -> do
  --       let illegals =
  --               map (\(_, ind, _) -> renderNameWithVersion pac ++ ": " ++ ind) .
  --               filter (\(ploidy, _, geno) -> ploidy == Just Haploid && geno == Het) .
  --               zip3 ploidyList indivNames . V.toList $ genoLine
  --       unless (null illegals) $ do
  --           logWithEnv logA . logDebug $ "The following samples have heterozygote genotypes despite being annotated as \"haploid\" in the Janno file:"
  --           mapM_ (logWithEnv logA . logDebug) illegals
  --           liftIO . throwIO $ PoseidonGenotypeException "Illegal heterozygote genotypes for individuals marked as 'haploid' in the Janno file. Choose --logMode VerboseLog to output more"


-- throws exception if any file is missing or checksum is incorrect
checkFiles :: FilePath -> Bool -> Bool -> PoseidonYamlStruct -> IO ()
checkFiles baseDir ignoreChecksums ignoreGenotypeFilesMissing yml = do
    -- Check README File
    case poseidonReadmeFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> checkFile fn Nothing
    -- Check CHANGELOG File
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
    -- Check SeqSource File
    case poseidonSeqSourceFilePath baseDir yml of
        Nothing -> return ()
        Just fn -> if ignoreChecksums
                   then checkFile fn Nothing
                   else checkFile fn $ _posYamlSeqSourceFileChkSum yml
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

checkJannoIndConsistency :: String -> JannoRows -> [EigenstratIndEntry] -> IO ()
checkJannoIndConsistency pacName (JannoRows rows) indEntries = do
    let genoIDs         = [ x | EigenstratIndEntry  x _ _ <- indEntries]
        genoSexs        = [ x | EigenstratIndEntry  _ x _ <- indEntries]
        genoGroups      = [ x | EigenstratIndEntry  _ _ x <- indEntries]
    let jannoIDs        = map jPoseidonID rows
        jannoSexs       = map (sfSex . jGeneticSex) rows
        jannoGroups     = map (show . head . getJannoList . jGroupName) rows
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
        "Individual GroupID mismatch between genotype data (left) and .janno files (right). Note \
        \that this could be due to a wrong Plink file population-name encoding \
        \(see the --inPlinkPopName option). " ++
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

checkSeqSourceJannoConsistency :: String -> SeqSourceRows -> JannoRows -> PoseidonIO ()
checkSeqSourceJannoConsistency pacName (SeqSourceRows sRows) (JannoRows jRows) = do
    checkPoseidonIDOverlap
    checkUDGandLibraryBuiltOverlap
    where
        js = map (\r -> (jPoseidonID r, jUDG r, jLibraryBuilt r)) jRows
        ss = map (\r -> (getMaybeJannoList $ sPoseidonID r, sUDG r, sLibraryBuilt r)) sRows
        checkPoseidonIDOverlap :: PoseidonIO ()
        checkPoseidonIDOverlap = do
            let flatSeqSourceIDs = nub $ concat $ [a | (a,_,_) <- ss]
                misMatch = flatSeqSourceIDs \\ [a | (a,_,_) <- js]
            unless (null misMatch) $ do
                logWarning $ "The .ssf file in the package " ++ pacName ++
                    " features Poseidon_IDs that are not in the package: " ++ intercalate ", " misMatch
        checkUDGandLibraryBuiltOverlap :: PoseidonIO ()
        checkUDGandLibraryBuiltOverlap = do
            mapM_ checkOneIndividual js
            where
                checkOneIndividual :: (String, Maybe JannoUDG, Maybe JannoLibraryBuilt) -> PoseidonIO ()
                checkOneIndividual (jannoPoseidonID, jannoUDG, jannoLibraryBuilt) = do
                    let relevantSeqSourceRows = filter (\(seqSourcePoseidonID,_,_) -> jannoPoseidonID `elem` seqSourcePoseidonID) ss
                        allSeqSourceUDGs = catMaybes $ [b | (_,b,_) <- relevantSeqSourceRows]
                        allSeqSourceLibraryBuilts = catMaybes $ [c | (_,_,c) <- relevantSeqSourceRows]
                    case jannoUDG of
                        Nothing -> return ()
                        Just j -> unless (all (compareU j) allSeqSourceUDGs) $
                            throwM $ PoseidonCrossFileConsistencyException pacName $
                            "The information on UDG treatment in .janno and .ssf do not match" ++
                            " for the individual: " ++ jannoPoseidonID ++ " (" ++ show j ++ " <> " ++ show allSeqSourceUDGs ++ ")"
                    case jannoLibraryBuilt of
                        Nothing -> return ()
                        Just j -> unless (all (compareL j) allSeqSourceLibraryBuilts) $
                            throwM $ PoseidonCrossFileConsistencyException pacName $
                            "The information on library strandedness in .janno and .ssf do not match" ++
                            " for the individual: " ++ jannoPoseidonID ++ " (" ++ show j ++ " <> " ++ show allSeqSourceLibraryBuilts ++ ")"
                compareU :: JannoUDG -> SSFUDG -> Bool
                compareU Mixed _        = True
                compareU Minus SSFMinus = True
                compareU Half  SSFHalf  = True
                compareU Plus  SSFPlus  = True
                compareU _     _        = False
                compareL :: JannoLibraryBuilt -> SSFLibraryBuilt -> Bool
                compareL MixedSSDS _     = True
                compareL DS        SSFDS = True
                compareL SS        SSFSS = True
                compareL _         _     = False

checkJannoBibConsistency :: String -> JannoRows -> BibTeX -> IO ()
checkJannoBibConsistency pacName (JannoRows rows) bibtex = do
    -- Cross-file consistency
    let literatureInJanno = map show $ nub . concatMap getJannoList . mapMaybe jPublication $ rows
        literatureInBib = nub $ map bibEntryId bibtex
        literatureNotInBibButInJanno = literatureInJanno \\ literatureInBib
    unless (null literatureNotInBibButInJanno) $ throwM $ PoseidonCrossFileConsistencyException pacName $
        "The following papers lack BibTeX entries: " ++
        intercalate ", " literatureNotInBibButInJanno

findAllPoseidonYmlFiles :: FilePath -> IO [FilePath]
findAllPoseidonYmlFiles baseDir = do
    entries <- listDirectory baseDir
    let posFiles = map (baseDir </>) $ filter (=="POSEIDON.yml") $ map takeFileName entries
    subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    morePosFiles <- fmap concat . mapM findAllPoseidonYmlFiles $ subDirs
    return $ posFiles ++ morePosFiles

-- | A function to read genotype data jointly from multiple packages
getJointGenotypeData :: MonadSafe m =>
                        LogA -- ^ how messages should be logged
                     -> Bool -- ^ whether to generate an intersection instead of union of input sites
                     -> PlinkPopNameMode -- ^ how to read population labels from Plink
                     -> [PoseidonPackage] -- ^ A list of poseidon packages.
                     -> Maybe FilePath -- ^ a genotype file to select SNPs from
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
getJointGenotypeData logA intersect popMode pacs maybeSnpFile = do
    genotypeTuples <- sequence [loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac) popMode | pac <- pacs]
    let indEntries      = map fst genotypeTuples
        jointIndEntries = concat indEntries
        nrInds          = map length indEntries
        pacNames        = map getPacName pacs
        prod            = (orderedZipAll compFunc . map snd) genotypeTuples >->
                                P.filter filterUnionOrIntersection >-> joinEntryPipe logA nrInds pacNames
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

getJointJanno :: [PoseidonPackage] -> JannoRows
getJointJanno pacs = mconcat $ map posPacJanno pacs

getJannoRowsFromPac :: PoseidonPackage -> [JannoRow]
getJannoRowsFromPac pac = let (JannoRows rows) = posPacJanno pac in rows

-- | A pipe to merge the genotype entries from multiple packages.
-- Uses the `joinEntries` function and catches exceptions to skip the respective SNPs.
joinEntryPipe :: (MonadIO m) => LogA -> [Int] -> [String] -> Pipe [Maybe (EigenstratSnpEntry, GenoLine)] (EigenstratSnpEntry, GenoLine) m r
joinEntryPipe logA nrInds pacNames = for cat $ \maybeEntries -> do
    eitherJE <- liftIO . try $ joinEntries logA nrInds pacNames maybeEntries
    case eitherJE of
        Left (PoseidonGenotypeException err) ->
            logWithEnv logA . logDebug $ "Skipping SNP due to " ++ err
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
    ,   posPacNameAndVersion = PacNameAndVersion name Nothing
    ,   posPacDescription = Nothing
    ,   posPacContributor = []
    ,   posPacLastModified = Nothing
    ,   posPacGenotypeData = GenotypeDataSpec format_ (takeFileName geno) Nothing (takeFileName snp) Nothing (takeFileName ind) Nothing snpSet_
    ,   posPacJannoFile = Nothing
    ,   posPacJanno = mempty
    ,   posPacJannoFileChkSum = Nothing
    ,   posPacSeqSourceFile = Nothing
    ,   posPacSeqSource = mempty
    ,   posPacSeqSourceFileChkSum = Nothing
    ,   posPacBibFile = Nothing
    ,   posPacBib = [] :: BibTeX
    ,   posPacBibFileChkSum = Nothing
    ,   posPacReadmeFile = Nothing
    ,   posPacChangelogFile = Nothing
    }

makePseudoPackageFromGenotypeData :: GenotypeDataSpec -> PoseidonIO PoseidonPackage
makePseudoPackageFromGenotypeData (GenotypeDataSpec format_ genoFile_ _ snpFile_ _ indFile_ _ snpSet_) = do
    let baseDir      = getBaseDir genoFile_ snpFile_ indFile_
        outInd       = takeFileName indFile_
        outSnp       = takeFileName snpFile_
        outGeno      = takeFileName genoFile_
        genotypeData = GenotypeDataSpec format_ outGeno Nothing outSnp Nothing outInd Nothing snpSet_
        pacName      = takeBaseName genoFile_
    inds <- loadIndividuals baseDir genotypeData
    newPackageTemplate baseDir pacName genotypeData (Just (Left inds)) mempty []
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
newPackageTemplate ::
       FilePath
    -> String
    -> GenotypeDataSpec
    -> Maybe (Either [EigenstratIndEntry] JannoRows)
    -> SeqSourceRows
    -> BibTeX
    -> PoseidonIO PoseidonPackage
newPackageTemplate baseDir name genoData indsOrJanno seqSource bib = do
    (UTCTime today _) <- liftIO getCurrentTime
    let minimalTemplate = newMinimalPackageTemplate baseDir name genoData
        fluffedUpTemplate = minimalTemplate {
            posPacDescription = Just "Empty package template. Please add a description"
        ,   posPacContributor = []
        ,   posPacNameAndVersion = PacNameAndVersion name (Just $ makeVersion [0, 1, 0])
        ,   posPacLastModified = Just today
        }
        jannoFilledTemplate     = completeJannoSpec name indsOrJanno fluffedUpTemplate
        seqSourceFilledTemplate = completeSeqSourceSpec name seqSource jannoFilledTemplate
        bibFilledTemplate       = completeBibSpec name bib seqSourceFilledTemplate
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
        completeSeqSourceSpec _ (SeqSourceRows []) inTemplate = inTemplate
        completeSeqSourceSpec name_ xs inTemplate =
            inTemplate {
                posPacSeqSourceFile = Just $ name_ ++ ".ssf",
                posPacSeqSource = xs
            }
        completeBibSpec _ [] inTemplate = inTemplate
        completeBibSpec name_ xs inTemplate =
            inTemplate {
                posPacBibFile = Just $ name_ ++ ".bib",
                posPacBib = xs
            }

writePoseidonPackage :: PoseidonPackage -> IO ()
writePoseidonPackage (PoseidonPackage baseDir ver nameAndVer des con mod_ geno jannoF _ jannoC seqSourceF _ seqSourceC bibF _ bibFC readF changeF) = do
    let yamlPac = PoseidonYamlStruct ver (getPacName nameAndVer) des con (getPacVersion nameAndVer) mod_ geno jannoF jannoC seqSourceF seqSourceC bibF bibFC readF changeF
        outF = baseDir </> "POSEIDON.yml"
    B.writeFile outF $!! encodePretty opts yamlPac
    where
        opts = setConfDropNull True $ setConfCompare (compare `on` fieldIndex) defConfig
        fieldIndex s = fromMaybe (length fields) $ s `elemIndex` fields
        fields = [
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
          "sequencingSourceFile",
          "sequencingSourceFileChkSum",
          "bibFile",
          "bibFileChkSum",
          "readmeFile",
          "changelogFile"
         ]

packagesToPackageInfos :: (MonadThrow m) => [PoseidonPackage] -> m [PackageInfo]
packagesToPackageInfos pacs = do
    forM pacs $ \pac -> do
        isLatest <- isLatestInCollection pacs pac
        return $ PackageInfo {
            pPac           = posPacNameAndVersion pac,
            pIsLatest      = isLatest,
            pPosVersion    = posPacPoseidonVersion pac,
            pDescription   = posPacDescription pac,
            pLastModified  = posPacLastModified pac,
            pNrIndividuals = (length . getJannoRowsFromPac) pac
        }

getAllGroupInfo :: (MonadThrow m) => [PoseidonPackage] -> m [GroupInfo]
getAllGroupInfo packages = do
    let individualInfoUnnested = do
            pac <- packages
            jannoRow <- getJannoRowsFromPac pac
            let groups = getJannoList . jGroupName $ jannoRow
            [(g, makePacNameAndVersion pac) | g <- groups]
    -- loop over pairs of groups and pacNames
    forM ((group . sort) individualInfoUnnested) $ \group_ -> do
        let groupName   = show . head . map fst $ group_
            groupPac    = head . map snd $ group_
            groupNrInds = length group_
        isLatest <- isLatestInCollection (map makePacNameAndVersion packages) groupPac
        return $ GroupInfo groupName groupPac isLatest groupNrInds

getJointIndividualInfo :: (MonadThrow m) => [PoseidonPackage] -> m IndividualInfoCollection
getJointIndividualInfo packages = do
    indInfoLatestPairs <- forM packages $ \pac -> do
        isLatest <- isLatestInCollection packages pac
        forM (getJannoRowsFromPac pac) $ \jannoRow -> do
            let indInfo = IndividualInfo
                    (jPoseidonID jannoRow)
                    ((map show . getJannoList . jGroupName) jannoRow)
                    (makePacNameAndVersion pac)
            return (indInfo, isLatest)
    return (map fst . concat $ indInfoLatestPairs, map snd . concat $ indInfoLatestPairs)


getExtendedIndividualInfo :: (MonadThrow m) => [PoseidonPackage] -> AddJannoColSpec -> m [ExtendedIndividualInfo]
getExtendedIndividualInfo allPackages addJannoColSpec = sequence $ do -- list monad
    pac <- allPackages -- outer loop (automatically concatenating over inner loops)
    jannoRow <- getJannoRowsFromPac pac -- inner loop
    let name = jPoseidonID jannoRow
        groups = map show $ getJannoList . jGroupName $ jannoRow
        colNames = case addJannoColSpec of
            AddJannoColAll -> jannoHeaderString \\ ["Poseidon_ID", "Group_Name"] -- Nothing means all Janno columns
                                                                          -- except for these two which are already explicit
            AddJannoColList c  -> c
        additionalColumnEntries = [(k, BSC.unpack <$> toNamedRecord jannoRow HM.!? BSC.pack k) | k <- colNames]
    isLatest <- isLatestInCollection allPackages pac -- this lives in monad m
    -- double-return for m and then list.
    return . return $ ExtendedIndividualInfo name groups (makePacNameAndVersion pac) isLatest additionalColumnEntries

-- | Filter packages such that only packages with individuals covered by the given EntitySpec are returned
filterToRelevantPackages :: (MonadThrow m) => (EntitySpec a) => [a] -> [PoseidonPackage] -> m [PoseidonPackage]
filterToRelevantPackages entities packages = do
    indInfoCollection <- getJointIndividualInfo packages
    relevantPacs <- determineRelevantPackages entities indInfoCollection
    return $ filter (\p -> makePacNameAndVersion p `elem` relevantPacs) packages
