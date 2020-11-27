{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PoseidonPackage(..),
    GenotypeDataSpec(..),
    GenotypeFormatSpec(..),
    ContributorSpec(..),
    PoseidonException(..),
    readPoseidonPackage,
    findPoseidonPackages,
    filterDuplicatePackages,
    loadPoseidonPackages,
    maybeLoadJannoFiles,
    maybeLoadBibTeXFiles,
    getJointGenotypeData,
    getIndividuals
) where

import           Poseidon.GenotypeData      (zipAll)
import           Poseidon.Janno             (PoseidonSample (..), loadJannoFile)
import           Poseidon.Utils             (PoseidonException (..))
import           Poseidon.BibFile           (loadBibTeXFile)


import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, forM, forM_)
import           Control.Monad.Catch        (throwM)
import           Data.Aeson                 (FromJSON, ToJSON, object, parseJSON,
                                             toJSON, withObject, withText, (.:),
                                             (.:?), (.=))
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
import           Data.List                  (groupBy, nub, sortOn)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import           Data.Time                  (Day)
import qualified Data.Vector                as V
import           Data.Version               (Version)
import           Data.Yaml                  (decodeEither')
import           GHC.Generics               (Generic)
import           Pipes                      (Producer, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (MonadSafe)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine,
                                             readEigenstrat, readEigenstratInd)
import           SequenceFormats.Plink      (readFamFile, readPlink)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             listDirectory)
import           System.FilePath.Posix      (takeDirectory, takeFileName, (</>))
import           System.IO                  (hPutStrLn, stderr)
import           Text.CSL.Exception         (CiteprocException)
import           Text.CSL.Reference         (Reference(..))

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
    , posPacLastModified    :: Maybe Day
    -- ^ the optional date of last update
    , posPacBibFile         :: Maybe FilePath
    -- ^ the optional path to the bibliography file
    , posPacGenotypeData    :: GenotypeDataSpec
    -- ^ the paths to the genotype files
    , posPacJannoFile       :: Maybe FilePath
    -- ^ the path to the janno file
    }
    deriving (Show, Eq, Generic)

-- | The FromJSON instance for the PoseidonPackage data type. Necessary to facilitate automatic reading from JSON files
instance FromJSON PoseidonPackage where
    parseJSON = withObject "PoseidonPackage" $ \v -> PoseidonPackage
        <$> v .:   "poseidonVersion"
        <*> v .:   "title"
        <*> v .:?  "description"
        <*> v .:   "contributor"
        <*> v .:?  "lastModified"
        <*> v .:?  "bibFile"
        <*> v .:   "genotypeData"
        <*> v .:   "jannoFile"

instance ToJSON PoseidonPackage where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "poseidonVersion" .= posPacPoseidonVersion x,
        "title" .= posPacTitle x,
        "description" .= posPacDescription x,
        "contributor" .= posPacContributor x,
        "lastModified" .= posPacLastModified x,
        "bibFile" .= posPacBibFile x,
        "genotypeData" .= posPacGenotypeData x,
        "jannoFile" .= posPacJannoFile x
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

-- | A datatype to specify genotype files
data GenotypeDataSpec = GenotypeDataSpec
    { format   :: GenotypeFormatSpec -- ^ the genotype format
    -- ^ path to the geno file
    , genoFile :: FilePath -- ^ path to the geno file
    -- ^ path to the snp file
    , snpFile  :: FilePath -- ^ path to the snp file
    -- ^ path to the ind file
    , indFile  :: FilePath -- ^ path to the ind file
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of GenotypeDataSpec from JSON files
instance FromJSON GenotypeDataSpec where
    parseJSON = withObject "GenotypeData" $ \v -> GenotypeDataSpec
        <$> v .: "format"
        <*> v .: "genoFile"
        <*> v .: "snpFile"
        <*> v .: "indFile"

instance ToJSON GenotypeDataSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "format" .= format x,
        "genoFile" .= genoFile x,
        "snpFile" .= snpFile x,
        "indFile" .= indFile x
        ]

-- | A data type representing the options fo the genotype format
data GenotypeFormatSpec = GenotypeFormatEigenstrat
    | GenotypeFormatPlink
    deriving (Show, Eq)

-- | To facilitate automatic parsing of GenotypeFormatSpec from JSON files
instance FromJSON GenotypeFormatSpec where
    parseJSON = withText "format" $ \v -> case v of
        "EIGENSTRAT" -> pure GenotypeFormatEigenstrat
        "PLINK"      -> pure GenotypeFormatPlink
        _            -> fail ("unknown format " ++ T.unpack v)

instance ToJSON GenotypeFormatSpec where
    toJSON a = case a of
        GenotypeFormatPlink      -> "PLINK"
        GenotypeFormatEigenstrat -> "EIGENSTRAT"

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
    entries <- listDirectory baseDir
    posPac  <- mapM tryReadPoseidonPackage . map (baseDir </>) . filter ((=="POSEIDON.yml") . takeFileName) $ entries
    forM_ (lefts posPac) $ (\e -> case e of
        PoseidonYamlParseException fp err ->
            putStrLn ("Can't read package at " ++ fp ++ " due to YAML parsing error: " ++ show err)
        _ -> error "this should never happen")
    subDirs     <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    morePosPacs <- fmap concat . mapM findPoseidonPackages $ subDirs
    return $ (rights posPac) ++ morePosPacs
  where
    tryReadPoseidonPackage :: FilePath -> IO (Either PoseidonException PoseidonPackage)
    tryReadPoseidonPackage = try . readPoseidonPackage

-- | a utility function to load all poseidon packages found recursively in multiple base directories.
loadPoseidonPackages :: [FilePath] -- ^ A list of base directories where to search in
                     -> IO [PoseidonPackage] -- ^ A list of returned poseidon packages.
loadPoseidonPackages dirs = do
    allPackages <- concat <$> mapM findPoseidonPackages dirs
    let checked = filterDuplicatePackages allPackages
    forM_ (lefts checked) $ \(PoseidonPackageException err) ->
        hPutStrLn stderr err
    return $ rights checked

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
            let maybeDates = map posPacLastModified pacs
            in  if (length . nub . catMaybes) maybeDates == length maybeDates -- all dates need to be given and be unique
                then
                    return . last . sortOn posPacLastModified $ pacs
                else
                    let t   = posPacTitle (head pacs)
                        msg = "duplicate package with missing lastModified field: " ++ t
                    in  Left $ PoseidonPackageException msg

-- | A function to return a list of all individuals in the genotype files of a package.
getIndividuals :: PoseidonPackage -- ^ the Poseidon package
               -> IO [EigenstratIndEntry] -- ^ the returned list of EigenstratIndEntries.
getIndividuals pac = do
    let (GenotypeDataSpec format_ _ _ indF) = posPacGenotypeData pac
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstratInd indF
        GenotypeFormatPlink      -> readFamFile indF

-- | A function to read the genotype data of a package
getGenotypeData :: (MonadSafe m) => PoseidonPackage -- ^ the package
                -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line.
getGenotypeData pac = do
    let (GenotypeDataSpec format_ genoF snpF indF) = posPacGenotypeData pac
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstrat genoF snpF indF
        GenotypeFormatPlink      -> readPlink genoF snpF indF

-- | A function to read genotype data jointly from multiple packages
getJointGenotypeData :: (MonadSafe m) => [PoseidonPackage] -- ^ A list of poseidon packages.
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
getJointGenotypeData pacs = do
    genotypeTuples <- mapM getGenotypeData pacs
    let indEntries      = map fst genotypeTuples
        jointIndEntries = concat indEntries
        nrInds          = map length indEntries
        jointProducer   = (zipAll nrInds . map snd) genotypeTuples >-> P.mapM joinEntries
    return (jointIndEntries, jointProducer >> return ())
  where
    joinEntries :: (MonadSafe m) => [(EigenstratSnpEntry, GenoLine)] -> m (EigenstratSnpEntry, GenoLine)
    joinEntries tupleList = do
        let allSnpEntries                            = map fst tupleList
            allGenoEntries                           = map snd tupleList
            (EigenstratSnpEntry _ _ _ _ refA1 altA1) = head allSnpEntries
        allEntriesFlipped <- forM (zip (tail allSnpEntries) (tail allGenoEntries)) $ \(es@(EigenstratSnpEntry _ _ _ _ refA altA), genoLine) ->
            if alleleConcordant refA refA1 && alleleConcordant altA altA1
            then return (es, genoLine)
            else if alleleConcordant refA altA1 && alleleConcordant altA refA1
                    then return (es {snpRef = altA, snpAlt = refA}, flipGenotypes genoLine)
                    else throwM (PoseidonGenotypeException ("SNP alleles are incongruent " ++ show allSnpEntries))
        let allSnpEntriesFlipped  = (head allSnpEntries) : map fst allEntriesFlipped
            allGenoEntriesFlipped = (head allGenoEntries) : map snd allEntriesFlipped
        return (makeSnpEntriesConcordant allSnpEntriesFlipped, V.concat allGenoEntriesFlipped)
    alleleConcordant :: Char -> Char -> Bool
    alleleConcordant '0' _   = True
    alleleConcordant _   '0' = True
    alleleConcordant 'N' _   = True
    alleleConcordant _   'N' = True
    alleleConcordant a1  a2  = a1 == a2
    flipGenotypes :: GenoLine -> GenoLine
    flipGenotypes = V.map (\a -> case a of
        HomRef  -> HomAlt
        Het     -> Het
        HomAlt  -> HomRef
        Missing -> Missing)
    makeSnpEntriesConcordant :: [EigenstratSnpEntry] -> EigenstratSnpEntry
    makeSnpEntriesConcordant snpEntries@(e:_) =
        let allRefs            = map snpRef snpEntries
            allAlts            = map snpAlt snpEntries
            allInformativeRefs = filter (\c -> c /= '0' && c /= 'N') allRefs
            allInformativeAlts = filter (\c -> c /= '0' && c /= 'N') allAlts
            ref = if not (null allInformativeRefs) then head allInformativeRefs else head allRefs
            alt = if not (null allInformativeAlts) then head allInformativeAlts else head allAlts
        in  e {snpRef = ref, snpAlt = alt}
    makeSnpEntriesConcordant _ = error "should not happen"

-- Janno file loading

maybeLoadJannoFiles :: [PoseidonPackage] -> IO [Either PoseidonException [Either PoseidonException PoseidonSample]]
maybeLoadJannoFiles pacs = do
    mapM maybeLoadJannoFile pacs

maybeLoadJannoFile :: PoseidonPackage -> IO (Either PoseidonException [Either PoseidonException PoseidonSample])
maybeLoadJannoFile pac = do
    let maybeJannoPath = posPacJannoFile pac
    case maybeJannoPath of
        Nothing -> do
            return $ Left $ PoseidonFileExistenceException $
                posPacTitle pac ++ ": Can't find .janno file path in the POSEIDON.yml"
        Just x  -> do
            fileExists <- doesFileExist x
            if not fileExists
            then do
                return $ Left $ PoseidonFileExistenceException $
                    posPacTitle pac ++ ": Can't find .janno file " ++ show x
            else do
                samples <- loadJannoFile x
                return $ Right samples

-- BibFile file loading

maybeLoadBibTeXFiles :: [PoseidonPackage] -> IO [Either PoseidonException (Either CiteprocException [Reference])]
maybeLoadBibTeXFiles pacs = do
    mapM maybeLoadBibTeXFile pacs

maybeLoadBibTeXFile :: PoseidonPackage -> IO (Either PoseidonException (Either CiteprocException [Reference]))
maybeLoadBibTeXFile pac = do
    let maybeBibPath = posPacBibFile pac
    case maybeBibPath of
        Nothing -> do
            return $ Left $ PoseidonFileExistenceException $
                posPacTitle pac ++ ": Can't find .bib file path in the POSEIDON.yml"
        Just x  -> do
            fileExists <- doesFileExist x
            if not fileExists 
            then do
                return $ Left $ PoseidonFileExistenceException $
                    posPacTitle pac ++ ": Can't find .bib file " ++ show x
            else do
                references_ <- loadBibTeXFile x
                return $ Right references_
