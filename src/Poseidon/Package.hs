{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Poseidon.Package (
    PoseidonPackage(..),
    GenotypeDataSpec(..),
    GenotypeFormatSpec(..),
    ContributorSpec(..),
    PoseidonException(..),
    PoseidonSample(..),
    readPoseidonPackage,
    findPoseidonPackages,
    filterDuplicatePackages,
    getIndividuals,
    loadPoseidonPackages,
    loadJannoFiles,
    getJointGenotypeData,
    EigenstratIndEntry(..)
) where

import           Poseidon.Utils             (PoseidonException (..))

import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, forM, forM_)
import           Control.Monad.Catch        (throwM)
import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             withText, (.:), (.:?))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as Bch
import           Data.Either                (lefts, rights)
import           Data.List                  (groupBy, nub, sortOn)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (unpack)
import           Data.Time                  (Day)
import qualified Data.Vector                as V
import           Data.Version               (Version)
import           Data.Yaml                  (decodeEither')
import           Pipes                      (Producer, (>->))
import           Pipes.OrderedZip           (orderedZip, orderCheckPipe)
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (MonadSafe)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine,
                                             readEigenstrat, readEigenstratInd)
import           SequenceFormats.Plink      (readFamFile, readPlink)
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath.Posix      (takeDirectory, takeFileName, (</>))
import           System.IO                  (hPutStrLn, stderr)
import           GHC.Generics               (Generic)
import qualified Data.Csv                   as Csv
import           Data.Char                  (ord)

-- | A data type to represent a Poseidon Package
data PoseidonPackage = PoseidonPackage
    { posPacPoseidonVersion :: Version -- ^ The version of the package
    , posPacTitle           :: String -- ^ The title of the package
    , posPacDescription     :: Maybe String -- ^ the optional description string of the package
    , posPacContributor     :: [ContributorSpec] -- ^ the contributor(s) of the package
    , posPacLastModified    :: Maybe Day -- ^ the optional date of last update
    , posPacBibFile         :: Maybe FilePath -- ^ the optional path to the bibliography file
    , posPacGenotypeData    :: GenotypeDataSpec -- ^ the paths to the genotype files
    , posPacJannoFile       :: FilePath -- ^ the path to the janno file 
    }
    deriving (Show, Eq)

-- | A data type to represent a contributor
data ContributorSpec = ContributorSpec
    { contributorName  :: String -- ^ the name of a contributor
    , contributorEmail :: String -- ^ the email address of a contributor
    }
    deriving (Show, Eq)

-- | A datatype to specify genotype files
data GenotypeDataSpec = GenotypeDataSpec
    { format   :: GenotypeFormatSpec -- ^ the genotype format
    , genoFile :: FilePath -- ^ path to the geno file
    , snpFile  :: FilePath -- ^ path to the snp file
    , indFile  :: FilePath -- ^ path to the ind file
    }
    deriving (Show, Eq)

-- | A data type representing the options fo the genotype format
data GenotypeFormatSpec = GenotypeFormatEigenstrat -- ^ the Eigenstrat format
    | GenotypeFormatPlink -- ^ the Plink format
    deriving (Show, Eq)

-- |A datatype to represent Sex in a janno file
data Sex = Male
    | Female
    | Unknown
    deriving (Eq, Show, Ord)

-- | A data type to represent a sample/janno file row
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/janno_columns.tsv
-- for more details
data PoseidonSample = PoseidonSample
    { posSamIndividualID        :: String
    , posSamCollectionID        :: Maybe String
    , posSamSourceTissue        :: Maybe [String]
    , posSamCountry             :: Maybe String
    , posSamLocation            :: Maybe String
    , posSamSite                :: Maybe String
    , posSamLatitude            :: Maybe Double
    , posSamLongitude           :: Maybe Double
    , posSamDateC14Labnr        :: Maybe [String]
    , posSamDateC14UncalBP      :: Maybe [Integer]
    , posSamDateC14UncalBPErr   :: Maybe [Integer]
    , posSamDateBCADMedian      :: Maybe Integer
    , posSamDateBCADStart       :: Maybe Integer
    , posSamDateBCADStop        :: Maybe Integer
    , posSamDateType            :: Maybe String
    , posSamNrLibraries         :: Maybe Integer
    , posSamDataType            :: Maybe [String]
    , posSamGenotypePloidy      :: Maybe String
    , posSamGroupName           :: [String]
    , posSamGeneticSex          :: Sex
    , posSamNrAutosomalSNPs     :: Maybe Integer
    , posSamCoverage1240K       :: Maybe Double
    , posSamMTHaplogroup        :: Maybe String
    , posSamYHaplogroup         :: Maybe String
    , posSamEndogenous          :: Maybe Double
    , posSamUDG                 :: Maybe String
    , posSamLibraryBuilt        :: Maybe String
    , posSamDamage              :: Maybe Double
    , posSamNuclearContam       :: Maybe Double
    , posSamNuclearContamErr    :: Maybe Double
    , posSamMTContam            :: Maybe Double
    , posSamMTContamErr         :: Maybe Double
    , posSamPrimaryContact      :: Maybe String
    , posSamPublication         :: Maybe String
    , posSamComments            :: Maybe String
    , posSamKeywords            :: Maybe [String]
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

-- | To facilitate automatic parsing of ContributorSpec from JSON files
instance FromJSON ContributorSpec where
    parseJSON = withObject "contributor" $ \v -> ContributorSpec
        <$> v .: "name"
        <*> v .: "email"

-- | To facilitate automatic parsing of GenotypeDataSpec from JSON files
instance FromJSON GenotypeDataSpec where
    parseJSON = withObject "GenotypeData" $ \v -> GenotypeDataSpec
        <$> v .: "format"
        <*> v .: "genoFile"
        <*> v .: "snpFile"
        <*> v .: "indFile"

-- | To facilitate automatic parsing of GenotypeFormatSpec from JSON files
instance FromJSON GenotypeFormatSpec where
    parseJSON = withText "format" $ \v -> case v of
        "EIGENSTRAT" -> pure GenotypeFormatEigenstrat
        "PLINK"      -> pure GenotypeFormatPlink
        _            -> fail ("unknown format " ++ unpack v)

-- | A helper function to add a base directory path to all file paths in a poseidon package.
-- By using the (</>) operator from System.FilePath.Posix, this automatically ensures that paths are only
-- added if the given paths in the Poseidon package are in fact relative. If they are absolute (which would be bad practice
-- but anyway), the (</>) operator would simply return the second argument, so it wouldn't attach the base path.
addFullPaths :: FilePath -- ^ the base file path to use as prefix for relative paths in the package
             -> PoseidonPackage -- ^ the original package
             -> PoseidonPackage -- ^ the new package with prefixed paths
addFullPaths baseDir pac =
    let bibFileFullPath                      = (baseDir </>) <$> posPacBibFile pac
        jannoFileFullPath                    = baseDir </> (posPacJannoFile pac)
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
            putStrLn ("Skipping package at " ++ fp ++ " due to YAML parsing error: " ++ show err)
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
    
zipAll :: MonadSafe m => [Int] -> [Producer (EigenstratSnpEntry, GenoLine) m r] -> Producer [(EigenstratSnpEntry, GenoLine)] m [r]
zipAll _                   []            = error "zipAll - should never happen (1)"
zipAll []                  _             = error "zipAll - should never happen (2)"
zipAll _                   [prod]        = fmap (\x -> [x]) (prod >-> orderCheckPipe compFunc1) >-> P.map (\x ->[x])
zipAll (nrHaps:restNrHaps) (prod1:prods) =
    fmap (\(r, rs) -> (r:rs)) (orderedZip compFunc2 (prod1 >-> orderCheckPipe compFunc1) (zipAll restNrHaps prods)) >-> P.map processMaybeTuples
  where
    processMaybeTuples :: (Maybe (EigenstratSnpEntry, GenoLine), Maybe [(EigenstratSnpEntry, GenoLine)]) -> [(EigenstratSnpEntry, GenoLine)]
    processMaybeTuples (Nothing,        Nothing)          = error "processMaybeTuples: should never happen"
    processMaybeTuples (Just (es, gl),  Nothing)          = (es, gl) : [(es, V.replicate l Missing) | l <- restNrHaps]
    processMaybeTuples (Nothing,        Just restEntries) = (fst (head restEntries), V.replicate nrHaps Missing) : restEntries
    processMaybeTuples (Just (es, gl1), Just restEntries) = (es, gl1) : restEntries

compFunc1 :: (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine) -> Ordering
compFunc1 (EigenstratSnpEntry c1 p1 _ _ _ _, _) (EigenstratSnpEntry c2 p2 _ _ _ _, _) = compare (c1, p1) (c2, p2)

compFunc2 :: (EigenstratSnpEntry, GenoLine) -> [(EigenstratSnpEntry, GenoLine)] -> Ordering
compFunc2 (EigenstratSnpEntry c1 p1 _ _ _ _, _) ((EigenstratSnpEntry c2 p2 _ _ _ _, _):_) = compare (c1, p1) (c2, p2)
compFunc2 _                                     []                                        = error "compFunc2 - should never happen"

-- Janno file loading

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions { 
    Csv.decDelimiter = fromIntegral (ord '\t')
}

instance Csv.FromRecord PoseidonSample

-- | A helper function for parsing double lists
bytestringToDouble :: [Bch.ByteString] -> [Double]
bytestringToDouble [] = []
bytestringToDouble (x:xs) = (read (Bch.unpack x) :: Double) : bytestringToDouble xs

instance Csv.FromField [Double] where
    parseField = fmap bytestringToDouble . fmap (\x -> Bch.splitWith (==';') x) . Csv.parseField

-- | A helper function for parsing integer lists
bytestringToInteger :: [Bch.ByteString] -> [Integer]
bytestringToInteger [] = []
bytestringToInteger (x:xs) = (read (Bch.unpack x) :: Integer) : bytestringToInteger xs

instance Csv.FromField [Integer] where
    parseField = fmap bytestringToInteger . fmap (\x -> Bch.splitWith (==';') x) . Csv.parseField

-- | A helper function for parsing string lists
bytestringToString :: [Bch.ByteString] -> [String]
bytestringToString [] = []
bytestringToString (x:xs) = (Bch.unpack x) : bytestringToString xs

instance Csv.FromField [String] where
    parseField = fmap bytestringToString . fmap (\x -> Bch.splitWith (==';') x) . Csv.parseField

-- | A helper function for Sex values
stringToSex :: String -> Sex
stringToSex "F" = Female
stringToSex "M" = Male
stringToSex "U" = Unknown

instance Csv.FromField Sex where
    parseField = fmap stringToSex . fmap Bch.unpack . Csv.parseField

-- | A utility function to load multiple janno files
loadJannoFiles :: [FilePath] -> IO [[PoseidonSample]]
loadJannoFiles jannoPaths = mapM loadJannoFile jannoPaths

-- | A helper function to replace n/a values in janno files with empty bytestrings 
replaceNA :: Bch.ByteString -> Bch.ByteString
replaceNA tsv =
   let tsvRows = Bch.lines tsv
       tsvCells = map (\x -> Bch.splitWith (=='\t') x) tsvRows
       tsvCellsUpdated = map (\x -> map (\y -> if y == (Bch.pack "n/a") then Bch.empty else y) x) tsvCells
       tsvRowsUpdated = map (\x -> Bch.intercalate (Bch.pack "\t") x) tsvCellsUpdated
   in Bch.unlines tsvRowsUpdated

-- | A function to load one janno file
loadJannoFile :: FilePath -> IO [PoseidonSample]
loadJannoFile jannoPath = do
    jannoFile <- Bch.readFile jannoPath
    -- replace n/a with empty
    let jannoFileUpdated = replaceNA jannoFile
    case Csv.decodeWith decodingOptions Csv.HasHeader jannoFileUpdated of
        Left err -> do
           throwIO $ PoseidonJannoException err
        Right (poseidonSamples :: V.Vector PoseidonSample) -> do
            return $ V.toList poseidonSamples
