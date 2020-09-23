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
    getIndividuals,
    loadPoseidonPackages,
    getJointGenotypeData,
    EigenstratIndEntry(..)
) where

import           Poseidon.Utils             (PoseidonException (..))

import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, forM, forM_, when)
import           Control.Monad.Catch        (throwM)
import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             withText, (.:), (.:?))
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
import           Data.List                  (groupBy, nub, sortOn)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (unpack)
import           Data.Time                  (Day)
import qualified Data.Vector                as V
import           Data.Version               (Version)
import           Data.Yaml                  (decodeEither')
import           Pipes                      (Producer, (>->))
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

data PoseidonPackage = PoseidonPackage
    { posPacPoseidonVersion :: Version
    , posPacTitle           :: String
    , posPacDescription     :: Maybe String
    , posPacContributor     :: [ContributorSpec]
    , posPacLastModified    :: Maybe Day
    , posPacBibFile         :: Maybe FilePath
    , posPacGenotypeData    :: GenotypeDataSpec
    , posPacJannoFile       :: FilePath
    }
    deriving (Show, Eq)

data ContributorSpec = ContributorSpec
    { contributorName  :: String
    , contributorEmail :: String
    }
    deriving (Show, Eq)

data GenotypeDataSpec = GenotypeDataSpec
    { format   :: GenotypeFormatSpec
    , genoFile :: FilePath
    , snpFile  :: FilePath
    , indFile  :: FilePath
    }
    deriving (Show, Eq)

data GenotypeFormatSpec = GenotypeFormatEigenstrat
    | GenotypeFormatPlink
    deriving (Show, Eq)

instance FromJSON PoseidonPackage where
    parseJSON = withObject "PoseidonPackage" $ \v -> PoseidonPackage
        <$> v .:   "poseidonVersion" --parseModuleVersion
        <*> v .:   "title"
        <*> v .:?  "description"
        <*> v .:   "contributor"
        <*> v .:?  "lastModified" --parseLastModified
        <*> v .:?  "bibFile"
        <*> v .:   "genotypeData"
        <*> v .:   "jannoFile"

instance FromJSON ContributorSpec where
    parseJSON = withObject "contributor" $ \v -> ContributorSpec
        <$> v .: "name"
        <*> v .: "email"

instance FromJSON GenotypeDataSpec where
    parseJSON = withObject "GenotypeData" $ \v -> GenotypeDataSpec
        <$> v .: "format"
        <*> v .: "genoFile"
        <*> v .: "snpFile"
        <*> v .: "indFile"

instance FromJSON GenotypeFormatSpec where
    parseJSON = withText "format" $ \v -> case v of
        "EIGENSTRAT" -> pure GenotypeFormatEigenstrat
        "PLINK"      -> pure GenotypeFormatPlink
        _            -> fail ("unknown format " ++ unpack v)


addFullPaths :: FilePath -> PoseidonPackage -> PoseidonPackage
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

readPoseidonPackage :: FilePath -> IO PoseidonPackage
readPoseidonPackage yamlPath = do
    let baseDir = takeDirectory yamlPath
    bs <- B.readFile yamlPath
    fromJSON <- case decodeEither' bs of
        Left err  -> throwIO $ PoseidonYamlParseException yamlPath err
        Right pac -> return pac
    return $ addFullPaths baseDir fromJSON

findPoseidonPackages :: FilePath -> IO [PoseidonPackage]
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

loadPoseidonPackages :: [FilePath] -> IO [PoseidonPackage]
loadPoseidonPackages dirs = do
    allPackages <- concat <$> mapM findPoseidonPackages dirs
    let checked = filterDuplicatePackages allPackages
    forM_ (lefts checked) $ \(PoseidonPackageException err) ->
        hPutStrLn stderr err
    return $ rights checked

filterDuplicatePackages :: [PoseidonPackage] -> [Either PoseidonException PoseidonPackage]
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

getIndividuals :: PoseidonPackage -> IO [EigenstratIndEntry]
getIndividuals pac = do
    let (GenotypeDataSpec format_ _ _ indF) = posPacGenotypeData pac
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstratInd indF
        GenotypeFormatPlink      -> readFamFile indF

getGenotypeData :: (MonadSafe m) => PoseidonPackage -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
getGenotypeData pac = do
    let (GenotypeDataSpec format_ genoF snpF indF) = posPacGenotypeData pac
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstrat genoF snpF indF
        GenotypeFormatPlink      -> readPlink genoF snpF indF

getJointGenotypeData :: (MonadSafe m) => [PoseidonPackage] -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
getJointGenotypeData pacs = do
    genotypeTuples <- mapM getGenotypeData pacs
    let jointIndEntries = concat . map fst $ genotypeTuples
        jointProducer = (zipAll . map snd) genotypeTuples >-> P.mapM joinEntries
    return (jointIndEntries, jointProducer)
  where
    joinEntries :: (MonadSafe m) => [(EigenstratSnpEntry, GenoLine)] -> m (EigenstratSnpEntry, GenoLine)
    joinEntries tupleList = do
        let allSnpEntries                                    = map fst tupleList
            allGenoEntries                                   = map snd tupleList
            (EigenstratSnpEntry chrom1 pos1 _ _ refA1 altA1) = head allSnpEntries
        -- check for positions being the same across files
        when (or [(c, p) /= (chrom1, pos1) | EigenstratSnpEntry c p _ _ _ _ <- tail allSnpEntries]) $
            throwM (PoseidonGenotypeException ("SNP positions don't match: " ++ show allSnpEntries))
        -- check for alleles to be alignable
        allGenoEntriesFlipped <- forM (zip (tail allSnpEntries) (tail allGenoEntries)) $ \(EigenstratSnpEntry _ _ _ _ refA altA, genoLine) ->
            if (refA, altA) == (refA1, altA1)
            then return genoLine
            else if (refA, altA) == (altA1, refA1)
                    then return (flipGenotypes genoLine)
                    else throwM (PoseidonGenotypeException ("SNP alleles are incongruent " ++ show allSnpEntries))
        return (head allSnpEntries, V.concat (head allGenoEntries : allGenoEntriesFlipped))
    flipGenotypes :: GenoLine -> GenoLine
    flipGenotypes = V.map (\a -> case a of
        HomRef  -> HomAlt
        Het     -> Het
        HomAlt  -> HomRef
        Missing -> Missing)

zipAll :: Monad m => [Producer a m r] -> Producer [a] m r
zipAll []            = error "zipAll - should never happen"
zipAll [prod]        = prod >-> P.map (\x ->[x])
zipAll (prod1:prods) = P.zip prod1 (zipAll prods) >-> P.map (\(a, as) -> a:as)
