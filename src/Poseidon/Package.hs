{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PoseidonPackage(..),
    GenotypeDataSpec(..),
    GenotypeFormatSpec(..),
    ContributorSpec(..),
    readPoseidonPackage,
    findPoseidonPackages,
    filterDuplicatePackages,
    getIndividuals,
    EigenstratIndEntry(..)
) where

import           Control.Exception            (Exception, throwIO, try)
import           Control.Monad                (filterM, forM_)
import           Data.Aeson                   (FromJSON, parseJSON,
                                               withObject, withText, (.:),
                                               (.:?))
import qualified Data.ByteString              as B
import           Data.Either                  (lefts, rights)
import           Data.List                    (groupBy, sortOn)
import           Data.Text                    (unpack)
import           Data.Time                    (Day)
import           Data.Version                 (Version)
import           Data.Yaml                    (ParseException, decodeEither')
import           SequenceFormats.Eigenstrat   (EigenstratIndEntry (..),
                                               readEigenstratInd)
import           SequenceFormats.Plink        (readFamFile)
import           System.Directory             (doesDirectoryExist,
                                               listDirectory)
import           System.FilePath.Posix        (takeDirectory, takeFileName,
                                               (</>))

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

data PoseidonException = PoseidonYamlParseException FilePath ParseException
    deriving (Show)

instance Exception PoseidonException

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
            putStrLn ("Warning: When parsing " ++ fp ++ ": " ++ show err))
    subDirs     <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    morePosPacs <- fmap concat . mapM findPoseidonPackages $ subDirs
    return $ (rights posPac) ++ morePosPacs
  where
    tryReadPoseidonPackage :: FilePath -> IO (Either PoseidonException PoseidonPackage)
    tryReadPoseidonPackage = try . readPoseidonPackage

filterDuplicatePackages :: [PoseidonPackage] -> [PoseidonPackage]
filterDuplicatePackages packages = map (\p -> last (sortOn posPacLastModified p)) . groupBy titleEq . sortOn posPacTitle $ packages
  where
    titleEq = (\p1 p2 -> posPacTitle p1 == posPacTitle p2)

getIndividuals :: PoseidonPackage -> IO [EigenstratIndEntry]
getIndividuals pac = do
    let (GenotypeDataSpec format_ _ _ indF) = posPacGenotypeData pac
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstratInd indF
        GenotypeFormatPlink      -> readFamFile indF
