{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Package (
    PoseidonPackage(..),
    GenotypeDataSpec(..),
    GenotypeFormatSpec(..),
    ContributorSpec(..),
    readPoseidonPackage,
    findPoseidonPackages,
    filterDuplicatePackages
) where

import           Control.Exception            (Exception, throwIO)
import           Control.Monad                (filterM)
import           Data.Aeson                   (FromJSON, Object, parseJSON,
                                               withObject, withText, (.:),
                                               (.:?))
import           Data.Aeson.Types             (Parser, modifyFailure)
import qualified Data.ByteString              as B
import           Data.List                    (sortOn, groupBy)
import           Data.Text                    (Text)
import           Data.Time                    (Day, defaultTimeLocale,
                                               readSTime)
import           Data.Version                 (Version, parseVersion)
import           Data.Yaml                    (decodeEither')
import           GHC.Generics                 hiding (moduleName)
import           Pipes                        (Producer)
import           Pipes.Safe                   (MonadSafe)
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist, listDirectory)
import           System.FilePath.Posix        (takeDirectory, takeFileName,
                                               (</>))
import           Text.ParserCombinators.ReadP (readP_to_S)

data PoseidonPackage = PoseidonPackage
    { posPacPoseidonVersion :: Version
    , posPacTitle           :: Text
    , posPacDescription     :: Text
    , posPacContributor     :: ContributorSpec
    , posPacLastModified    :: Day
    , posPacBibFile         :: Maybe FilePath
    , posPacGenotypeData    :: GenotypeDataSpec
    , posPacJannoFile       :: FilePath
    }
    deriving (Show, Eq)

data ContributorSpec = ContributorSpec
    { contributorName  :: Text
    , contributorEmail :: Text
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
        <$> v .:  "poseidonVersion" --parseModuleVersion
        <*> v .:  "title"
        <*> v .:  "description"
        <*> v .:  "contributor"
        <*> v .:  "lastModified" --parseLastModified
        <*> v .:? "bibFile"
        <*> v .:  "genotypeData"
        <*> v .:  "jannoFile"

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

parseLastModified :: Object -> Parser Day
parseLastModified v = do
    lastModifiedString <- v .: "lastModified"
    let parseResult = readSTime False defaultTimeLocale "%Y-%m-%d" lastModifiedString
    case parseResult of
        [(r, "")] -> return r
        otherwise -> fail ("could not parse date string " ++ lastModifiedString)

parseModuleVersion :: Object -> Parser Version
parseModuleVersion v = do
    versionString <- v .:  "poseidonVersion"
    let parseResult  = (readP_to_S parseVersion) versionString
        validResults = filter ((==""). snd) $ parseResult
    case validResults of
        [(t, "")] -> return t
        otherwise -> fail ("could not parse version string " ++ versionString)

data PoseidonException = PoseidonPackageParseException String
    | PoseidonGenotypeFormatException String
    deriving (Show)

instance Exception PoseidonException

addFullPaths :: FilePath -> PoseidonPackage -> PoseidonPackage
addFullPaths baseDir pac =
    let bibFileFullPath                      = (baseDir </>) <$> posPacBibFile pac
        jannoFileFullPath                    = baseDir </> (posPacJannoFile pac)
        GenotypeDataSpec format geno snp ind = posPacGenotypeData pac
        genotypeDataFullPath                 =
            GenotypeDataSpec format (baseDir </> geno) (baseDir </> snp) (baseDir </> ind)
    in  pac {
            posPacBibFile      = bibFileFullPath,
            posPacJannoFile    = jannoFileFullPath,
            posPacGenotypeData = genotypeDataFullPath
        }

readPoseidonPackage :: FilePath -> IO PoseidonPackage
readPoseidonPackage jsonPath = do
    let baseDir = takeDirectory jsonPath
    bs <- B.readFile jsonPath
    fromJSON <- case decodeEither' bs of
        Left err  -> throwIO $ PoseidonPackageParseException ("module YAML parsing error: " ++ show err)
        Right pac -> return pac
    return $ addFullPaths baseDir fromJSON

findPoseidonPackages :: FilePath -> IO [PoseidonPackage]
findPoseidonPackages baseDir = do
    entries     <- listDirectory baseDir
    posPac      <- mapM readPoseidonPackage . map (baseDir </>) . filter ((=="POSEIDON.yml") . takeFileName) $ entries
    subDirs     <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    morePosPacs <- fmap concat . mapM findPoseidonPackages $ subDirs
    return $ posPac ++ morePosPacs

filterDuplicatePackages :: [PoseidonPackage] -> [PoseidonPackage]
filterDuplicatePackages packages = map (\p -> last (sortOn posPacLastModified p)) . groupBy titleEq . sortOn posPacTitle $ packages
  where
    titleEq = (\p1 p2 -> posPacTitle p1 == posPacTitle p2)

