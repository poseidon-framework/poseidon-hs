{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Module (
    PoseidonModule(..),
    PoseidonModuleJSON(..),
    GenotypeDataSpecJSON(..),
    PoseidonIndEntry(..),
    PoseidonMetaData(..),
    readPoseidonModule,
    getCombinedGenotypeData
) where

import           SequenceFormats.Eigenstrat   (EigenstratIndEntry (..), Sex,
                                               EigenstratSnpEntry, GenoLine, readEigenstrat)

import           Control.Exception            (Exception)
import           Control.Monad.Catch          (MonadThrow, throwM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Aeson                   (FromJSON, Object, parseJSON,
                                               withObject, (.:), (.:?), eitherDecodeStrict)
import           Data.Aeson.Types             (Parser, modifyFailure)
import qualified Data.ByteString as B
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime, defaultTimeLocale,
                                               readSTime)
import           Data.Version                 (Version, parseVersion)
import           GHC.Generics hiding (moduleName)
import           Pipes                        (Producer)
import           Pipes.Safe                   (MonadSafe)
import System.FilePath.Posix (takeDirectory)
import           Text.ParserCombinators.ReadP (readP_to_S)

data PoseidonModuleJSON = PoseidonModuleJSON {
    moduleName      :: Text,
    metaData        :: Maybe FilePath,
    notes           :: Maybe Text,
    maintainer      :: Text,
    maintainerEmail :: Text,
    lastUpdate      :: UTCTime,
    version         :: Version,
    genotypeData    :: GenotypeDataSpecJSON
} deriving (Show, Eq)

data GenotypeDataSpecJSON = GenotypeDataSpecJSON {
    format   :: String,
    genoFile :: FilePath,
    snpFile  :: FilePath,
    indFile  :: FilePath
} deriving (Generic, Show, Eq)

instance FromJSON PoseidonModuleJSON where
    parseJSON = withObject "PoseidonModuleJSON" $ \v -> PoseidonModuleJSON
        <$> v .:  "moduleName"
        <*> v .:? "metaData"
        <*> v .:? "notes"
        <*> v .:  "maintainer"
        <*> v .:  "maintainerEmail"
        <*> parseLastUpdate v
        <*> parseModuleVersion v
        <*> v .:  "genotypeData"

parseLastUpdate :: Object -> Parser UTCTime
parseLastUpdate v = do
    lastUpdateString <- v .:  "lastUpdate"
    let parseResult = readSTime False defaultTimeLocale "%Y-%m-%d" lastUpdateString
    case parseResult of
        [(r, "")] -> return r
        otherwise -> fail ("could not parse lastUpdate date string " ++ lastUpdateString)

parseModuleVersion :: Object -> Parser Version
parseModuleVersion v = do
    versionString <- v .:  "version"
    let parseResult = (readP_to_S parseVersion) versionString
        validResults = filter ((==""). snd) $ parseResult
    case validResults of
        [(t, "")] -> return t
        otherwise -> fail ("could not parse version string " ++ versionString)

instance FromJSON GenotypeDataSpecJSON

data (MonadSafe m) => PoseidonModule m = PoseidonModule {
    pmBaseDir         :: FilePath,
    pmModuleName      :: Text,
    pmNotes           :: Maybe Text,
    pmMaintainer      :: Text,
    pmMaintainerEmail :: Text,
    pmLastUpdate      :: UTCTime,
    pmVersion         :: Version,
    pmIndividuals     :: [PoseidonIndEntry],
    pmGenotypeData    :: Producer (EigenstratSnpEntry, GenoLine) m ()
}

data PoseidonIndEntry = PoseidonIndEntry {
    piName          :: String,
    piEigenstratSex :: Sex,
    piEigenstratPop :: String,
    piMetaData      :: Maybe PoseidonMetaData
}

data PoseidonMetaData = PoseidonMetaData {
    pmCountry     :: String,
    pmGeoPosition :: (Double, Double),
    pmUncalAge    :: Int,
    pmCalAge      :: (Int, Int)
}

data PoseidonException = PoseidonModuleParseException String
                       | PoseidonGenotypeFormatException String deriving (Show)

instance Exception PoseidonException

data IndSelection = AllIndividuals | SelectionList [SelectionSpec]
data SelectionSpec = SelectedInd String | SelectedPop String

makePoseidonModule :: (MonadSafe m) => FilePath -> PoseidonModuleJSON -> m (PoseidonModule m)
makePoseidonModule baseDir json = do
    let GenotypeDataSpecJSON format g s i = genotypeData json
    (esIndEntries, prod) <- case format of
        "EIGENSTRAT" -> readEigenstrat g s i
        "PLINK" -> throwM $
            PoseidonGenotypeFormatException "Currently only EIGENSTRAT formatted genotype \
            \data is supported."
    let indEntries = [PoseidonIndEntry n s p Nothing | EigenstratIndEntry n s p <- esIndEntries]
    return $ PoseidonModule {
        pmBaseDir = baseDir ,
        pmModuleName = moduleName json,
        pmNotes = notes json,
        pmMaintainer = maintainer json,
        pmMaintainerEmail = maintainerEmail json,
        pmLastUpdate = lastUpdate json,
        pmVersion = version json,
        pmIndividuals = indEntries,
        pmGenotypeData = prod
    }
    
readPoseidonModule :: (MonadSafe m) => FilePath -> m (PoseidonModule m)
readPoseidonModule jsonPath = do
    let baseDir = takeDirectory jsonPath
    bs <- liftIO $ B.readFile jsonPath
    fromJSON <- case eitherDecodeStrict bs of
        Left err -> throwM $ PoseidonModuleParseException ("module JSON parsing error: " ++ err)
        Right v -> return v
    makePoseidonModule baseDir fromJSON

getCombinedGenotypeData :: (MonadSafe m) => [PoseidonModule m] -> IndSelection -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
getCombinedGenotypeData pms indSelection = undefined

