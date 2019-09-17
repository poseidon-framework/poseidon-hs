{-# LANGUAGE DeriveGeneric #-}

module Poseidon.Module (
    PoseidonModule(..),
    parsePoseidonModuleBS,
    parsePoseidonModule,
    getGenotypeDataData,
    getCombinedGenotypeData
) where

import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry, GenoLine)

import           Control.Exception          (Exception)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 (FromJSON)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Version               (Version)
import           GHC.Generics
import           Pipes                      (Producer)
import           Pipes.Safe                 (MonadSafe)

data PoseidonModuleJSON = PoseidonModuleJSON {
    moduleName      :: Text,
    metaData        :: Maybe FilePath,
    notes           :: Maybe Text,
    maintainer      :: Text,
    maintainerEmail :: Text,
    lastUpdate      :: UTCTime,
    version         :: Version,
    genotypeData    :: GenotypeDataSpecJSON
} deriving (Generic, Show)

data GenotypeDataSpecJSON = GenotypeDataSpecJSON {
    format   :: String,
    genoFile :: FilePath,
    snpFile  :: FilePath,
    indFile  :: FilePath
} deriving (Generic, Show)

instance FromJSON PoseidonModuleJSON
instance FromJSON GenotypeDataSpecJSON

data PoseidonModule = PoseidonModule {
    pmBaseDir         :: FilePath,
    pmModuleName      :: Text,
    pmNotes           :: Maybe Text,
    pmMaintainer      :: Text,
    pmMaintainerEmail :: Text,
    pmLastUpdate      :: UTCTime,
    pmVersion         :: Version,
    pmIndividuals     :: [PoseidonIndEntry],
    pmGenotypeData    :: m (Producer (EigenstratSnpEntry, GenoLine) m ())
}

data PoseidonIndEntry = PoseidonIndEntry {
    piName          :: String,
    piEigenstratSex :: String,
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
instance Exception PoseidonException

data IndSelection = AllIndividuals | SelectionList [SelectionSpec]
data SelectionSpec = SelectedInd String | SelectedPop String

parsePoseidonModuleBS :: (MonadThrow m) => ByteString -> m PoseidonModule
parsePoseidonModuleBS = undefined

parsePoseidonModule :: (MonadIO m) => FilePath -> m PoseidonModule
parsePoseidonModule jsonPath = undefined

getGenotypeDataData :: (MonadSafe m) => PoseidonModule -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
getGenotypeDataData pm = undefined

getCombinedGenotypeData :: (MonadSafe m) => [PoseidonModule] -> IndSelection -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
getCombinedGenotypeData pms indSelection = undefined

