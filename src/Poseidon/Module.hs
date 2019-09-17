{-# LANGUAGE DeriveGeneric #-}

module Poseidon.Module (
    PoseidonModule(..),
    parsePoseidonModuleBS,
    parsePoseidonModule,
    getGenotypeDataData,
    getCombinedGenotypeData
) where

import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..))

import           Poseidon.MetaData          (PoseidonMetaData)

import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Version               (Version)
import           GHC.Generics

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
    pmBaseDir          :: FilePath,
    pmModuleName       :: Text,
    pmMetaData         :: Maybe MetaData,
    pmNotes            :: Maybe Text,
    pmMaintainer       :: Text,
    pmMaintainerEmail  :: Text,
    pmLastUpdate       :: UTCTime,
    pmVersion          :: Version,
    pmIndividuals      :: [EigenstratIndEntry],
    pmGenotypeDataSpec :: GenotypeDataSpec
}

data GenotypeDataSpec = EigenstratDataSpec FilePath FilePath
                      | PlinkDataSpec FilePath FilePath

data PoseidonException = PoseidonModuleParseException String
instance Exception PoseidonException

data IndSelection = AllIndividuals | SelectionList [SelectionSpec]
data SelectionSpec = SelectedInd String | SelectedPop String

parsePoseidonModuleBS :: (MonadThrow m) => B.ByteString -> m PoseidonModule
parsePoseidonModuleBS = undefined

parsePoseidonModule :: (MonadIO m) => FilePath -> m PoseidonModule
parsePoseidonModule jsonPath = undefined

getGenotypeDataData :: (MonadSafe m) => PoseidonModule -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
getGenotypeDataData pm = undefined

getCombinedGenotypeData :: (MonadSafe m) => [PoseidonModule] -> IndSelection -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
getCombinedGenotypeData pms indSelection = undefined

