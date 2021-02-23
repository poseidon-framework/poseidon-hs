{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Utils (
    PoseidonException(..),
    renderPoseidonException,
    IndividualInfo(..)
) where

import           Control.Exception (Exception)
import           Data.Aeson        (FromJSON, ToJSON, object, parseJSON, toJSON,
                                    withObject, (.:), (.=))
import           Data.Yaml         (ParseException)

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException = PoseidonYamlParseException FilePath ParseException
    | PoseidonPackageException String
    | PoseidonIndSearchException String
    | PoseidonGenotypeException String
    | PoseidonJannoRowException FilePath Int String
    | PoseidonJannoConsistencyException FilePath String
    | PoseidonCrossFileConsistencyException String String
    | PoseidonFileExistenceException FilePath
    | PoseidonFileChecksumException FilePath
    | PoseidonFStatsFormatException String
    | PoseidonBibTeXException FilePath String
    | PoseidonPoseidonEntityParsingException String
    | PoseidonEmptyForgeException
    | PoseidonNewPackageConstructionException String
    | PoseidonRemoteJSONParsingException String
    deriving (Show)

instance Exception PoseidonException

renderPoseidonException :: PoseidonException -> String
renderPoseidonException (PoseidonYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e
renderPoseidonException (PoseidonPackageException s) =
    "Encountered a logical error with a poseidon package: " ++ s
renderPoseidonException (PoseidonIndSearchException s) =
    show s
renderPoseidonException (PoseidonGenotypeException s) =
    "Error in the genotype data: " ++ show s
renderPoseidonException (PoseidonJannoRowException f i s) =
    "Can't read sample in " ++ f ++ " in line " ++ show i ++ ": " ++ s
renderPoseidonException (PoseidonJannoConsistencyException f s) =
    "Consistency issues in .janno file " ++ f ++ ": " ++ s
renderPoseidonException (PoseidonCrossFileConsistencyException p s) =
    "Cross-file consistency issue in package " ++ p ++ ": " ++ s
renderPoseidonException (PoseidonFileExistenceException f) =
    "File " ++ f ++ " does not exist"
renderPoseidonException (PoseidonFileChecksumException f) =
    "File checksum test failed: " ++ f
renderPoseidonException (PoseidonFStatsFormatException s) =
    "Fstat specification error: " ++ s
renderPoseidonException (PoseidonBibTeXException f s) =
    "BibTex problem in file " ++ f ++ ": " ++ s
renderPoseidonException (PoseidonPoseidonEntityParsingException s) =
    "Error when parsing the forge selection: " ++ s
renderPoseidonException PoseidonEmptyForgeException =
    "Nothing to be forged"
renderPoseidonException (PoseidonNewPackageConstructionException s) =
    show s


data IndividualInfo = IndividualInfo
    { indInfoName    :: String
    , indInfoGroup   :: String
    , indInfoPacName :: String
    }

instance ToJSON IndividualInfo where
    toJSON x = object [
        "name" .= indInfoName x,
        "group" .= indInfoGroup x,
        "pacName" .= indInfoPacName x]

instance FromJSON IndividualInfo where
    parseJSON = withObject "IndividualInfo" $ \v -> IndividualInfo
        <$> v .:   "name"
        <*> v .:   "group"
        <*> v .:  "pacName"
