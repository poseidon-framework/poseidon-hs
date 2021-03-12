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
data PoseidonException = 
    PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors when trying to parse the genotype data
    | PoseidonJannoRowException FilePath Int String -- ^ An exception to represent errors when trying to parse the .janno file
    | PoseidonJannoConsistencyException FilePath String -- ^ An exception to represent within-janno consistency errors
    | PoseidonCrossFileConsistencyException String String -- ^ An exception to represent inconsistencies across multiple files in a package
    | PoseidonCollectionException String -- ^ An exception to represent logical issues in a poseidon package Collection
    | PoseidonFileExistenceException FilePath -- ^ An exception to represent missing files
    | PoseidonFileChecksumException FilePath -- ^ An exception to represent failed checksum tests
    | PoseidonFStatsFormatException String -- ^ An exception type to represent FStat specification errors
    | PoseidonBibTeXException FilePath String -- ^ An exception to represent errors when trying to parse the .bib file
    | PoseidonPoseidonEntityParsingException String -- ^ An exception to indicate failed entity parsing
    | PoseidonEmptyForgeException -- ^ An exception to throw if there is nothing to be forged
    | PoseidonNewPackageConstructionException String -- ^ An exception to indicate an issue in newPackageTemplate
    | PoseidonRemoteJSONParsingException String -- ^ An exception to indicate failed remote info JSON parsing
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
renderPoseidonException (PoseidonCollectionException s) =
    "The package collection is broken: " ++ s
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