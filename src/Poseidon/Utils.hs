{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Utils (
    PoseidonException (..),
    renderPoseidonException
) where

import           Control.Exception      (Exception)
import           Data.Yaml              (ParseException)

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException = 
    PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonPackageVersionException FilePath String -- ^ An exception to represent an issue with a package version 
    | PoseidonPackageMissingVersionException FilePath -- ^ An exception to indicate a missing poseidonVersion field
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
    | PoseidonForgeEntitiesException String -- ^ An exception to indicate issues in the forge selection
    | PoseidonEmptyForgeException -- ^ An exception to throw if there is nothing to be forged
    | PoseidonNewPackageConstructionException String -- ^ An exception to indicate an issue in newPackageTemplate
    | PoseidonRemoteJSONParsingException String -- ^ An exception to indicate failed remote info JSON parsing
    | PoseidonGenericException String -- ^ A catch-all for any other type of exception
    | PoseidonEmptyOutPacNameException -- ^ An exception to throw if the output package lacks a name
    | PoseidonUnequalBaseDirException FilePath FilePath FilePath -- ^ An exception to throw if genotype data files don't share a common base directory
    deriving (Show)

instance Exception PoseidonException

renderPoseidonException :: PoseidonException -> String
renderPoseidonException (PoseidonYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e
renderPoseidonException (PoseidonPackageException s) =
    "Encountered a logical error with a poseidon package: " ++ s
renderPoseidonException (PoseidonPackageVersionException p s) =
    "Poseidon version mismatch in " ++ show p ++ 
    ". It has version \"" ++ s ++ "\", which is not supported by this trident version."
renderPoseidonException (PoseidonPackageMissingVersionException p) =
    "The POSEIDON.yml file " ++ show p ++ " has no poseidonVersion field. " ++
    "This is mandatory."
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
renderPoseidonException (PoseidonForgeEntitiesException s) =
    "Error in the forge selection: " ++ s
renderPoseidonException PoseidonEmptyForgeException =
    "Nothing to be forged"
renderPoseidonException (PoseidonNewPackageConstructionException s) =
    show s
renderPoseidonException (PoseidonRemoteJSONParsingException s) =
    "Error in parsing JSON: " ++ show s
renderPoseidonException (PoseidonGenericException s) = s
renderPoseidonException PoseidonEmptyOutPacNameException =
    "Error when preparing the new package: The output package does not have a name. Add one with: -n YourPackageName"
renderPoseidonException (PoseidonUnequalBaseDirException g s i) =
    "The base directories of these genotype files are not equal."
    ++ " --genoFile: " ++ g
    ++ " --snpFile: "  ++ s
    ++ " --indFile: "  ++ i