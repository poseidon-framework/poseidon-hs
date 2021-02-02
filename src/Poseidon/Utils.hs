module Poseidon.Utils (
    PoseidonException(..),
    renderPoseidonException
) where

import           Control.Exception          (Exception)
import           Data.Yaml                  (ParseException)

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException = 
    PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors when trying to parse the genotype data
    | PoseidonJannoRowException FilePath Int String -- ^ An exception to represent errors when trying to parse the .janno file
    | PoseidonJannoConsistencyException FilePath String -- ^ An exception to represent within-janno consistency errors
    | PoseidonCrossFileConsistencyException String String -- ^ An exception to represent inconsistencies across multiple files in a package
    | PoseidonFileExistenceException FilePath -- ^ An exception to represent missing files
    | PoseidonFileChecksumException FilePath -- ^ An exception to represent failed checksum tests
    | PoseidonFStatsFormatException String -- ^ An exception type to represent FStat specification errors
    | PoseidonBibTeXException FilePath String -- ^ An exception to represent errors when trying to parse the .bib file
    | PoseidonForgeEntityParsingException String -- ^ An exception to indicate failed entity parsing
    | PoseidonNewPackageConstructionException String -- ^ An exception to indicate an issue in newPackageTemplate
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
renderPoseidonException (PoseidonForgeEntityParsingException s) =
    "Error when parsing the forge selection: " ++ s
renderPoseidonException (PoseidonNewPackageConstructionException s) =
    show s