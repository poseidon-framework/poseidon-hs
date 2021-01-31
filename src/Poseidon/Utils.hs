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
    | PoseidonFileExistenceException FilePath -- ^ An exception to represent missing files
    | PoseidonFileChecksumException FilePath -- ^ An exception to represent failed checksum tests
    | PoseidonFStatsFormatException String -- ^ An exception type to represent FStat specification errors
    | PoseidonBibTeXException FilePath String -- ^ An exception to represent errors when trying to parse the .bib file
    | PoseidonValidationException String -- ^ An exception to indicate failed package validation
    | PoseidonForgeEntityParsingException String -- ^ An exception to indicate failed entity parsing
    | PoseidonGenotypeFormatParsingException String -- ^ An exception to indicate wrong user input in the Genotype format option
    | PoseidonNewPackageConstructionException String -- ^ An exception to indicate an issue in newPackageTemplate
    deriving (Show)

instance Exception PoseidonException

renderPoseidonException :: PoseidonException -> String
renderPoseidonException (PoseidonYamlParseException fn e) = "Could not parse YAML file " ++ fn ++ ": " ++ show e
renderPoseidonException (PoseidonPackageException s) = "Encountered a logical error with a poseidon package: " ++ s
renderPoseidonException (PoseidonJannoRowException f i s) = 
    "Can't read sample in " ++ f 
    ++ " in line " ++ (show i)
    ++ ": " ++ s
renderPoseidonException (PoseidonFileExistenceException fn) = "File " ++ fn ++ " does not exist"
renderPoseidonException (PoseidonBibTeXException f s) = "BibTex problem in file " ++ f ++ ": " ++ s
renderPoseidonException (PoseidonJannoConsistencyException f s) = "Consistency issues in " ++ f ++ ": " ++ s
renderPoseidonException (PoseidonFileChecksumException s) = "File checksum test failed: " ++ s
renderPoseidonException e = show e
