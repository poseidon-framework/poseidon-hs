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
    | PoseidonFileExistenceException String -- ^ An exception to represent missing files
    | PoseidonFStatsFormatException String -- ^ An exception type to represent FStat specification errors
    | PoseidonBibTeXException FilePath String -- ^ An exception to represent errors when trying to parse the .bib file
    | PoseidonValidationException String -- ^ An exception to indicate failed package validation
    | PoseidonForgeEntityParsingException String -- ^ An exception to indicate failed entity parsing
    | POseidonGenotypeFormatParsingException String -- ^ An exception to indicate wrong user input in the Genotype format option
    deriving (Show)

instance Exception PoseidonException

renderPoseidonException :: PoseidonException -> String
renderPoseidonException (PoseidonJannoRowException f i s) = 
    "Can't read sample in " ++ f 
    ++ " in line " ++ (show i)
    ++ ": " ++ s
renderPoseidonException (PoseidonFileExistenceException s) = s
renderPoseidonException (PoseidonBibTeXException f s) = "BibText problem in file " ++ f ++ ": " ++ s
renderPoseidonException (PoseidonJannoConsistencyException f s) = "Consistency issues in " ++ f ++ ": " ++ s
renderPoseidonException _ = error "should never happen"
