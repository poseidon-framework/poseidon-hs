module Poseidon.Utils (
    PoseidonException(..), 
    renderPoseidonJannoException
) where

import           Control.Exception          (Exception)
import           Data.Yaml                  (ParseException)
import qualified Data.Maybe                 as DM

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException = 
    PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors when trying to parse the genotype data
    | PoseidonJannoException FilePath Int String-- ^ An exception to represent errors when trying to parse the .janno file
    | PoseidonFStatsFormatException String -- ^ An exception type to represent FStat specification errors
    | PoseidonBibTeXException FilePath String -- ^ An exception to represent errors when trying to parse the .bib file
    | PoseidonValidationException -- ^ An exception to indicate failed package validation
    deriving (Show)

instance Exception PoseidonException

renderPoseidonJannoException :: PoseidonException -> String
renderPoseidonJannoException (PoseidonJannoException f i s) = 
    "Can't read sample in " ++ f 
    ++ " in line " ++ (show i)
    ++ " due to a .janno parsing error"
    -- ++ s -- this error message is pretty useless and can be omitted
