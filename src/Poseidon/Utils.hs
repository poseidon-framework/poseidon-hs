module Poseidon.Utils (PoseidonException(..)) where

import           Control.Exception          (Exception)
import           Data.Yaml                  (ParseException)

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException = PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors when trying to parse the genotype data.
    deriving (Show)

-- | Making PoseidonExcepton a Haskell Exception.
instance Exception PoseidonException
