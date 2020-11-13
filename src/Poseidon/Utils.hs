module Poseidon.Utils (PoseidonException(..), printPoseidonJannoException) where

import           Control.Exception          (Exception)
import           Data.Yaml                  (ParseException)

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException = 
    PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors when trying to parse the genotype data
    | PoseidonJannoException FilePath Int String-- ^ An exception to represent errors when trying to parse the .janno file
    | PoseidonFStatsFormatException String -- ^ An exception type to represent FStat specification errors
    deriving (Show)

instance Exception PoseidonException

printPoseidonJannoException :: PoseidonException -> IO()
printPoseidonJannoException (PoseidonJannoException f i s) = 
    putStrLn $  "Parsing error in " ++ f 
                ++ " in line " ++ (show i) 
                -- ++ ": " ++ s
