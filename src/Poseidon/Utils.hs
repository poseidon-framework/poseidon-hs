module Poseidon.Utils (PoseidonException(..)) where

import           Control.Exception          (Exception)
import           Data.Yaml                  (ParseException)

data PoseidonException = PoseidonYamlParseException FilePath ParseException
    | PoseidonPackageException String
    | PoseidonIndSearchException String
    deriving (Show)

instance Exception PoseidonException
