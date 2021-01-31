{-# LANGUAGE DeriveGeneric #-}

module Poseidon.CLI.Validate where

import           Poseidon.BibFile           (readBibTeXFile)
import           Poseidon.Janno             (PoseidonSample (..), readJannoFile)
import           Poseidon.Package           (PoseidonPackage (..),
                                             getIndividuals,
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..),
                                             renderPoseidonException)

import           Control.Exception          (try)
import           Control.Exception          (throw)
import           Control.Monad              (forM, unless, when)
import qualified Data.Either                as E
import           Data.List                  (intercalate, nub, (\\))
import           Data.Maybe                 (mapMaybe)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..))
import           System.IO                  (hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data ValidateOptions = ValidateOptions
    { _jaBaseDirs :: [FilePath]
    }

u :: String -> String
u x = "\x1b[4m" ++ x ++ "\x1b[0m"

-- TODO: Modularize validator!

-- | The main function running the janno command
runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDirs) = do
    -- POSEIDON.yml
    putStrLn $ u "POSEIDON.yml file consistency:"
    --allPackages <- readPoseidonPackageCollection False baseDirs

