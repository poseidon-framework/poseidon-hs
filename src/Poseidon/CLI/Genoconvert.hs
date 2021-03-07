module Poseidon.CLI.Genoconvert where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..))
import           Poseidon.Package           (findAllPoseidonYmlFiles,
                                             readPoseidonPackageCollection,
                                             PoseidonPackage (..))

import           System.IO                  (hPutStr, hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _baseDirs :: [FilePath]
    , _outFormat :: GenotypeFormatSpec
    }

runGenoconvert :: GenoconvertOptions -> IO ()
runGenoconvert (GenoconvertOptions baseDirs outFormat) = do
    -- load packages
    allPackages <- readPoseidonPackageCollection True False baseDirs
    -- convert
    mapM_ (convertGenoTo outFormat) allPackages

convertGenoTo :: GenotypeFormatSpec -> PoseidonPackage -> IO ()
convertGenoTo genoFormat pac = do
    hPutStr stderr $
        "Converting genotype data in package "
        ++ posPacTitle pac
        ++ " to format "
        ++ show genoFormat
        
