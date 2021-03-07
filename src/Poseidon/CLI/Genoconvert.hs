module Poseidon.CLI.Genoconvert where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..))
import           Poseidon.Package           (findAllPoseidonYmlFiles,
                                             readPoseidonPackageCollection,
                                             PoseidonPackage (..))

import           System.IO                  (hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _baseDirs :: [FilePath]
    , _outFormat :: GenotypeFormatSpec
    }

runGenoconvert :: GenoconvertOptions -> IO ()
runGenoconvert (GenoconvertOptions baseDirs outFormat) = do
    hPutStrLn stderr "test"

