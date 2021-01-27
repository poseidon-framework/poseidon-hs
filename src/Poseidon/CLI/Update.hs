module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..),
    ) where

import           Poseidon.Package           (PoseidonPackageMeta (..),
                                             loadPoseidonPackages,
                                             updateChecksumsInPackageMeta)

import           Data.Yaml.Pretty.Extras    (encodeFilePretty)
import           System.IO                  (hPutStrLn, stderr)

data UpdateOptions = UpdateOptions
    { _jaBaseDirs :: [FilePath]
    }

runUpdate :: UpdateOptions -> IO ()
runUpdate (UpdateOptions baseDirs) = do
    allMetaPackages <- loadPoseidonPackages baseDirs True
    hPutStrLn stderr $ (show . length $ allMetaPackages) ++ " Poseidon packages found"
    putStrLn "Updating checksums in the packages"
    updatedPackagesMeta <- mapM updateChecksumsInPackageMeta allMetaPackages
    putStrLn "Writing modified POSEIDON.yml files"
    mapM_ (uncurry encodeFilePretty) $ zip (map posPacPath updatedPackagesMeta) (map posPac updatedPackagesMeta)
