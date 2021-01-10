module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..),
    ) where

import           Poseidon.Package           (loadPoseidonPackagesForChecksumUpdate,
                                             updateChecksumsInPackage)

import           Data.Yaml.Pretty.Extras    (encodeFilePretty)
import           System.IO                  (hPutStrLn, stderr)

data UpdateOptions = UpdateOptions
    { _jaBaseDirs :: [FilePath]
    }

runUpdate :: UpdateOptions -> IO ()
runUpdate (UpdateOptions baseDirs) = do
    packages <- loadPoseidonPackagesForChecksumUpdate baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    putStrLn "Updating checksums in the packages"
    updatedPackages <- mapM (updateChecksumsInPackage . snd) packages
    putStrLn "Writing modified POSEIDON.yml files"
    mapM_ (uncurry encodeFilePretty) $ zip (map fst packages) updatedPackages
