module Poseidon.CLI.Chronicle where

import           Poseidon.Chronicle (makeChronicle, readChronicle,
                                     updateChronicle, writeChronicle)
import           Poseidon.Package   (PackageReadOptions (..),
                                     defaultPackageReadOptions,
                                     readPoseidonPackageCollection)
import           Poseidon.Utils     (PoseidonIO)


data ChronicleOptions = ChronicleOptions
    { _chronicleBaseDirs  :: [FilePath]
    , _chronicleOperation :: ChronOperation
    }

data ChronOperation = CreateChron FilePath | UpdateChron FilePath

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums      = True
    , _readOptIgnoreGeno           = True
    , _readOptGenoCheck            = False
    , _readOptIgnorePosVersion     = True
    }

runChronicle :: ChronicleOptions -> PoseidonIO ()
runChronicle (ChronicleOptions baseDirs operation) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    case operation of
        CreateChron outPath -> do
            newChronicle <- makeChronicle outPath allPackages
            writeChronicle outPath newChronicle
        UpdateChron inPath -> do
            newChronicle <- makeChronicle inPath allPackages
            oldChronicle <- readChronicle inPath
            let updatedChronicle = updateChronicle oldChronicle newChronicle
            writeChronicle inPath updatedChronicle
