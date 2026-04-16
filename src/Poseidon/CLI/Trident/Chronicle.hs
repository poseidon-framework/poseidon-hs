module Poseidon.CLI.Trident.Chronicle where

import           Poseidon.Core.Chronicle (makeChronicle, readChronicle,
                                     updateChronicle, writeChronicle)
import           Poseidon.Core.Package   (PackageReadOptions (..),
                                     defaultPackageReadOptions,
                                     readPoseidonPackageCollection)
import           Poseidon.Core.Utils     (PoseidonIO)


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
