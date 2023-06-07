module Poseidon.CLI.Chronicle where

import           Poseidon.Chronicle (makeChronicle, readChronicle,
                                     updateChronicle, writeChronicle)
import           Poseidon.Package   (PackageReadOptions (..),
                                     defaultPackageReadOptions,
                                     readPoseidonPackageCollection)
import           Poseidon.Utils     (PoseidonIO)

-- | A datatype representing command line options for the summarise command
data ChronicleOptions = ChronicleOptions
    { _chronicleBaseDirs       :: [FilePath]
    , _chronicleOperation      :: SnapOperation
    }

data SnapOperation = CreateSnap FilePath | UpdateSnap FilePath (Maybe FilePath)

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums      = True
    , _readOptIgnoreGeno           = True
    , _readOptGenoCheck            = False
    , _readOptIgnorePosVersion     = True
    , _readOptKeepMultipleVersions = True
    }

-- | The main function running the janno command
runChronicle :: ChronicleOptions -> PoseidonIO ()
runChronicle (ChronicleOptions baseDirs operation) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    newChronicle <- makeChronicle allPackages
    case operation of
        CreateSnap outPath -> writeChronicle outPath newChronicle
        UpdateSnap inPath maybeOutPath -> do
            oldChronicle <- readChronicle inPath
            let updatedChronicle = updateChronicle oldChronicle newChronicle
            case maybeOutPath of
                Nothing      -> writeChronicle inPath updatedChronicle
                Just outPath -> writeChronicle outPath updatedChronicle
