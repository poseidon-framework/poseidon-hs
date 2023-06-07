module Poseidon.CLI.Chronicle where

import           Poseidon.Chronicle (ChronicleMode (..), makeChronicle,
                                     makeMinimalChronicle, readChronicle,
                                     updateChronicle, writeChronicle)
import           Poseidon.Package   (PackageReadOptions (..),
                                     defaultPackageReadOptions,
                                     readPoseidonPackageCollection)
import           Poseidon.Utils     (PoseidonIO)

-- | A datatype representing command line options for the summarise command
data ChronicleOptions = ChronicleOptions
    { _chronicleBaseDirs       :: [FilePath]
    , _chronicleOperation      :: SnapOperation
    , _chronicleWithGitCommits :: Bool
    , _chronicleMinimal        :: Bool
    }

data SnapOperation = CreateSnap FilePath | UpdateSnap FilePath (Maybe FilePath)

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

-- | The main function running the janno command
runChronicle :: ChronicleOptions -> PoseidonIO ()
runChronicle (ChronicleOptions baseDirs operation withGit minimal) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    let modeSetting = if withGit then ChronicleWithGit else SimpleChronicle
    newChronicle <- if minimal
                   then do makeMinimalChronicle modeSetting allPackages
                   else do makeChronicle modeSetting allPackages
    case operation of
        CreateSnap outPath -> writeChronicle outPath newChronicle
        UpdateSnap inPath maybeOutPath -> do
            oldChronicle <- readChronicle inPath
            let updatedChronicle = updateChronicle oldChronicle newChronicle
            case maybeOutPath of
                Nothing      -> writeChronicle inPath updatedChronicle
                Just outPath -> writeChronicle outPath updatedChronicle
