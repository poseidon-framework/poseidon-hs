module Poseidon.CLI.Snapshot where

import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonIO)
import Poseidon.Snapshot (makeSnapshot, SnapshotMode (..), writeSnapshot, makeMinimalSnapshot)

-- | A datatype representing command line options for the summarise command
data SnapshotOptions = SnapshotOptions
    { _snapshotBaseDirs        :: [FilePath]
    , _snapshotOutputDirectory :: FilePath
    , _snapshotWithGitCommits  :: Bool
    , _snapshotMinimal         :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

-- | The main function running the janno command
runSnapshot :: SnapshotOptions -> PoseidonIO ()
runSnapshot (SnapshotOptions baseDirs outDir withGit minimal) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    let modeSetting = if withGit then SnapshotWithGit else SimpleSnapshot
    snapshot <- if minimal
                then do makeMinimalSnapshot modeSetting allPackages
                else do makeSnapshot modeSetting allPackages
    writeSnapshot outDir snapshot

