module Poseidon.CLI.Timetravel where

import           Poseidon.Package (PackageReadOptions (..),
                                   defaultPackageReadOptions)
import           Poseidon.Utils   (PoseidonIO, logInfo)

data TimetravelOptions = TimetravelOptions
    { _timetravelBaseDirs  :: [FilePath]
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums      = True
    , _readOptIgnoreGeno           = True
    , _readOptGenoCheck            = False
    , _readOptIgnorePosVersion     = True
    , _readOptKeepMultipleVersions = True
    }

runTimetravel :: TimetravelOptions -> PoseidonIO ()
runTimetravel (TimetravelOptions baseDirs) = do
    logInfo $ show baseDirs
    -- That would be exactly the logic we need:
    -- https://hackage.haskell.org/package/git-0.3.0/docs/Data-Git-Monad.html#v:withCommit
    -- Unfortunately the git library is not maintained any more.
