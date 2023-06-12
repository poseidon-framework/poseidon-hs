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
