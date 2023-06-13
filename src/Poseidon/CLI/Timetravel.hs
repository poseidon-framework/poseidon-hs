module Poseidon.CLI.Timetravel where

import           Poseidon.Utils   (PoseidonIO, logInfo)
import           Poseidon.Package (PackageReadOptions (..),
                                   defaultPackageReadOptions,
                                   readPoseidonPackageCollection)
import Poseidon.Chronicle (readChronicle)

data TimetravelOptions = TimetravelOptions
    { _timetravelBaseDirs      :: [FilePath]
    , _timetravelChronicleFile :: FilePath
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
runTimetravel (TimetravelOptions baseDirs chroniclePath) = do
    
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    chronicle <- readChronicle chroniclePath

    logInfo "done"



    -- That would be exactly the logic we need:
    -- https://hackage.haskell.org/package/git-0.3.0/docs/Data-Git-Monad.html#v:withCommit
    -- Unfortunately the git library is not maintained any more.
