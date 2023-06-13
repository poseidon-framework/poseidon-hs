module Poseidon.CLI.Timetravel where

import           Poseidon.Utils   (PoseidonIO, logInfo)
import           Poseidon.Package (PackageReadOptions (..),
                                   defaultPackageReadOptions,
                                   readPoseidonPackageCollection)
import Poseidon.Chronicle (readChronicle, PoseidonPackageChronicle (..), chroniclePackages)

import Data.Set as S

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
    pacsInBaseDirs <- chroniclePackages True allPackages

    chronicle <- readChronicle chroniclePath
    let pacsInChronicle = snapYamlPackages chronicle

    logInfo $ show (S.difference pacsInChronicle pacsInBaseDirs)



    -- That would be exactly the logic we need:
    -- https://hackage.haskell.org/package/git-0.3.0/docs/Data-Git-Monad.html#v:withCommit
    -- Unfortunately the git library is not maintained any more.
