module Poseidon.CLI.Timetravel where

import           Poseidon.Chronicle     (PackageIteration (..),
                                         PoseidonPackageChronicle (..),
                                         chroniclePackages, readChronicle)
import           Poseidon.Package       (PackageReadOptions (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonIO, logInfo)

import           Control.Monad          (filterM, forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Set               as S
import           Data.Version           (showVersion)
import           System.Directory       (copyFile, createDirectoryIfMissing,
                                         doesFileExist, listDirectory)
import           System.FilePath        (takeDirectory, (</>))
import           System.Process         (callCommand)

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

    let pacStatesToAdd = S.difference pacsInChronicle pacsInBaseDirs
    --logInfo $ show pacStatesToAdd

    mapM_ (recoverPacIter (takeDirectory chroniclePath) (head baseDirs)) $ S.toList pacStatesToAdd

recoverPacIter :: FilePath -> FilePath -> PackageIteration -> PoseidonIO ()
recoverPacIter sourceDir destDir (PackageIteration title version commit path) = do
    let pacIterName = title ++ "-" ++ showVersion version
    logInfo $ "Recovering package " ++ pacIterName

    logInfo $ "Checking out commit " ++ commit ++ " in " ++ sourceDir
    liftIO $ callCommand ("git -C " ++ sourceDir ++ " checkout " ++ commit ++ " --quiet")
    -- Instead of this nasty system call we could do something like this:
    -- https://hackage.haskell.org/package/git-0.3.0/docs/Data-Git-Monad.html#v:withCommit
    -- Unfortunately this library is not maintained any more.

    logInfo $ "Copying dir " ++ path ++ " to " ++ destDir
    liftIO $ copyDirectory (sourceDir </> path) (destDir </> pacIterName)

    logInfo $ "Checking out master branch " ++ commit ++ " in " ++ sourceDir
    liftIO $ callCommand ("git -C " ++ sourceDir ++ " checkout master --quiet")
    -- master must of course be changed - it should instead go back to the initial revision

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory srcDir destDir = do
  createDirectoryIfMissing True destDir
  files <- listDirectory srcDir
  forM_ files $ \file -> do
    let srcFile = srcDir </> file
        destFile = destDir </> file
    copyFile srcFile destFile
