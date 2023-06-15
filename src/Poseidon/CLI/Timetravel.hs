module Poseidon.CLI.Timetravel where

import           Poseidon.Chronicle      (PackageIteration (..),
                                          PoseidonPackageChronicle (..),
                                          chroniclePackages, readChronicle)
import           Poseidon.Package        (PackageReadOptions (..),
                                          defaultPackageReadOptions,
                                          readPoseidonPackageCollection)
import           Poseidon.SecondaryTypes (makeNameWithVersion)
import           Poseidon.Utils          (PoseidonException (..), PoseidonIO,
                                          logDebug, logInfo)

import           Control.Monad           (forM_)
import           Control.Monad.Catch     (throwM)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Set                as S
import           GitHash                 (getGitInfo, giHash)
import           System.Directory        (copyFile, createDirectoryIfMissing,
                                          listDirectory)
import           System.FilePath         ((</>))
import           System.Process          (callCommand)

data TimetravelOptions = TimetravelOptions
    { _timetravelBaseDirs      :: [FilePath]
    , _timetravelSourceDir     :: FilePath
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
runTimetravel (TimetravelOptions baseDirs srcDir chroniclePath) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    pacsInBaseDirs <- chroniclePackages True allPackages
    chronicle <- readChronicle chroniclePath
    let pacsInChronicle = snapYamlPackages chronicle
    case S.toList $ S.difference pacsInChronicle pacsInBaseDirs of
        []             -> do logInfo "All packages already there, nothing to add"
        pacStatesToAdd -> do
            eitherGit <- liftIO $ getGitInfo srcDir
            case eitherGit of
                Left _ -> do
                    throwM $ PoseidonChronicleException $ "Did not find .git directory in " ++ show srcDir
                Right info -> do
                    let startCommit = giHash info
                    mapM_ (recoverPacIter startCommit (head baseDirs)) pacStatesToAdd
    where
        recoverPacIter :: String -> FilePath -> PackageIteration -> PoseidonIO ()
        recoverPacIter startCommit destDir pacIter@(PackageIteration _ _ commit path) = do
            let pacIterName = makeNameWithVersion pacIter
            logInfo $ "Recovering package " ++ pacIterName
            if startCommit == commit
            then do
                logInfo $ "Already at the right commit " ++ commit ++ " in " ++ srcDir
                copyDirectory (srcDir </> path) (destDir </> pacIterName)
            else do
                gitCheckout srcDir commit
                copyDirectory (srcDir </> path) (destDir </> pacIterName)
                gitCheckout srcDir startCommit
                -- Depending on the state of the chronicle file, the srcDir and the destDir,
                -- it could be more efficient not to switch back...

gitCheckout :: FilePath -> String -> PoseidonIO ()
gitCheckout srcDir commit = do
    logInfo $ "Checking out commit " ++ commit ++ " in " ++ srcDir
    liftIO $ callCommand ("git -C " ++ srcDir ++ " checkout " ++ commit ++ " --quiet")
    -- Instead of this nasty system call and changing the world with the checkout
    -- we could do something like this:
    -- https://hackage.haskell.org/package/git-0.3.0/docs/Data-Git-Monad.html#v:withCommit
    -- Unfortunately this library is not maintained any more.
    -- And I'm also not entirely sure how git lfs integrates with that...

copyDirectory :: FilePath -> FilePath -> PoseidonIO ()
copyDirectory srcDir destDir = do
  logInfo $ "Copying dir " ++ srcDir ++ " to " ++ destDir
  liftIO $ createDirectoryIfMissing True destDir
  files <- liftIO $ listDirectory srcDir
  forM_ files $ \file -> do
    let srcFile = srcDir </> file
        destFile = destDir </> file
    logDebug $ "Copying: " ++ srcFile ++ " -> " ++ destFile
    liftIO $ copyFile srcFile destFile
