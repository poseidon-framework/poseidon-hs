{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Trident.Rectify (
    runRectify, RectifyOptions (..)
    ) where

import           Poseidon.Core.Contributor     (ContributorSpec (..))
import           Poseidon.Core.EntityTypes     (HasNameAndVersion (..),
                                                PacNameAndVersion (..),
                                                renderNameWithVersion)
import           Poseidon.Core.GenotypeData    (GenotypeDataSpec (..),
                                                GenotypeFileSpec (..))
import           Poseidon.Core.Janno           (makeJannoHeader,
                                                writeJannoFileWithoutEmptyCols)
import           Poseidon.Core.Package         (PackageReadOptions (..),
                                                PoseidonPackage (..),
                                                defaultPackageReadOptions,
                                                readPoseidonPackageCollection,
                                                writePoseidonPackage)
import           Poseidon.Core.PoseidonVersion (PoseidonVersion (..))
import           Poseidon.Core.Utils           (PoseidonIO, getChecksum,
                                                logDebug, logInfo, logWarning)
import           Poseidon.Core.Version         (VersionComponent (..),
                                                updateThreeComponentVersion)
import Poseidon.CLI.Trident.Modify (PackageVersionUpdate (..), ChecksumsToModify (..), updateChecksums, addContributors, completeAndWritePackage)

import           Control.DeepSeq               ((<$!!>))
import           Control.Monad                 (when, filterM)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.List                     (nub)
import           Data.Maybe                    (fromJust)
import           Data.Time                     (UTCTime (..), getCurrentTime)
import           Data.Version                  (Version (..), makeVersion,
                                                showVersion)
import           System.Directory              (doesFileExist, removeFile)
import           System.FilePath               ((</>))

data RectifyOptions = RectifyOptions
    { _rectifyBaseDirs              :: [FilePath]
    , _rectifyIgnorePoseidonVersion :: Bool
    , _rectifyPackageVersionUpdate  :: Maybe PackageVersionUpdate
    , _rectifyNewContributors       :: Maybe [ContributorSpec]
    }

runRectify :: RectifyOptions -> PoseidonIO ()
runRectify (RectifyOptions baseDirs ignorePosVer pacVerUpdate newContributors) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = True
        , _readOptIgnoreGeno       = True
        , _readOptGenoCheck        = False
        , _readOptOnlyLatest       = False
        , _readOptIgnorePosVersion = ignorePosVer
    }
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    logInfo "Find packages that need rectification"
    toRectifyPackages <- filterM needsRectification allPackages
    case toRectifyPackages of
        [] -> do
            logInfo "Nothing to rectify"
        xs -> do
            logInfo "Starting per-package update procedure"
            mapM_ rectifyOnePackage xs
    logInfo "Done"
    where
        rectifyOnePackage :: PoseidonPackage -> PoseidonIO ()
        rectifyOnePackage inPac = do
            logInfo $ "Rectifying package: " ++ renderNameWithVersion inPac
            updatedPackage <- updateChecksums ChecksumAll inPac >>= addContributors newContributors
            completeAndWritePackage pacVerUpdate updatedPackage

needsRectification :: PoseidonPackage -> PoseidonIO Bool
needsRectification = undefined
