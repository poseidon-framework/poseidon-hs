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
                                                logDebug, logInfo, logWarning, getChk)
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
    , _rectifyPackageVersionUpdate  :: Maybe PackageVersionUpdate
    , _rectifyNewContributors       :: Maybe [ContributorSpec]
    }

runRectify :: RectifyOptions -> PoseidonIO ()
runRectify (RectifyOptions baseDirs pacVerUpdate newContributors) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = True
        , _readOptIgnoreGeno       = True
        , _readOptGenoCheck        = False
        , _readOptOnlyLatest       = False
        , _readOptIgnorePosVersion = True
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
needsRectification pac = do
    let d = posPacBaseDir pac
    let gFileSpec = genotypeFileSpec . posPacGenotypeData $ pac
    chkGeno <- do
        logDebug "Checking genotype data checksums"
        case gFileSpec of
            GenotypeEigenstrat gf gfc sf sfc if_ ifc -> do
                and <$> sequence [checkChecksum d (Just f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
            GenotypePlink gf gfc sf sfc if_ ifc -> do
                and <$> sequence [checkChecksum d (Just f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
            GenotypeVCF gf gfc -> do
                checkChecksum d (Just gf) gfc
    chkJanno <- do
        logDebug "Checking .janno file checksum"
        checkChecksum d (posPacJannoFile pac) (posPacJannoFileChkSum pac)
    chkSeqSource <- do
        logDebug "Checking .ssf file checksum"
        checkChecksum d (posPacSeqSourceFile pac) (posPacSeqSourceFileChkSum pac)
    chkBib <- do
        logDebug "Checking .bib file checksum"
        checkChecksum d (posPacBibFile pac) (posPacBibFileChkSum pac)
    return $ and [chkGeno, chkJanno, chkSeqSource, chkBib]

checkChecksum :: (MonadIO m) => FilePath -> Maybe FilePath -> Maybe String -> m Bool
checkChecksum _ Nothing _ = return True
checkChecksum _ _ Nothing = return True
checkChecksum baseDir (Just file) (Just expectedCheckSum) = do
    let f = baseDir </> file
    exists <- liftIO . doesFileExist $ f
    if exists
    then do
        realChecksum <- getChk f
        return $ realChecksum == expectedCheckSum
    else return True
