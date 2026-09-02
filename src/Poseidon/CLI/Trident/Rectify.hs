{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Trident.Rectify (
    runRectify, RectifyOptions (..)
    ) where

import           Poseidon.CLI.Trident.Modify   (ChecksumsToModify (..),
                                                PackageVersionUpdate (..),
                                                addContributors,
                                                completeAndWritePackage,
                                                updateChecksums)
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
import           Poseidon.Core.Utils           (PoseidonIO, getChecksum, getChk,
                                                logDebug, logInfo, logWarning)
import           Poseidon.Core.Version         (VersionComponent (..),
                                                updateThreeComponentVersion)

import           Control.DeepSeq               ((<$!!>))
import           Control.Monad                 (filterM, when)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.List                     (nub)
import           Data.Maybe                    (fromJust)
import           Data.Time                     (UTCTime (..), getCurrentTime)
import           Data.Version                  (Version (..), makeVersion,
                                                showVersion)
import           System.Directory              (doesFileExist, removeFile)
import           System.FilePath               ((</>))

data RectifyOptions = RectifyOptions
    { _rectifyBaseDirs             :: [FilePath]
    , _rectifyPackageVersionUpdate :: Maybe PackageVersionUpdate
    , _rectifyNewContributors      :: Maybe [ContributorSpec]
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
    logInfo "Searching packages that need rectification"
    toRectifyPackages <- filterM needsRectification allPackages
    case toRectifyPackages of
        [] -> logInfo "Nothing to rectify"
        xs -> mapM_ rectifyOnePackage xs
    logInfo "Done"
    where
        rectifyOnePackage :: PoseidonPackage -> PoseidonIO ()
        rectifyOnePackage inPac = do
            logInfo $ "Rectifying package: " ++ renderNameWithVersion inPac
            pure inPac >>=
              updateChecksums ChecksumAll >>=
              addContributors newContributors >>=
              completeAndWritePackage pacVerUpdate

needsRectification :: PoseidonPackage -> PoseidonIO Bool
needsRectification pac = do
    let d = posPacBaseDir pac
        gFileSpec = genotypeFileSpec . posPacGenotypeData $ pac
    chkGeno <- case gFileSpec of
        GenotypeEigenstrat gf gfc sf sfc if_ ifc ->
            and <$> sequence [checkChecksum d (Just f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
        GenotypePlink gf gfc sf sfc if_ ifc ->
            and <$> sequence [checkChecksum d (Just f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
        GenotypeVCF gf gfc -> checkChecksum d (Just gf) gfc
    chkJanno <- checkChecksum d (posPacJannoFile pac) (posPacJannoFileChkSum pac)
    chkSeqSo <- checkChecksum d (posPacSeqSourceFile pac) (posPacSeqSourceFileChkSum pac)
    chkBib   <- checkChecksum d (posPacBibFile pac) (posPacBibFileChkSum pac)
    return $ and [chkGeno, chkJanno, chkSeqSo, chkBib]

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
