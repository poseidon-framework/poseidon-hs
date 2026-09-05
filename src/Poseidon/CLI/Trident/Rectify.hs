{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Trident.Rectify (
    runRectify, RectifyOptions (..)
    ) where

import           Poseidon.CLI.Trident.Modify (ChecksumsToModify (..),
                                              PackageVersionUpdate (..),
                                              addContributors,
                                              completeAndWritePackage,
                                              updateChecksums)
import           Poseidon.Core.Contributor   (ContributorSpec (..))
import           Poseidon.Core.EntityTypes   (renderNameWithVersion)
import           Poseidon.Core.GenotypeData  (GenotypeDataSpec (..),
                                              GenotypeFileSpec (..))
import           Poseidon.Core.Package       (PackageReadOptions (..),
                                              PoseidonPackage (..),
                                              defaultPackageReadOptions,
                                              readPoseidonPackageCollection)
import           Poseidon.Core.Utils         (PoseidonIO, getChk, logInfo)

import           Control.Monad               (filterM)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           System.Directory            (doesFileExist)
import           System.FilePath             ((</>))

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
        [] -> logInfo "No packages need rectification"
        xs -> do
            logInfo $ show (length xs) ++ " packages need rectification"
            mapM_ rectifyOnePackage xs
    logInfo "Done"
    where
        rectifyOnePackage :: PoseidonPackage -> PoseidonIO ()
        rectifyOnePackage pac = do
            logInfo $ "Rectifying package: " ++ renderNameWithVersion pac
            pure pac >>=
              updateChecksums ChecksumAll >>=
              addContributors newContributors >>=
              completeAndWritePackage pacVerUpdate

needsRectification :: PoseidonPackage -> PoseidonIO Bool
needsRectification pac = do
    let d = posPacBaseDir pac
        gFileSpec = genotypeFileSpec . posPacGenotypeData $ pac
    goodGeno <- case gFileSpec of
        GenotypeEigenstrat gf gfc sf sfc if_ ifc ->
            and <$> sequence [goodChecksum d (Just f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
        GenotypePlink gf gfc sf sfc if_ ifc ->
            and <$> sequence [goodChecksum d (Just f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
        GenotypeVCF gf gfc -> goodChecksum d (Just gf) gfc
    goodJanno <- goodChecksum d (posPacJannoFile pac) (posPacJannoFileChkSum pac)
    goodSeqSo <- goodChecksum d (posPacSeqSourceFile pac) (posPacSeqSourceFileChkSum pac)
    goodBib   <- goodChecksum d (posPacBibFile pac) (posPacBibFileChkSum pac)
    let needsRect = not $ and [goodGeno, goodJanno, goodSeqSo, goodBib]
    logInfo $ (if needsRect then "CHANGED " else "OK      ") ++ renderNameWithVersion pac
    return needsRect

goodChecksum :: (MonadIO m) => FilePath -> Maybe FilePath -> Maybe String -> m Bool
goodChecksum _ Nothing _ = return True
goodChecksum _ _ Nothing = return True
goodChecksum baseDir (Just file) (Just expectedCheckSum) = do
    let f = baseDir </> file
    exists <- liftIO . doesFileExist $ f
    if exists
    then do
        realChecksum <- getChk f
        return $ realChecksum == expectedCheckSum
    else return True
