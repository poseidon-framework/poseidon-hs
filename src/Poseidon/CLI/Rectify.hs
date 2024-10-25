{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Rectify (
    runRectify, RectifyOptions (..), PackageVersionUpdate (..), ChecksumsToRectify (..)
    ) where

import           Poseidon.Contributor   (ContributorSpec (..))
import           Poseidon.EntityTypes   (HasNameAndVersion (..),
                                         PacNameAndVersion (..),
                                         renderNameWithVersion)
import           Poseidon.GenotypeData  (GenotypeDataSpec (..),
                                         GenotypeFileSpec (..))
import           Poseidon.Package       (PackageReadOptions (..),
                                         PoseidonPackage (..),
                                         defaultPackageReadOptions,
                                         readPoseidonPackageCollection,
                                         writePoseidonPackage)
import           Poseidon.Utils         (PoseidonIO, getChecksum, logDebug,
                                         logInfo)
import           Poseidon.Version       (VersionComponent (..),
                                         updateThreeComponentVersion)

import           Control.DeepSeq        ((<$!!>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List              (nub)
import           Data.Maybe             (fromJust)
import           Data.Time              (UTCTime (..), getCurrentTime)
import           Data.Version           (Version (..), makeVersion, showVersion)
import           System.Directory       (doesFileExist, removeFile)
import           System.FilePath        ((</>))

data RectifyOptions = RectifyOptions
    { _rectifyBaseDirs              :: [FilePath]
    , _rectifyIgnorePoseidonVersion :: Bool
    , _rectifyPoseidonVersion       :: Maybe Version
    , _rectifyPackageVersionUpdate  :: Maybe PackageVersionUpdate
    , _rectifyChecksums             :: ChecksumsToRectify
    , _rectifyNewContributors       :: Maybe [ContributorSpec]
    , _rectifyOnlyLatest            :: Bool
    }

data PackageVersionUpdate = PackageVersionUpdate
    { _pacVerUpVersionComponent :: VersionComponent
    , _pacVerUpLog              :: Maybe String
    }

data ChecksumsToRectify =
    ChecksumNone |
    ChecksumAll |
    ChecksumsDetail
    { _rectifyChecksumGeno  :: Bool
    , _rectifyChecksumJanno :: Bool
    , _rectifyChecksumSSF   :: Bool
    , _rectifyChecksumBib   :: Bool
    }

runRectify :: RectifyOptions -> PoseidonIO ()
runRectify (RectifyOptions baseDirs ignorePosVer newPosVer pacVerUpdate checksumUpdate newContributors onlyLatest) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = True
        , _readOptIgnoreGeno       = True
        , _readOptGenoCheck        = False
        , _readOptOnlyLatest       = onlyLatest
    }
    allPackages <- readPoseidonPackageCollection
        pacReadOpts {_readOptIgnorePosVersion = ignorePosVer}
        baseDirs
    logInfo "Starting per-package update procedure"
    mapM_ rectifyOnePackage allPackages
    logInfo "Done"
    where
        rectifyOnePackage :: PoseidonPackage -> PoseidonIO ()
        rectifyOnePackage inPac = do
            logInfo $ "Rectifying package: " ++ renderNameWithVersion inPac
            updatedPacPosVer <- updatePoseidonVersion newPosVer inPac
            updatedPacContri <- addContributors newContributors updatedPacPosVer
            updatedPacChecksums <- updateChecksums checksumUpdate updatedPacContri
            completeAndWritePackage pacVerUpdate updatedPacChecksums

updatePoseidonVersion :: Maybe Version -> PoseidonPackage -> PoseidonIO PoseidonPackage
updatePoseidonVersion Nothing    pac = return pac
updatePoseidonVersion (Just ver) pac = do
    logDebug "Updating Poseidon version"
    return pac { posPacPoseidonVersion = ver }

addContributors :: Maybe [ContributorSpec] -> PoseidonPackage -> PoseidonIO PoseidonPackage
addContributors Nothing pac = return pac
addContributors (Just cs) pac = do
    logDebug "Updating list of contributors"
    return pac { posPacContributor = nub (posPacContributor pac ++ cs) }

updateChecksums :: ChecksumsToRectify -> PoseidonPackage -> PoseidonIO PoseidonPackage
updateChecksums checksumSetting pac = do
    case checksumSetting of
        ChecksumNone            -> logDebug "Update no checksums" >> return pac
        ChecksumAll             -> update True True True True
        ChecksumsDetail g j s b -> update g j s b
    where
        update :: Bool -> Bool -> Bool -> Bool -> PoseidonIO PoseidonPackage
        update g j s b = do
            let d = posPacBaseDir pac
            let gFileSpec = genotypeFileSpec . posPacGenotypeData $ pac
            newGenotypeFileSpec <-
                if g
                then do
                    logDebug "Updating genotype data checksums"
                    case gFileSpec of
                        GenotypeEigenstrat gf gfc sf sfc if_ ifc -> do
                            [genoChkSum, snpChkSum, indChkSum] <-
                                sequence [testAndGetChecksum f c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
                            return $ GenotypeEigenstrat gf genoChkSum sf snpChkSum if_ indChkSum
                        GenotypePlink gf gfc sf sfc if_ ifc -> do
                            [genoChkSum, snpChkSum, indChkSum] <-
                                sequence [testAndGetChecksum f c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
                            return $ GenotypePlink gf genoChkSum sf snpChkSum if_ indChkSum
                        GenotypeVCF gf gfc -> do
                            genoChkSum <- testAndGetChecksum gf gfc
                            return $ GenotypeVCF gf genoChkSum
                else return gFileSpec
            newJannoChkSum <-
                if j
                then do
                    logDebug "Updating .janno file checksums"
                    case posPacJannoFile pac of
                        Nothing -> return $ posPacJannoFileChkSum pac
                        Just fn -> Just <$!!> getChk (d </> fn)
                else return $ posPacJannoFileChkSum pac
            newSeqSourceChkSum <-
                if s
                then do
                    logDebug "Updating .ssf file checksums"
                    case posPacSeqSourceFile pac of
                        Nothing -> return $ posPacSeqSourceFileChkSum pac
                        Just fn -> Just <$!!> getChk (d </> fn)
                else return $ posPacSeqSourceFileChkSum pac
            newBibChkSum <-
                if b
                then do
                    logDebug "Updating .bib file checksums"
                    case posPacBibFile pac of
                        Nothing -> return $ posPacBibFileChkSum pac
                        Just fn -> Just <$!!> getChk (d </> fn)
                else return $ posPacBibFileChkSum pac
            let gd = posPacGenotypeData pac
            return $ pac {
                    posPacGenotypeData = gd {genotypeFileSpec = newGenotypeFileSpec},
                    posPacJannoFileChkSum = newJannoChkSum,
                    posPacSeqSourceFileChkSum = newSeqSourceChkSum,
                    posPacBibFileChkSum = newBibChkSum
                }
        getChk :: (MonadIO m) => FilePath -> m String
        getChk = liftIO . getChecksum
        testAndGetChecksum :: (MonadIO m) => FilePath -> Maybe String -> m (Maybe String)
        testAndGetChecksum file defaultChkSum = do
            e <- liftIO . doesFileExist $ file
            if e then Just <$!!> getChk file else return defaultChkSum



completeAndWritePackage :: Maybe PackageVersionUpdate -> PoseidonPackage -> PoseidonIO ()
completeAndWritePackage Nothing pac = do
    logDebug "Writing rectified POSEIDON.yml file"
    liftIO $ writePoseidonPackage pac
completeAndWritePackage (Just (PackageVersionUpdate component logText)) pac = do
    updatedPacPacVer <- updatePackageVersion component pac
    updatePacChangeLog <- writeOrUpdateChangelogFile logText updatedPacPacVer
    logDebug "Writing rectified POSEIDON.yml file"
    liftIO $ writePoseidonPackage updatePacChangeLog

updatePackageVersion :: VersionComponent -> PoseidonPackage -> PoseidonIO PoseidonPackage
updatePackageVersion component pac = do
    logDebug "Updating package version"
    (UTCTime today _) <- liftIO getCurrentTime
    let pacNameAndVer = posPacNameAndVersion pac
    let outPac = pac {
        posPacNameAndVersion = pacNameAndVer {panavVersion = maybe (Just $ makeVersion [0, 1, 0])
                (Just . updateThreeComponentVersion component)
                (getPacVersion pac)
            }
        , posPacLastModified = Just today
        }
    return outPac

writeOrUpdateChangelogFile :: Maybe String -> PoseidonPackage -> PoseidonIO PoseidonPackage
writeOrUpdateChangelogFile Nothing pac = return pac
writeOrUpdateChangelogFile (Just logText) pac = do
    case posPacChangelogFile pac of
        Nothing -> do
            logDebug "Creating CHANGELOG.md"
            liftIO $ writeFile (posPacBaseDir pac </> "CHANGELOG.md") $
                "- V " ++ showVersion (fromJust $ getPacVersion pac) ++ ": " ++
                logText ++ "\n"
            return pac { posPacChangelogFile = Just "CHANGELOG.md" }
        Just x -> do
            logDebug "Updating CHANGELOG.md"
            changelogFile <- liftIO $ readFile (posPacBaseDir pac </> x)
            liftIO $ removeFile (posPacBaseDir pac </> x)
            liftIO $ writeFile (posPacBaseDir pac </> x) $
                "- V " ++ showVersion (fromJust $ getPacVersion pac) ++ ": "
                ++ logText ++ "\n" ++ changelogFile
            return pac
