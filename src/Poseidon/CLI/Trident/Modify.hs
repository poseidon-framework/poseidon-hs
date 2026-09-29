{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Trident.Modify (
    runModify, ModifyOptions (..), PackageVersionUpdate (..), ChecksumsToModify (..),
    updateChecksums, addContributors, completeAndWritePackage
    ) where

import           Poseidon.Core.Contributor      (ContributorSpec (..))
import           Poseidon.Core.EntityTypes      (HasNameAndVersion (..),
                                                 PacNameAndVersion (..),
                                                 renderNameWithVersion)
import           Poseidon.Core.GenotypeData     (GenotypeDataSpec (..),
                                                 GenotypeFileSpec (..),
                                                 loadIndividuals,
                                                 printSNPCopyProgress)
import           Poseidon.Core.Janno            (JannoRow (..), JannoRows (..),
                                                 makeJannoHeader,
                                                 writeJannoFile,
                                                 writeJannoFileWithoutEmptyCols)
import           Poseidon.Core.Package          (PackageReadOptions (..),
                                                 PoseidonException (..),
                                                 PoseidonPackage (..),
                                                 defaultPackageReadOptions,
                                                 getJointGenotypeData,
                                                 readPoseidonPackageCollection,
                                                 writePoseidonPackage)
import           Poseidon.Core.PoseidonVersion  (PoseidonVersion (..))
import           Poseidon.Core.Utils            (PoseidonIO, envErrorLength,
                                                 envLogAction, getChk, logDebug,
                                                 logError, logInfo, logWarning)
import           Poseidon.Core.Version          (VersionComponent (..),
                                                 updateThreeComponentVersion)

import           Control.DeepSeq                ((<$!!>))
import           Control.Exception              (catch, throwIO)
import           Control.Monad                  (when)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.List                      (nub)
import           Data.Maybe                     (fromJust)
import           Data.Time                      (UTCTime (..), getCurrentTime)
import qualified Data.Vector.Unboxed            as VU
import qualified Data.Vector.Unboxed.Mutable    as VUM
import           Data.Version                   (Version (..), makeVersion,
                                                 showVersion)
import           Pipes                          ((>->))
import qualified Pipes.Prelude                  as P
import           Pipes.Safe                     (runSafeT)
import           Poseidon.CLI.Trident.Forge     (sumNonMissingSNPs)
import           Poseidon.Core.ColumnTypesJanno (JannoNrSNPs (..))
import           System.Directory               (doesFileExist, removeFile)
import           System.Exit                    (exitFailure)
import           System.FilePath                ((</>))

data ModifyOptions = ModifyOptions
    { _modifyBaseDirs              :: [FilePath]
    , _modifyIgnorePoseidonVersion :: Bool
    , _modifyPoseidonVersion       :: Maybe Version
    , _modifyPackageVersionUpdate  :: Maybe PackageVersionUpdate
    , _modifyChecksums             :: ChecksumsToModify
    , _modifyNewContributors       :: Maybe [ContributorSpec]
    , _modifyJannoRemoveEmptyCols  :: Bool
    , _modifyUpdateNrSNPs          :: Bool
    , _modifyOnlyLatest            :: Bool
    , _modifyForce                 :: Bool
    }

data PackageVersionUpdate = PackageVersionUpdate
    { _pacVerUpVersionComponent :: VersionComponent
    , _pacVerUpLog              :: Maybe String
    }

data ChecksumsToModify =
    ChecksumNone |
    ChecksumAll |
    ChecksumsDetail
    { _modifyChecksumGeno  :: Bool
    , _modifyChecksumJanno :: Bool
    , _modifyChecksumSSF   :: Bool
    , _modifyChecksumBib   :: Bool
    }

runModify :: ModifyOptions -> PoseidonIO ()
runModify (ModifyOptions
                baseDirs
                ignorePosVer newPosVer pacVerUpdate checksumUpdate newContributors
                jannoRemoveEmptyCols
                updateNrSNPs
                onlyLatest force
           ) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = True
        , _readOptIgnoreGeno       = True
        , _readOptGenoCheck        = False
        , _readOptOnlyLatest       = onlyLatest
    }
    allPackages <- readPoseidonPackageCollection
        pacReadOpts {_readOptIgnorePosVersion = ignorePosVer}
        baseDirs
    case allPackages of
        []  -> do
            logWarning "No packages found."
            logInfo "Done"
        [x] -> do
            modifyOnePackage x
            logInfo "Done"
        xs  -> if force
               then do
                   mapM_ modifyOnePackage allPackages
                   logInfo "Done"
               else do
                   logError $ show (length xs) ++
                              " packages selected for modification.\
                              \ Run modify with --force if you really want to edit\
                              \ all of them."
                   liftIO exitFailure
    where
        modifyOnePackage :: PoseidonPackage -> PoseidonIO ()
        modifyOnePackage inPac = do
            logInfo $ "Modifying package: " ++ renderNameWithVersion inPac
            -- counting SNPs for .janno column Nr_SNPs
            let inJannoRows = getJannoRows $ posPacJanno inPac
            updatedJanno <- if updateNrSNPs
                            then do
                                nrSNPs <- countSNPs inPac
                                nrSNPsFrozen <- liftIO $ VU.freeze nrSNPs
                                let updatedRows = zipWith
                                        (\x y -> x {jNrSNPs = Just (JannoNrSNPs y)})
                                        inJannoRows (VU.toList nrSNPsFrozen)
                                return $ JannoRows updatedRows
                            else return $ JannoRows inJannoRows
            when (updateNrSNPs || jannoRemoveEmptyCols) $
                case posPacJannoFile inPac of
                        Nothing -> logError "No .janno file to modify"
                        Just jannoPath -> do
                            logInfo "Writing .janno file"
                            if jannoRemoveEmptyCols
                            then do
                                logInfo "Reordering and removing empty .janno columns"
                                liftIO $ writeJannoFileWithoutEmptyCols
                                             (posPacBaseDir inPac </> jannoPath)
                                             (makeJannoHeader updatedJanno)
                                             updatedJanno
                            else do
                                liftIO $ writeJannoFile
                                             (posPacBaseDir inPac </> jannoPath)
                                             (makeJannoHeader updatedJanno)
                                             updatedJanno
            updatedPacPosVer <- updatePoseidonVersion newPosVer inPac
            updatedPacContri <- addContributors newContributors updatedPacPosVer
            updatedPacChecksums <- updateChecksums checksumUpdate updatedPacContri
            completeAndWritePackage pacVerUpdate updatedPacChecksums

countSNPs :: PoseidonPackage -> PoseidonIO (VUM.IOVector Int)
countSNPs pac = do
    logInfo "Counting SNPs..."
    inds <- loadIndividuals (posPacBaseDir pac) (posPacGenotypeData pac)
    logA <- envLogAction
    currentTime <- liftIO getCurrentTime
    errLength <- envErrorLength
    newNrSNPs <- liftIO $ catch (
        runSafeT $ do
            eigenstratProd <- getJointGenotypeData logA False False False [pac] Nothing
            let forgePipe = eigenstratProd >-> printSNPCopyProgress logA currentTime
            let startAcc = liftIO $ VUM.replicate (length inds) 0
            P.foldM sumNonMissingSNPs startAcc return forgePipe
        ) (throwIO . PoseidonGenotypeExceptionForward errLength)
    logInfo "Done"
    return newNrSNPs

updatePoseidonVersion :: Maybe Version -> PoseidonPackage -> PoseidonIO PoseidonPackage
updatePoseidonVersion Nothing    pac = return pac
updatePoseidonVersion (Just ver) pac = do
    logDebug "Updating Poseidon version"
    return pac { posPacPoseidonVersion = PoseidonVersion ver }

addContributors :: Maybe [ContributorSpec] -> PoseidonPackage -> PoseidonIO PoseidonPackage
addContributors Nothing pac = return pac
addContributors (Just cs) pac = do
    logDebug "Updating list of contributors"
    return pac { posPacContributor = nub (posPacContributor pac ++ cs) }

updateChecksums :: ChecksumsToModify -> PoseidonPackage -> PoseidonIO PoseidonPackage
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
                                sequence [testAndGetChecksum (d </> f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
                            return $ GenotypeEigenstrat gf genoChkSum sf snpChkSum if_ indChkSum
                        GenotypePlink gf gfc sf sfc if_ ifc -> do
                            [genoChkSum, snpChkSum, indChkSum] <-
                                sequence [testAndGetChecksum (d </> f) c | (f, c) <- zip [gf, sf, if_] [gfc, sfc, ifc]]
                            return $ GenotypePlink gf genoChkSum sf snpChkSum if_ indChkSum
                        GenotypeVCF gf gfc -> do
                            genoChkSum <- testAndGetChecksum (d </> gf) gfc
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
        testAndGetChecksum :: (MonadIO m) => FilePath -> Maybe String -> m (Maybe String)
        testAndGetChecksum file defaultChkSum = do
            e <- liftIO . doesFileExist $ file
            if e then Just <$!!> getChk file else return defaultChkSum


completeAndWritePackage :: Maybe PackageVersionUpdate -> PoseidonPackage -> PoseidonIO ()
completeAndWritePackage Nothing pac = do
    logDebug "Writing modified POSEIDON.yml file"
    liftIO $ writePoseidonPackage pac
completeAndWritePackage (Just (PackageVersionUpdate component logText)) pac = do
    updatedPacPacVer <- updatePackageVersion component pac
    updatePacChangeLog <- writeOrUpdateChangelogFile logText updatedPacPacVer
    logDebug "Writing modified POSEIDON.yml file"
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
