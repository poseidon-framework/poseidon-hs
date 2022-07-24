{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..)
    ) where

import           Poseidon.GenotypeData   (GenotypeDataSpec (..))
import           Poseidon.Package        (PackageReadOptions (..),
                                          PoseidonPackage (..),
                                          defaultPackageReadOptions,
                                          readPoseidonPackageCollection,
                                          writePoseidonPackage)
import           Poseidon.SecondaryTypes (ContributorSpec (..),
                                          VersionComponent (..))
import           Poseidon.Utils          (PoseidonLogIO, getChecksum, logInfo,
                                          logWarning)

import           Control.Monad.IO.Class  (liftIO)
import           Data.List               (nub)
import           Data.Maybe              (fromJust, fromMaybe, isNothing)
import           Data.Time               (Day, UTCTime (..), getCurrentTime)
import           Data.Version            (Version (..), makeVersion,
                                          showVersion)
import           System.Directory        (doesFileExist, removeFile)
import           System.FilePath         ((</>))

data UpdateOptions = UpdateOptions
    { _updateBaseDirs              :: [FilePath]
    , _updatePoseidonVersion       :: Maybe Version
    , _updateIgnorePoseidonVersion :: Bool
    , _updateVersionUpdate         :: VersionComponent
    , _updateNoChecksumUpdate      :: Bool
    , _updateIgnoreGeno            :: Bool
    , _updateNewContributors       :: [ContributorSpec]
    , _updateLog                   :: String
    , _updateForce                 :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

runUpdate :: UpdateOptions -> PoseidonLogIO ()
runUpdate (UpdateOptions baseDirs poseidonVersion ignorePoseidonVersion versionComponent noChecksumUpdate ignoreGeno newContributors logText force) = do
    allPackages <- readPoseidonPackageCollection
        pacReadOpts {_readOptIgnorePosVersion = ignorePoseidonVersion}
        baseDirs
    -- updating poseidon version
    let updatedPacsPoseidonVersion = if isNothing poseidonVersion
        then allPackages
        else map (updatePoseidonVersion poseidonVersion) allPackages
    -- updating checksums
    logInfo "Calculating checksums"
    updatedPacsChecksums <-
        if noChecksumUpdate
        then return updatedPacsPoseidonVersion
        else liftIO $ mapM (updateChecksums ignoreGeno) updatedPacsPoseidonVersion
    -- see which packages were changed and need to be updated formally
    let updatedPacsChanged = if force
        then updatedPacsChecksums
        else map fst $ filter (uncurry (/=)) $ zip updatedPacsChecksums allPackages
    if null updatedPacsChanged
    then logWarning "No packages changed"
    else do
        -- update yml files
        (UTCTime today _) <- liftIO $ getCurrentTime
        let updatedPacsMeta = map (updateMeta versionComponent today newContributors) updatedPacsChanged
        -- write/update CHANGELOG files
        logInfo "Updating CHANGELOG files"
        updatedPacsWithChangelog <- liftIO $ mapM (writeOrUpdateChangelogFile logText) updatedPacsMeta
        -- write yml files with all changes
        logInfo "Writing modified POSEIDON.yml files"
        liftIO $ mapM_ writePoseidonPackage updatedPacsWithChangelog

writeOrUpdateChangelogFile :: String -> PoseidonPackage -> IO PoseidonPackage
writeOrUpdateChangelogFile logText pac = do
    case posPacChangelogFile pac of
        Nothing -> do
            writeFile (posPacBaseDir pac </> "CHANGELOG.md") $
                "V " ++ showVersion (fromJust $ posPacPackageVersion pac) ++ ": " ++ logText ++ "\n"
            return pac {
                posPacChangelogFile = Just "CHANGELOG.md"
            }
        Just x -> do
            changelogFile <- readFile (posPacBaseDir pac </> x)
            removeFile (posPacBaseDir pac </> x)
            writeFile (posPacBaseDir pac </> x) $
                "V " ++ showVersion (fromJust $ posPacPackageVersion pac) ++ ": " ++ logText ++ "\n" ++ changelogFile
            return pac

updateMeta :: VersionComponent -> Day -> [ContributorSpec] -> PoseidonPackage -> PoseidonPackage
updateMeta versionComponent date newContributors pac =
    pac { posPacPackageVersion =
        maybe (Just $ makeVersion [0, 1, 0])
            (Just . makeVersion . updateVersionInt versionComponent . versionBranch)
            (posPacPackageVersion pac)
        , posPacLastModified = Just date
        , posPacContributor = nub $ posPacContributor pac ++ newContributors
    }
    where
        updateVersionInt :: VersionComponent -> [Int] -> [Int]
        updateVersionInt component v =
            case component of
                Patch -> [v !! 0, v !! 1, (v !! 2) + 1]
                Minor -> [v !! 0, (v !! 1) + 1, 0]
                Major -> [(v !! 0) + 1, 0, 0]

updateChecksums :: Bool -> PoseidonPackage -> IO PoseidonPackage
updateChecksums ignoreGeno pac = do
    let d = posPacBaseDir pac
    jannoChkSum <- case posPacJannoFile pac of
        Nothing -> return $ posPacJannoFileChkSum pac
        Just fn -> Just <$> getChecksum (d </> fn)
    bibChkSum <- case posPacBibFile pac of
        Nothing -> return $ posPacBibFileChkSum pac
        Just fn -> Just <$> getChecksum (d </> fn)
    let newPac1 = pac {
            posPacJannoFileChkSum = jannoChkSum,
            posPacBibFileChkSum = bibChkSum
        }
    if ignoreGeno
    then return newPac1
    else do
        let gd = posPacGenotypeData newPac1
        genoExists <- doesFileExist (d </> genoFile gd)
        genoChkSum <- if genoExists
                    then Just <$> getChecksum (d </> genoFile gd)
                    else return $ genoFileChkSum gd
        snpExists <-  doesFileExist (d </> snpFile gd)
        snpChkSum <-  if snpExists
                    then Just <$> getChecksum (d </> snpFile gd)
                    else return $ snpFileChkSum gd
        indExists <-  doesFileExist (d </> indFile gd)
        indChkSum <-  if indExists
                    then Just <$> getChecksum (d </> indFile gd)
                    else return $ indFileChkSum gd
        return $ newPac1 {
            posPacGenotypeData = gd {
                genoFileChkSum = genoChkSum,
                snpFileChkSum = snpChkSum,
                indFileChkSum = indChkSum
            }
        }

updatePoseidonVersion :: Maybe Version -> PoseidonPackage -> PoseidonPackage
updatePoseidonVersion maybeNewPoseidonVersion pac = pac {
        posPacPoseidonVersion = fromMaybe (posPacPoseidonVersion pac) maybeNewPoseidonVersion
    }
