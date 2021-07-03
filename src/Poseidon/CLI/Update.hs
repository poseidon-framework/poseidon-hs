module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..)
    ) where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             readPoseidonPackageCollection,
                                             writePoseidonPackage, 
                                             PackageReadOptions (..), 
                                             defaultPackageReadOptions,
                                             getChecksum)
import           Poseidon.SecondaryTypes    (ContributorSpec (..),
                                            VersionComponent (..))

import           Data.Maybe                 (fromMaybe, isNothing, fromJust)
import           Data.Time                  (Day, UTCTime (..), getCurrentTime)
import           Data.Version               (Version (..), makeVersion, showVersion)
import           System.Directory           (doesFileExist, removeFile)
import           System.FilePath            ((</>))
import           System.IO                  (hPutStrLn, stderr)

data UpdateOptions = UpdateOptions
    { _updateBaseDirs :: [FilePath]
    , _updatePoseidonVersion :: Maybe Version
    , _updateVersionUpdate :: VersionComponent
    , _updateNoChecksumUpdate :: Bool
    , _updateIgnoreGeno :: Bool
    , _updateNewContributors :: [ContributorSpec]
    , _updateLog :: String
    , _updateForce :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

runUpdate :: UpdateOptions -> IO ()
runUpdate (UpdateOptions baseDirs poseidonVersion versionComponent noChecksumUpdate ignoreGeno newContributors logText force) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    -- updating poseidon version
    let updatedPacsPoseidonVersion = if isNothing poseidonVersion
        then allPackages
        else map (updatePoseidonVersion poseidonVersion) allPackages
    -- updating checksums
    hPutStrLn stderr "Calculating and updating checksums"
    updatedPacsChecksums <-
        if noChecksumUpdate
        then return updatedPacsPoseidonVersion
        else mapM (updateChecksums ignoreGeno) updatedPacsPoseidonVersion
    -- see which packages were changed and need to be updated formally
    let updatedPacsChanged = if force
        then updatedPacsChecksums
        else map fst $ filter (uncurry (/=)) $ zip updatedPacsChecksums allPackages
    if null updatedPacsChanged
    then hPutStrLn stderr "No packages changed"
    else do
        -- update yml files
        (UTCTime today _) <- getCurrentTime
        let updatedPacsMeta = map (updateMeta versionComponent today newContributors) updatedPacsChanged
        -- write/update CHANGELOG files
        hPutStrLn stderr "Updating CHANGELOG files"
        updatedPacsWithChangelog <- mapM (writeOrUpdateChangelogFile logText) updatedPacsMeta
        -- write yml files with all changes
        hPutStrLn stderr "Writing modified POSEIDON.yml files"
        mapM_ writePoseidonPackage updatedPacsWithChangelog

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
        , posPacContributor = posPacContributor pac ++ newContributors
    }
    where
        updateVersionInt :: VersionComponent -> [Int] -> [Int] 
        updateVersionInt component v = 
            case component of
                Patch -> [v !! 0, v !! 1, (v !! 2) + 1]
                Minor -> [v !! 0, (v !! 1) + 1, v !! 2]
                Major -> [(v !! 0) + 1, v !! 1, v !! 2]

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
        return $ pac {
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