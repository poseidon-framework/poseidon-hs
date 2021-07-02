module Poseidon.CLI.Update (
    runUpdate, UpdateOptions (..),
    ) where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             readPoseidonPackageCollection,
                                             writePoseidonPackage, 
                                             PackageReadOptions (..), 
                                             defaultPackageReadOptions,
                                             ContributorSpec,
                                             getChecksum)

import           Data.Time                  (Day, UTCTime (..), getCurrentTime)
import           Data.Version               (Version (..), makeVersion)
import           System.IO                  (hPutStrLn, stderr)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Maybe (fromMaybe, isNothing)

data UpdateOptions = UpdateOptions
    { _updateBaseDirs :: [FilePath]
    , _updatePoseidonVersion :: Maybe Version
    , _updateVersionUpdate :: VersionComponent
    , _updateChecksumUpdate :: Bool
    , _updateIgnoreGeno :: Bool
    , _updateDate :: Day
    , _updateNewContributors :: [ContributorSpec]
    , _updateLog :: String
    , _updateForce :: Bool
    }

data VersionComponent = Major | Minor | Patch

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

runUpdate :: UpdateOptions -> IO ()
runUpdate (UpdateOptions baseDirs poseidonVersion versionComponent checksumUpdate ignoreGeno date newContributors log force) = do
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    let updatedPacsPoseidonVersion = do
        if isNothing poseidonVersion
        then allPackages
        else map (updatePoseidonVersion poseidonVersion) allPackages
    hPutStrLn stderr "Calculating and updating checksums"
    updatedPacsChecksums <-
        if not checksumUpdate
        then return updatedPacsPoseidonVersion
        else mapM (updateChecksums ignoreGeno) updatedPacsPoseidonVersion
    let updatedPacsChanged = do
        if force
        then updatedPacsChecksums
        else map fst $ filter (uncurry (/=)) $ zip updatedPacsChecksums allPackages 
    if null updatedPacsChanged
    then hPutStrLn stderr "No packages changed"
    else do
        let updatedPacsMeta = map (updateMeta versionComponent date newContributors) updatedPacsChanged
        hPutStrLn stderr "Writing modified POSEIDON.yml files"
        mapM_ writePoseidonPackage updatedPacsMeta

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

updatePoseidonVersion :: Maybe Version -> PoseidonPackage -> PoseidonPackage
updatePoseidonVersion maybeNewPoseidonVersion pac = pac {
        posPacPoseidonVersion = fromMaybe (posPacPoseidonVersion pac) maybeNewPoseidonVersion
    }

-- updatePackageVersion :: Maybe Version -> PoseidonPackage -> PoseidonPackage
-- updatePackageVersion maybeNewPoseidonVersion pac = pac {
--         posPacPoseidonVersion = fromMaybe (posPacPoseidonVersion pac) maybeNewPoseidonVersion
--     }

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