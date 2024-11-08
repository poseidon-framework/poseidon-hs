{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Genoconvert where

import           Poseidon.EntityTypes       (HasNameAndVersion (..))
import           Poseidon.GenotypeData      (GenoDataSource (..),
                                             GenotypeDataSpec (..),
                                             GenotypeFileSpec (..),
                                             loadGenotypeData,
                                             printSNPCopyProgress)
import           Poseidon.Janno             (jannoRows2EigenstratIndEntries)
import           Poseidon.Package           (PackageReadOptions (..),
                                             PoseidonPackage (..),
                                             defaultPackageReadOptions,
                                             makePseudoPackageFromGenotypeData,
                                             readPoseidonPackageCollection,
                                             writePoseidonPackage)
import           Poseidon.Utils             (PoseidonException (..), PoseidonIO,
                                             envErrorLength, envLogAction,
                                             logInfo, logWarning)

import           Control.Exception          (catch, throwIO)
import           Control.Monad              (unless, when)
import           Data.List                  ((\\))
import           Data.Maybe                 (isJust)
import           Data.Time                  (getCurrentTime)
import           Pipes                      (MonadIO (liftIO), runEffect, (>->))
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat)
import           SequenceFormats.Plink      (PlinkPopNameMode,
                                             eigenstratInd2PlinkFam, writePlink)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, removeFile,
                                             renameFile)
import           System.FilePath            (dropTrailingPathSeparator, (<.>),
                                             (</>))

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _genoconvertGenoSources     :: [GenoDataSource]
    , _genoConvertOutFormat       :: String
    , _genoConvertOutOnlyGeno     :: Bool
    , _genoMaybeOutPackagePath    :: Maybe FilePath
    , _genoconvertRemoveOld       :: Bool
    , _genoconvertOutPlinkPopMode :: PlinkPopNameMode
    , _genoconvertOnlyLatest      :: Bool
    , _genoconvertOutZip          :: Bool
    }

runGenoconvert :: GenoconvertOptions -> PoseidonIO ()
runGenoconvert (GenoconvertOptions genoSources outFormat onlyGeno outPath
                removeOld outPlinkPopMode onlyLatest outZip) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = True
        , _readOptIgnoreGeno       = False
        , _readOptGenoCheck        = True
        , _readOptOnlyLatest       = onlyLatest
    }
    -- load packages
    properPackages <- readPoseidonPackageCollection pacReadOpts $
        [getPacBaseDir x | x@PacBaseDir {} <- genoSources]
    pseudoPackages <- mapM makePseudoPackageFromGenotypeData
        [getGenoDirect x | x@GenoDirect {} <- genoSources]

    logInfo $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    -- convert
    mapM_ (convertGenoTo outFormat onlyGeno outPath removeOld outPlinkPopMode outZip) properPackages
    mapM_ (convertGenoTo outFormat True outPath removeOld outPlinkPopMode outZip) pseudoPackages

convertGenoTo :: String -> Bool -> Maybe FilePath -> Bool ->
    PlinkPopNameMode -> Bool -> PoseidonPackage -> PoseidonIO ()
convertGenoTo outFormat onlyGeno outPath removeOld outPlinkPopMode outZip pac = do
    -- start message
    logInfo $
        "Converting genotype data in "
        ++ show (posPacNameAndVersion pac)
        ++ " to format "
        ++ show outFormat
        ++ if outZip then "(gzipped):" else ":"
    -- compile file names paths
    let outName = getPacName . posPacNameAndVersion $ pac
    (outInd, outSnp, outGeno) <- case (outFormat, outZip) of
            ("EIGENSTRAT", False) -> return
                (outName <.> ".ind", outName <.> ".snp"   , outName <.> ".geno"   )
            ("EIGENSTRAT", True ) -> return
                (outName <.> ".ind", outName <.> ".snp.gz", outName <.> ".geno.gz")
            ("PLINK",      False) -> return
                (outName <.> ".fam", outName <.> ".bim"   , outName <.> ".bed"    )
            ("PLINK",      True ) -> return
                (outName <.> ".fam", outName <.> ".bim.gz", outName <.> ".bed.gz" )
            _                     -> liftIO . throwIO . PoseidonGenericException $
                "Illegal outFormat " ++ outFormat ++
                ". Only Outformats EIGENSTRAT or PLINK are allowed at the moment"
    -- create new genotype data files
    newBaseDir <- case outPath of
        Just x -> do
            -- create new directory
            logInfo $ "Writing to directory (will be created if missing): " ++ x
            liftIO $ createDirectoryIfMissing True (dropTrailingPathSeparator x)
            return x
        Nothing -> return $ posPacBaseDir pac
    let (outG, outS, outI) = (newBaseDir </> outGeno, newBaseDir </> outSnp, newBaseDir </> outInd)
    allExists <- and <$> mapM checkFile [outG, outS, outI]
    if allExists
    then logWarning $ "Package already in desired file-type, skipping genotype conversion for " ++
        show (posPacNameAndVersion pac)
    else do
        logInfo "Processing SNPs..."
        logA <- envLogAction
        currentTime <- liftIO getCurrentTime
        errLength <- envErrorLength
        let eigenstratIndEntries = jannoRows2EigenstratIndEntries . posPacJanno $ pac
        liftIO $ catch (
            runSafeT $ do
                eigenstratProd <- loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac)
                let outConsumer = case outFormat of
                        "EIGENSTRAT" -> writeEigenstrat (outG ++ ".gconvert")
                                                        (outS ++ ".gconvert")
                                                        (outI ++ ".gconvert")
                                                        eigenstratIndEntries
                        "PLINK"      -> writePlink (outG ++ ".gconvert")
                                                   (outS ++ ".gconvert")
                                                   (outI ++ ".gconvert")
                                                   (map (eigenstratInd2PlinkFam outPlinkPopMode) eigenstratIndEntries)
                        _  -> liftIO . throwIO . PoseidonGenericException $
                            "Illegal outFormat " ++ outFormat ++
                            ". Only Outformats EIGENSTRAT or PLINK are allowed at the moment"
                runEffect $ eigenstratProd >-> printSNPCopyProgress logA currentTime >-> outConsumer
            ) (throwIO . PoseidonGenotypeExceptionForward errLength)
        -- the following will just override if the file already exists.
        liftIO $ renameFile (outG ++ ".gconvert") outG
        liftIO $ renameFile (outS ++ ".gconvert") outS
        liftIO $ renameFile (outI ++ ".gconvert") outI
        logInfo "Done"
        -- overwrite genotype data field in POSEIDON.yml file
        unless (onlyGeno || isJust outPath) $ do
            gFileSpec <- case outFormat of
                    "EIGENSTRAT" -> return $
                        GenotypeEigenstrat outGeno Nothing outSnp Nothing outInd Nothing
                    "PLINK"      -> return $
                        GenotypePlink      outGeno Nothing outSnp Nothing outInd Nothing
                    _  -> liftIO . throwIO . PoseidonGenericException $
                        "Illegal outFormat " ++ outFormat ++
                        ". Only Outformats EIGENSTRAT or PLINK are allowed at the moment"
            let oldGenotypeData = GenotypeDataSpec gFileSpec (genotypeSnpSet . posPacGenotypeData $ pac)
                newPac = pac { posPacGenotypeData = oldGenotypeData }
            logInfo "Adjusting POSEIDON.yml..."
            liftIO $ writePoseidonPackage newPac
        -- delete now replaced input genotype data
        let oldBaseDir = posPacBaseDir pac
        oldGenoFiles <- case genotypeFileSpec . posPacGenotypeData $ pac of
                GenotypeEigenstrat g _ s _ i _ -> return [oldBaseDir </> g, oldBaseDir </> s, oldBaseDir </> i]
                GenotypePlink      g _ s _ i _ -> return [oldBaseDir </> g, oldBaseDir </> s, oldBaseDir </> i]
                GenotypeVCF        g _         -> return [oldBaseDir </> g]
        let newGenoFiles = [outG, outS, outI]

        let filesToDelete = oldGenoFiles \\ newGenoFiles
        when removeOld . liftIO . mapM_ removeFile $ filesToDelete
  where
    checkFile :: FilePath -> PoseidonIO Bool
    checkFile fn = do
        fe <- liftIO $ doesFileExist fn
        when fe $ logWarning $ "File " ++ fn ++ " exists"
        return fe
