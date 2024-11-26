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
                                             logError, logInfo, logWarning)

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
import           System.Exit                (ExitCode (..), exitWith)
import           System.FilePath            (dropTrailingPathSeparator, (<.>),
                                             (</>))

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _genoconvertGenoSources     :: [GenoDataSource]
    , _genoConvertOutFormat       :: String
    , _genoMaybeOutPackagePath    :: Maybe FilePath
    , _genoconvertRemoveOld       :: Bool
    , _genoconvertOutPlinkPopMode :: PlinkPopNameMode
    , _genoconvertOnlyLatest      :: Bool
    , _genoconvertOutZip          :: Bool
    }

runGenoconvert :: GenoconvertOptions -> PoseidonIO ()
runGenoconvert (GenoconvertOptions genoSources outFormat outPath
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
    mapM_ (convertGenoTo outFormat False outPath removeOld outPlinkPopMode outZip) properPackages
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
        ++ if outZip then " (gzipped):" else ":"

    -- compile new relative file names
    let outName = getPacName . posPacNameAndVersion $ pac
    let gz = if outZip then "gz" else ""
    (outIrel, outSrel, outGrel) <- case outFormat of
            "EIGENSTRAT" -> return
                (outName <.> ".ind", outName <.> ".snp" <.> gz, outName <.> ".geno" <.> gz)
            "PLINK" -> return
                (outName <.> ".fam", outName <.> ".bim" <.> gz, outName <.> ".bed" <.> gz)
            _                     -> liftIO . throwIO . PoseidonGenericException $
                "Illegal outFormat " ++ outFormat ++
                ". Only Outformats EIGENSTRAT or PLINK are allowed at the moment"

    -- compile new absolute genotype file names
    newBaseDir <- case outPath of
        Just x -> do
            -- create new directory
            logInfo $ "Writing to directory (will be created if missing): " ++ x
            liftIO $ createDirectoryIfMissing True (dropTrailingPathSeparator x)
            return x
        Nothing -> return $ posPacBaseDir pac
    let (outGabs, outSabs, outIabs) = (newBaseDir </> outGrel, newBaseDir </> outSrel, newBaseDir </> outIrel)

    -- check whether anything needs doing at all
    allExists <- and <$> mapM checkFile [outGabs, outSabs, outIabs]
    if allExists
    then do
        if onlyGeno
        then do
            logError $ "No files were created or overwritten for " ++ show (posPacNameAndVersion pac)
            liftIO $ exitWith (ExitFailure 1)
        else
            logWarning $ "Package already in desired file-type, skipping genotype conversion for " ++
                show (posPacNameAndVersion pac)
    else do
        -- Convert!
        logInfo "Processing SNPs..."
        logA <- envLogAction
        currentTime <- liftIO getCurrentTime
        errLength <- envErrorLength
        let eigenstratIndEntries = jannoRows2EigenstratIndEntries . posPacJanno $ pac
        let zipEnding = if outZip then ".gz" else "" -- we need this to trigger the zipping
        liftIO $ catch (
            runSafeT $ do
                eigenstratProd <- loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac)
                let outConsumer = case outFormat of
                        "EIGENSTRAT" -> writeEigenstrat (outGabs ++ ".gconvert" ++ zipEnding)
                                                        (outSabs ++ ".gconvert" ++ zipEnding)
                                                        (outIabs ++ ".gconvert")
                                                        eigenstratIndEntries
                        "PLINK"      -> writePlink (outGabs ++ ".gconvert" ++ zipEnding)
                                                   (outSabs ++ ".gconvert" ++ zipEnding)
                                                   (outIabs ++ ".gconvert")
                                                   (map (eigenstratInd2PlinkFam outPlinkPopMode) eigenstratIndEntries)
                        _  -> liftIO . throwIO . PoseidonGenericException $
                            "Illegal outFormat " ++ outFormat ++
                            ". Only Outformats EIGENSTRAT or PLINK are allowed at the moment"
                runEffect $ eigenstratProd >-> printSNPCopyProgress logA currentTime >-> outConsumer
            ) (throwIO . PoseidonGenotypeExceptionForward errLength)
        -- the following will just overwrite if the file already exists, which is OK
        liftIO $ renameFile (outGabs ++ ".gconvert" ++ zipEnding) outGabs
        liftIO $ renameFile (outSabs ++ ".gconvert" ++ zipEnding) outSabs
        liftIO $ renameFile (outIabs ++ ".gconvert") outIabs
        logInfo "Done"

    -- overwrite genotype data field in POSEIDON.yml file (using relative paths)
    unless (onlyGeno || isJust outPath) $ do
        gFileSpec <- case outFormat of
                "EIGENSTRAT" -> return $
                    GenotypeEigenstrat outGrel Nothing outSrel Nothing outIrel Nothing
                "PLINK"      -> return $
                    GenotypePlink      outGrel Nothing outSrel Nothing outIrel Nothing
                _  -> liftIO . throwIO . PoseidonGenericException $
                    "Illegal outFormat " ++ outFormat ++
                    ". Only Outformats EIGENSTRAT or PLINK are allowed at the moment"
        let newGenotypeData = GenotypeDataSpec gFileSpec (genotypeSnpSet . posPacGenotypeData $ pac)
            newPac = pac { posPacGenotypeData = newGenotypeData }
        logInfo $ "Adjusting POSEIDON.yml for " ++ show (posPacNameAndVersion pac)
        liftIO $ writePoseidonPackage newPac
        -- delete now replaced input genotype data
    when removeOld $ do
        let oldBaseDir = posPacBaseDir pac
        oldGenoFiles <- case genotypeFileSpec . posPacGenotypeData $ pac of
                GenotypeEigenstrat g _ s _ i _ -> return [oldBaseDir </> g, oldBaseDir </> s, oldBaseDir </> i]
                GenotypePlink      g _ s _ i _ -> return [oldBaseDir </> g, oldBaseDir </> s, oldBaseDir </> i]
                GenotypeVCF        g _         -> return [oldBaseDir </> g]
        let newGenoFiles = [outGabs, outSabs, outIabs]
        let filesToDelete = oldGenoFiles \\ newGenoFiles
        liftIO . mapM_ removeFile $ filesToDelete
  where
    checkFile :: FilePath -> PoseidonIO Bool
    checkFile fn = do
        fe <- liftIO $ doesFileExist fn
        when fe $ logWarning $ "File " ++ fn ++ " exists"
        return fe
