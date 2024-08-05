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
                                             PoseidonException (PoseidonGenotypeExceptionForward),
                                             PoseidonPackage (..),
                                             defaultPackageReadOptions,
                                             makePseudoPackageFromGenotypeData,
                                             readPoseidonPackageCollection,
                                             writePoseidonPackage)
import           Poseidon.Utils             (PoseidonIO, envErrorLength,
                                             envInputPlinkMode, envLogAction,
                                             logInfo, logWarning,
                                             PoseidonException(..))

import           Control.Exception          (catch, throwIO)
import           Control.Monad              (unless, when)
import           Data.Maybe                 (isJust)
import           Data.Time                  (getCurrentTime)
import           Pipes                      (MonadIO (liftIO), runEffect, (>->))
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat)
import           SequenceFormats.Plink      (PlinkPopNameMode,
                                             eigenstratInd2PlinkFam, writePlink)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, removeFile)
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
    }

runGenoconvert :: GenoconvertOptions -> PoseidonIO ()
runGenoconvert (GenoconvertOptions genoSources outFormat onlyGeno outPath removeOld outPlinkPopMode onlyLatest) = do
    let pacReadOpts = defaultPackageReadOptions {
          _readOptIgnoreChecksums  = True
        , _readOptIgnoreGeno       = False
        , _readOptGenoCheck        = True
        , _readOptOnlyLatest       = onlyLatest
    }
    -- load packages
    properPackages <- readPoseidonPackageCollection pacReadOpts $ [getPacBaseDir x | x@PacBaseDir {} <- genoSources]
    inPlinkPopMode <- envInputPlinkMode
    pseudoPackages <- mapM makePseudoPackageFromGenotypeData [getGenoDirect x | x@GenoDirect {} <- genoSources]

    logInfo $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    -- convert
    mapM_ (convertGenoTo outFormat onlyGeno outPath removeOld inPlinkPopMode outPlinkPopMode) properPackages
    mapM_ (convertGenoTo outFormat True outPath removeOld inPlinkPopMode outPlinkPopMode) pseudoPackages

convertGenoTo :: String -> Bool -> Maybe FilePath -> Bool -> PlinkPopNameMode ->
    PlinkPopNameMode -> PoseidonPackage -> PoseidonIO ()
convertGenoTo outFormat onlyGeno outPath removeOld inPlinkPopMode outPlinkPopMode pac = do
    -- start message
    logInfo $
        "Converting genotype data in "
        ++ show (posPacNameAndVersion pac)
        ++ " to format "
        ++ show outFormat
        ++ ":"
    -- compile file names paths
    let outName = getPacName . posPacNameAndVersion $ pac
    (outInd, outSnp, outGeno) <- case outFormat of
            "EIGENSTRAT" -> return (outName <.> ".ind", outName <.> ".snp", outName <.> ".geno")
            "PLINK"      -> return (outName <.> ".fam", outName <.> ".bim", outName <.> ".bed")
            _  -> liftIO . throwIO $ PoseidonGenericException "only Outformats EIGENSTRAT or PLINK are allowed at the moment"
    -- check if genotype data needs conversion
    if getFormat (genotypeFileSpec (posPacGenotypeData pac)) == outFormat
    then logWarning "The genotype data is already in the requested format"
    else do
        -- create new genotype data files
        newBaseDir <- case outPath of
            Just x -> do
                -- create new directory
                logInfo $ "Writing to directory (will be created if missing): " ++ x
                liftIO $ createDirectoryIfMissing True (dropTrailingPathSeparator x)
                return x
            Nothing -> return $ posPacBaseDir pac
        let (outG, outS, outI) = (newBaseDir </> outGeno, newBaseDir </> outSnp, newBaseDir </> outInd)
        anyExists <- or <$> mapM checkFile [outG, outS, outI]
        if anyExists
        then logWarning ("skipping genotype conversion for " ++ show (posPacNameAndVersion pac))
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
                            "EIGENSTRAT" -> writeEigenstrat outG outS outI eigenstratIndEntries
                            "PLINK"      -> writePlink outG outS outI (map (eigenstratInd2PlinkFam outPlinkPopMode) eigenstratIndEntries)
                            _  -> liftIO . throwIO $ PoseidonGenericException "only Outformats EIGENSTRAT or PLINK are allowed at the moment"
                    runEffect $ eigenstratProd >-> printSNPCopyProgress logA currentTime >-> outConsumer
                ) (throwIO . PoseidonGenotypeExceptionForward errLength)
            logInfo "Done"
            -- overwrite genotype data field in POSEIDON.yml file
            unless (onlyGeno || isJust outPath) $ do
                gFileSpec <- case outFormat of
                        "EIGENSTRAT" -> return $ GenotypeEigenstrat outGeno Nothing outSnp Nothing outInd Nothing
                        "PLINK"      -> return $ GenotypePlink      outGeno Nothing outSnp Nothing outInd Nothing
                        _  -> liftIO . throwIO $ PoseidonGenericException "only Outformats EIGENSTRAT or PLINK are allowed at the moment"
                let genotypeData = GenotypeDataSpec gFileSpec (genotypeSnpSet . posPacGenotypeData $ pac)
                    newPac = pac { posPacGenotypeData = genotypeData }
                logInfo "Adjusting POSEIDON.yml..."
                liftIO $ writePoseidonPackage newPac
            -- delete now replaced input genotype data
            let filesToDelete = case genotypeFileSpec . posPacGenotypeData $ pac of
                    GenotypeEigenstrat g _ s _ i _ -> [g, s, i]
                    GenotypePlink      g _ s _ i _ -> [g, s, i]
                    GenotypeVCF        g _         -> [g]
            when removeOld . liftIO . mapM_ (removeFile . (posPacBaseDir pac </>)) $ filesToDelete
  where
    checkFile :: FilePath -> PoseidonIO Bool
    checkFile fn = do
        fe <- liftIO $ doesFileExist fn
        when fe $ logWarning $ "File " ++ fn ++ " exists"
        return fe
    getFormat :: GenotypeFileSpec -> String
    getFormat (GenotypeEigenstrat _ _ _ _ _ _) = "EIGENSTRAT"
    getFormat (GenotypePlink      _ _ _ _ _ _) = "PLINK"
    getFormat (GenotypeVCF        _ _        ) = "VCF"
