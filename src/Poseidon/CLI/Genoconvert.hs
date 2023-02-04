{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Genoconvert where

import           Poseidon.GenotypeData      (GenoDataSource (..),
                                             GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..),
                                             loadGenotypeData,
                                             printSNPCopyProgress)
import           Poseidon.Package           (PackageReadOptions (..),
                                             PoseidonException (PoseidonGenotypeExceptionForward),
                                             PoseidonPackage (..),
                                             defaultPackageReadOptions,
                                             makePseudoPackageFromGenotypeData,
                                             readPoseidonPackageCollection,
                                             writePoseidonPackage)
import           Poseidon.Utils             (PoseidonLogIO, logInfo, logWarning)

import           Control.Exception          (catch, throwIO)
import           Control.Monad              (unless, when, forM)
import           Control.Monad.Reader       (ask)
import           Data.Maybe                 (isJust)
import           Data.Time                  (getCurrentTime)
import           Pipes                      (MonadIO (liftIO), runEffect, (>->))
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat)
import           SequenceFormats.Plink      (writePlink, PlinkPopNameMode, eigenstratInd2PlinkFam)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist, removeFile)
import           System.FilePath            (dropTrailingPathSeparator, (<.>),
                                             (</>))

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _genoconvertGenoSources  :: [GenoDataSource]
    , _genoConvertOutFormat    :: GenotypeFormatSpec
    , _genoConvertOutOnlyGeno  :: Bool
    , _genoMaybeOutPackagePath :: Maybe FilePath
    , _genoconvertRemoveOld    :: Bool
    , _genoconvertPlinkPopMode :: PlinkPopNameMode
    }

runGenoconvert :: GenoconvertOptions -> PoseidonLogIO ()
runGenoconvert (GenoconvertOptions genoSources outFormat onlyGeno outPath removeOld plinkPopMode) = do

    let pacReadOpts = defaultPackageReadOptions {
            _readOptStopOnDuplicates = False
            , _readOptIgnoreChecksums  = True
            , _readOptIgnoreGeno       = False
            , _readOptGenoCheck        = True
            , _readOptPlinkPopMode     = plinkPopMode
            }

    -- load packages
    properPackages <- readPoseidonPackageCollection pacReadOpts $ [getPacBaseDirs x | x@PacBaseDir {} <- genoSources]
    pseudoPackages <- liftIO . forM [getGenoDirect x | x@GenoDirect {} <- genoSources] $ \gd ->
        makePseudoPackageFromGenotypeData gd plinkPopMode

    logInfo $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    -- convert
    mapM_ (convertGenoTo outFormat onlyGeno outPath removeOld plinkPopMode) properPackages
    mapM_ (convertGenoTo outFormat True outPath removeOld plinkPopMode) pseudoPackages

convertGenoTo :: GenotypeFormatSpec -> Bool -> Maybe FilePath -> Bool -> PlinkPopNameMode ->
    PoseidonPackage -> PoseidonLogIO ()
convertGenoTo outFormat onlyGeno outPath removeOld plinkPopMode pac = do
    -- start message
    logInfo $
        "Converting genotype data in "
        ++ posPacTitle pac
        ++ " to format "
        ++ show outFormat
        ++ ":"
    -- compile file names paths
    let outName = posPacTitle pac
    let [outInd, outSnp, outGeno] = case outFormat of
            GenotypeFormatEigenstrat -> [outName <.> ".ind", outName <.> ".snp", outName <.> ".geno"]
            GenotypeFormatPlink -> [outName <.> ".fam", outName <.> ".bim", outName <.> ".bed"]
    -- check if genotype data needs conversion
    if format (posPacGenotypeData pac) == outFormat
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
        let [outG, outS, outI] = map (newBaseDir </>) [outGeno, outSnp, outInd]
        anyExists <- or <$> mapM checkFile [outG, outS, outI]
        if anyExists
        then logWarning ("skipping genotype conversion for " ++ posPacTitle pac)
        else do
            logInfo "Processing SNPs..."
            logEnv <- ask
            currentTime <- liftIO getCurrentTime
            liftIO $ catch (
                runSafeT $ do
                    (eigenstratIndEntries, eigenstratProd) <- loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac) plinkPopMode
                    let outConsumer = case outFormat of
                            GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI eigenstratIndEntries
                            GenotypeFormatPlink -> writePlink outG outS outI (map (eigenstratInd2PlinkFam plinkPopMode) eigenstratIndEntries)
                    runEffect $ eigenstratProd >-> printSNPCopyProgress logEnv currentTime >-> outConsumer
                ) (throwIO . PoseidonGenotypeExceptionForward)
            logInfo "Done"
            -- overwrite genotype data field in POSEIDON.yml file
            unless (onlyGeno || isJust outPath) $ do
                let genotypeData = GenotypeDataSpec outFormat outGeno Nothing outSnp Nothing outInd Nothing (snpSet . posPacGenotypeData $ pac)
                    newPac = pac { posPacGenotypeData = genotypeData }
                logInfo "Adjusting POSEIDON.yml..."
                liftIO $ writePoseidonPackage newPac
            -- delete now replaced input genotype data
            when removeOld $ liftIO $ mapM_ removeFile [
                  posPacBaseDir pac </> genoFile (posPacGenotypeData pac)
                , posPacBaseDir pac </> snpFile  (posPacGenotypeData pac)
                , posPacBaseDir pac </> indFile  (posPacGenotypeData pac)
                ]
  where
    checkFile :: FilePath -> PoseidonLogIO Bool
    checkFile fn = do
        fe <- liftIO $ doesFileExist fn
        when fe $ logWarning $ "File " ++ fn ++ " exists"
        return fe
