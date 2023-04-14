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
import           Poseidon.Utils             (PoseidonIO, envInputPlinkMode,
                                             envLogAction, logInfo, logWarning)

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
    , _genoConvertOutFormat       :: GenotypeFormatSpec
    , _genoConvertOutOnlyGeno     :: Bool
    , _genoMaybeOutPackagePath    :: Maybe FilePath
    , _genoconvertRemoveOld       :: Bool
    , _genoconvertOutPlinkPopMode :: PlinkPopNameMode
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
        _readOptStopOnDuplicates = False
        , _readOptIgnoreChecksums  = True
        , _readOptIgnoreGeno       = False
        , _readOptGenoCheck        = True
        }

runGenoconvert :: GenoconvertOptions -> PoseidonIO ()
runGenoconvert (GenoconvertOptions genoSources outFormat onlyGeno outPath removeOld outPlinkPopMode) = do

    -- load packages
    properPackages <- readPoseidonPackageCollection pacReadOpts $ [getPacBaseDirs x | x@PacBaseDir {} <- genoSources]
    inPlinkPopMode <- envInputPlinkMode
    pseudoPackages <- mapM makePseudoPackageFromGenotypeData [getGenoDirect x | x@GenoDirect {} <- genoSources]

    logInfo $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    -- convert
    mapM_ (convertGenoTo outFormat onlyGeno outPath removeOld inPlinkPopMode outPlinkPopMode) properPackages
    mapM_ (convertGenoTo outFormat True outPath removeOld inPlinkPopMode outPlinkPopMode) pseudoPackages

convertGenoTo :: GenotypeFormatSpec -> Bool -> Maybe FilePath -> Bool -> PlinkPopNameMode ->
    PlinkPopNameMode -> PoseidonPackage -> PoseidonIO ()
convertGenoTo outFormat onlyGeno outPath removeOld inPlinkPopMode outPlinkPopMode pac = do
    -- start message
    logInfo $
        "Converting genotype data in "
        ++ posPacTitle pac
        ++ " to format "
        ++ show outFormat
        ++ ":"
    -- compile file names paths
    let outName = posPacTitle pac
    let (outInd, outSnp, outGeno) = case outFormat of
            GenotypeFormatEigenstrat -> (outName <.> ".ind", outName <.> ".snp", outName <.> ".geno")
            GenotypeFormatPlink -> (outName <.> ".fam", outName <.> ".bim", outName <.> ".bed")
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
        let (outG, outS, outI) = (newBaseDir </> outGeno, newBaseDir </> outSnp, newBaseDir </> outInd)
        anyExists <- or <$> mapM checkFile [outG, outS, outI]
        if anyExists
        then logWarning ("skipping genotype conversion for " ++ posPacTitle pac)
        else do
            logInfo "Processing SNPs..."
            logA <- envLogAction
            currentTime <- liftIO getCurrentTime
            liftIO $ catch (
                runSafeT $ do
                    (eigenstratIndEntries, eigenstratProd) <- loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac) inPlinkPopMode
                    let outConsumer = case outFormat of
                            GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI eigenstratIndEntries
                            GenotypeFormatPlink -> writePlink outG outS outI (map (eigenstratInd2PlinkFam outPlinkPopMode) eigenstratIndEntries)
                    runEffect $ eigenstratProd >-> printSNPCopyProgress logA currentTime >-> outConsumer
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
    checkFile :: FilePath -> PoseidonIO Bool
    checkFile fn = do
        fe <- liftIO $ doesFileExist fn
        when fe $ logWarning $ "File " ++ fn ++ " exists"
        return fe
