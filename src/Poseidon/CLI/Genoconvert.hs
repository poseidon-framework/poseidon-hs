{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Genoconvert where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..),
                                             loadGenotypeData,
                                             printSNPCopyProgress)
import           Poseidon.Package           (readPoseidonPackageCollection,
                                             PoseidonPackage (..),
                                             writePoseidonPackage,
                                             PackageReadOptions (..),
                                             defaultPackageReadOptions,
                                             makePseudoPackageFromGenotypeData)
import           Poseidon.Utils             (PoseidonLogIO)

import           Colog                      (logInfo, logWarning)
import           Data.Maybe                 (isJust)
import           Data.Text                  (pack)
import           Control.Monad              (when, unless)
import           Pipes                      (MonadIO (liftIO), 
                                            runEffect, (>->))
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat)
import           SequenceFormats.Plink      (writePlink)
import           System.Directory           (removeFile, doesFileExist)
import           System.FilePath            ((<.>), (</>))

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _genoconvertBaseDirs     :: [FilePath]
    , _genoconvertInGenos      :: [GenotypeDataSpec]
    , _genoConvertOutFormat    :: GenotypeFormatSpec
    , _genoConvertOutOnlyGeno  :: Bool
    , _genoMaybeOutPackagePath :: Maybe FilePath
    , _genoconvertRemoveOld    :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

runGenoconvert :: GenoconvertOptions -> PoseidonLogIO ()
runGenoconvert (GenoconvertOptions baseDirs inGenos outFormat onlyGeno outPath removeOld) = do
    -- load packages
    properPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    pseudoPackages <- liftIO $ mapM makePseudoPackageFromGenotypeData inGenos
    logInfo $ pack $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    -- create new directory
    logInfo $ pack $ "Writing to directory (will be created if needed): " ++ outPath
    liftIO $ createDirectoryIfMissing True outPath
    -- convert
    mapM_ (convertGenoTo outFormat onlyGeno outPath removeOld) properPackages
    mapM_ (convertGenoTo outFormat True outPath removeOld) pseudoPackages

convertGenoTo :: GenotypeFormatSpec -> Bool -> Maybe FilePath -> Bool -> PoseidonPackage -> PoseidonLogIO ()
convertGenoTo outFormat onlyGeno outPath removeOld pac = do
    -- start message
    logInfo $ pack $
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
        let newBaseDir = case outPath of
                Just x -> x
                Nothing -> posPacBaseDir pac
        let [outG, outS, outI] = map (newBaseDir </>) [outGeno, outSnp, outInd]
        anyExists <- or <$> mapM checkFile [outG, outS, outI]
        if anyExists
        then logWarning $ pack $ ("skipping genotype conversion for " ++ posPacTitle pac)
        else do
            logInfo "Processing SNPs..."
            liftIO $ runSafeT $ do            
                (eigenstratIndEntries, eigenstratProd) <- loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac)
                let outConsumer = case outFormat of
                        GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI eigenstratIndEntries
                        GenotypeFormatPlink -> writePlink outG outS outI eigenstratIndEntries
                runEffect $ eigenstratProd >-> printSNPCopyProgress >-> outConsumer
            logInfo "Done"
            -- overwrite genotype data field in POSEIDON.yml file
            unless (onlyGeno || (isJust outPath)) $ do
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
        when fe $ logWarning $ pack $ ("File " ++ fn ++ " exists")
        return fe