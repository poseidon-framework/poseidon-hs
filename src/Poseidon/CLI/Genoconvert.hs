module Poseidon.CLI.Genoconvert where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             InGenotypeData (..),
                                             GenotypeFormatSpec (..),
                                             loadGenotypeData,
                                             printSNPCopyProgress)
import           Poseidon.Package           (readPoseidonPackageCollection,
                                             PoseidonPackage (..),
                                             writePoseidonPackage,
                                             PackageReadOptions (..),
                                             defaultPackageReadOptions,
                                             makePseudoPackageFromInGenotypeData)

import           Control.Monad              (when, unless)
import           Pipes                      (MonadIO (liftIO), 
                                            runEffect, (>->))
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat)
import           SequenceFormats.Plink      (writePlink)
import           System.Directory           (removeFile, doesFileExist)
import           System.FilePath            ((<.>), (</>))
import           System.IO                  (hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _genoconvertBaseDirs    :: [FilePath]
    , _genoconvertInGenos     :: [InGenotypeData]
    , _genoConvertOutFormat   :: GenotypeFormatSpec
    , _genoConvertOutOnlyGeno :: Bool
    , _genoconvertRemoveOld   :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

runGenoconvert :: GenoconvertOptions -> IO ()
runGenoconvert (GenoconvertOptions baseDirs inGenos outFormat onlyGeno removeOld) = do
    -- load packages
    properPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    pseudoPackages <- mapM makePseudoPackageFromInGenotypeData inGenos
    hPutStrLn stderr $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    -- convert
    mapM_ (convertGenoTo outFormat onlyGeno removeOld) properPackages
    mapM_ (convertGenoTo outFormat True removeOld) pseudoPackages

convertGenoTo :: GenotypeFormatSpec -> Bool -> Bool -> PoseidonPackage -> IO ()
convertGenoTo outFormat onlyGeno removeOld pac = do
    -- start message
    hPutStrLn stderr $
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
    then hPutStrLn stderr "The genotype data is already in the requested format"
    else do
        -- create new genotype data files
        let [outG, outS, outI] = map (posPacBaseDir pac </>) [outGeno, outSnp, outInd]
        anyExists <- or <$> mapM checkFile [outG, outS, outI]
        if anyExists
        then hPutStrLn stderr ("skipping genotype convertion for " ++ posPacTitle pac)
        else do
            runSafeT $ do            
                (eigenstratIndEntries, eigenstratProd) <- loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac)
                let outConsumer = case outFormat of
                        GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI eigenstratIndEntries
                        GenotypeFormatPlink -> writePlink outG outS outI eigenstratIndEntries
                liftIO $ hPutStrLn stderr "Processing SNPs..."
                runEffect $ eigenstratProd >-> printSNPCopyProgress >-> outConsumer
                liftIO $ hPutStrLn stderr "Done"
            -- overwrite genotype data field in POSEIDON.yml file
            unless onlyGeno $ do
                let genotypeData = GenotypeDataSpec outFormat outGeno Nothing outSnp Nothing outInd Nothing (snpSet . posPacGenotypeData $ pac)
                    newPac = pac { posPacGenotypeData = genotypeData }
                hPutStrLn stderr "Adjusting POSEIDON.yml..."
                writePoseidonPackage newPac
            -- delete now replaced input genotype data
            when removeOld $ mapM_ removeFile [
                  posPacBaseDir pac </> genoFile (posPacGenotypeData pac)
                , posPacBaseDir pac </> snpFile  (posPacGenotypeData pac)
                , posPacBaseDir pac </> indFile  (posPacGenotypeData pac)
                ]
  where
    checkFile :: FilePath -> IO Bool
    checkFile fn = do
        fe <- doesFileExist fn
        when fe $ hPutStrLn stderr ("File " ++ fn ++ " exists")
        return fe