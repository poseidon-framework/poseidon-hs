module Poseidon.CLI.Genoconvert where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..),
                                             loadGenotypeData,
                                             printSNPCopyProgress)
import           Poseidon.Package           (findAllPoseidonYmlFiles,
                                             readPoseidonPackageCollection,
                                             PoseidonPackage (..),
                                             writePoseidonPackage)

import           Control.Monad              (when)
import           Pipes                      (MonadIO (liftIO), 
                                            lift, runEffect, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (SafeT (..), runSafeT, throwM)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..), GenoLine,
                                             writeEigenstrat)
import           SequenceFormats.Plink      (writePlink)
import           System.Console.ANSI        (hClearLine, hSetCursorColumn)
import           System.Directory           (removeFile)
import           System.FilePath            ((<.>), (</>))
import           System.IO                  (hPutStr, hPutStrLn, stderr)

-- | A datatype representing command line options for the validate command
data GenoconvertOptions = GenoconvertOptions
    { _baseDirs :: [FilePath]
    , _outFormat :: GenotypeFormatSpec
    }

runGenoconvert :: GenoconvertOptions -> IO ()
runGenoconvert (GenoconvertOptions baseDirs outFormat) = do
    -- load packages
    allPackages <- readPoseidonPackageCollection True False baseDirs
    -- convert
    mapM_ (convertGenoTo outFormat) allPackages

convertGenoTo :: GenotypeFormatSpec -> PoseidonPackage -> IO ()
convertGenoTo outFormat pac = do
    -- start message
    hPutStrLn stderr $
        "Converting genotype data in package "
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
        runSafeT $ do
            (eigenstratIndEntries, eigenstratProd) <- loadGenotypeData (posPacBaseDir pac) (posPacGenotypeData pac)
            let [outG, outS, outI] = map (posPacBaseDir pac </>) [outGeno, outSnp, outInd]
            let outConsumer = case outFormat of
                    GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI eigenstratIndEntries
                    GenotypeFormatPlink -> writePlink outG outS outI eigenstratIndEntries
            runEffect $ eigenstratProd >-> printSNPCopyProgress >-> outConsumer
            liftIO $ hClearLine stderr
            liftIO $ hSetCursorColumn stderr 0
            liftIO $ hPutStrLn stderr "SNPs processed: All done"
        -- overwrite genotype data field in POSEIDON.yml file
        let genotypeData = GenotypeDataSpec outFormat outGeno Nothing outSnp Nothing outInd Nothing
            newPac = pac { posPacGenotypeData = genotypeData }
        writePoseidonPackage newPac
        -- delete now replaced input genotype data
        mapM_ removeFile [
              posPacBaseDir pac </> genoFile (posPacGenotypeData pac)
            , posPacBaseDir pac </> snpFile  (posPacGenotypeData pac)
            , posPacBaseDir pac </> indFile  (posPacGenotypeData pac)
            ]