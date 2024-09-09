{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Init where

import           Poseidon.BibFile       (dummyBibEntry, writeBibTeXFile)
import           Poseidon.GenotypeData  (GenotypeDataSpec (..),
                                         GenotypeFileSpec (..), loadIndividuals,
                                         reduceGenotypeFilepaths)
import           Poseidon.Janno         (writeJannoFile)
import           Poseidon.Package       (PoseidonPackage (..),
                                         newMinimalPackageTemplate,
                                         newPackageTemplate,
                                         writePoseidonPackage)
import           Poseidon.Utils         (PoseidonIO, checkFile,
                                         determinePackageOutName, logInfo)

import           Control.Monad          (unless, forM_)
import           Control.Monad.IO.Class (liftIO)
import           System.Directory       (copyFile, createDirectoryIfMissing)
import           System.FilePath        (dropTrailingPathSeparator,
                                         takeFileName, (<.>), (</>))

data InitOptions = InitOptions
    { _initGenoData :: GenotypeDataSpec
    , _initPacPath  :: FilePath
    , _initPacName  :: Maybe String
    , _initMinimal  :: Bool
    }

runInit :: InitOptions -> PoseidonIO ()
runInit (InitOptions genotypeDataIn outPathRaw maybeOutName minimal) = do
    -- create new directory
    let outPath = dropTrailingPathSeparator outPathRaw
    logInfo $ "Creating new package directory: " ++ outPath
    liftIO $ createDirectoryIfMissing True outPath
    -- compile genotype data structure
    genotypeDataOut <- snd <$> reduceGenotypeFilepaths genotypeDataIn
    -- genotype data
    logInfo "Copying genotype data"
    let sourceFiles = case genotypeFileSpec genotypeDataIn of
            GenotypeEigenstrat genoFile _ snpFile _ indFile _ -> [genoFile, snpFile, indFile]
            GenotypePlink      genoFile _ snpFile _ indFile _ -> [genoFile, snpFile, indFile]
            GenotypeVCF        vcfFile  _                     -> [vcfFile]
    forM_ sourceFiles $ \sourceFile -> do
        liftIO $ checkFile sourceFile Nothing
        let targetFile = outPath </> takeFileName sourceFile
        liftIO $ copyFile sourceFile targetFile
    -- create new package
    logInfo "Creating new package entity"
    outName <- liftIO $ determinePackageOutName maybeOutName outPath
    inds <- loadIndividuals outPath genotypeDataOut
    pac <- if minimal
           then newMinimalPackageTemplate outPath outName genotypeDataOut
           else newPackageTemplate outPath outName genotypeDataOut (Just (Left inds)) mempty [dummyBibEntry]
    -- POSEIDON.yml
    logInfo "Creating POSEIDON.yml"
    liftIO $ writePoseidonPackage pac
    unless minimal $ do
        -- janno
        logInfo "Creating minimal .janno file"
        liftIO $ writeJannoFile (outPath </> outName <.> "janno") $ posPacJanno pac
        -- bib
        logInfo "Creating dummy .bib file"
        liftIO $ writeBibTeXFile (outPath </> outName <.> "bib") $ posPacBib pac
