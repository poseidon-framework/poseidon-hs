{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Init where

import           Poseidon.BibFile           (dummyBibEntry, writeBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             loadIndividuals,
                                             )
import           Poseidon.Janno             (writeJannoFile)
import           Poseidon.Package           (PoseidonPackage (..),
                                             newPackageTemplate,
                                             newMinimalPackageTemplate,
                                             writePoseidonPackage)
import           Poseidon.Utils              (PoseidonException (..), PoseidonLogIO, checkFile)

import           Colog                      (logInfo)
import           Control.Exception          (throwIO)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  (pack)
import           System.Directory           (createDirectoryIfMissing, copyFile)
import           System.FilePath            ((<.>), (</>), takeFileName, takeBaseName)

data InitOptions = InitOptions
    { _initGenoData :: GenotypeDataSpec
    , _initPacPath :: FilePath
    , _initPacName :: Maybe String
    , _initMinimal :: Bool
    }

runInit :: InitOptions -> PoseidonLogIO ()
runInit (InitOptions (GenotypeDataSpec format_ genoFile_ _ snpFile_ _ indFile_ _ snpSet_) outPath maybeOutName minimal) = do
    -- create new directory
    logInfo $ pack $ "Creating new package directory: " ++ outPath
    liftIO $ createDirectoryIfMissing True outPath
    -- compile genotype data structure
    let outInd = takeFileName indFile_
        outSnp = takeFileName snpFile_
        outGeno = takeFileName genoFile_
        genotypeData = GenotypeDataSpec format_ outGeno Nothing outSnp Nothing outInd Nothing snpSet_
    -- genotype data
    logInfo "Copying genotype data"
    liftIO $ checkFile indFile_ Nothing
    liftIO $ checkFile snpFile_ Nothing
    liftIO $ checkFile genoFile_ Nothing
    liftIO $ copyFile indFile_ $ outPath </> outInd
    liftIO $ copyFile snpFile_ $ outPath </> outSnp
    liftIO $ copyFile genoFile_ $ outPath </> outGeno
    -- create new package
    logInfo "Creating new package entity"
    let outName = case maybeOutName of -- take basename of outPath, if name is not provided
            Just x -> x
            Nothing -> takeBaseName outPath
    when (outName == "") $ liftIO $ throwIO PoseidonEmptyOutPacNameException
    inds <- liftIO $ loadIndividuals outPath genotypeData
    pac <- if minimal
           then return $ newMinimalPackageTemplate outPath outName genotypeData
           else liftIO $ newPackageTemplate outPath outName genotypeData (Just (Left inds)) [dummyBibEntry]
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
