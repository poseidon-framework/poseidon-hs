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
import           Poseidon.Utils              (PoseidonException (..))

import           Control.Monad              (unless, when)
import           Control.Monad.Catch        (throwM)
import           System.Directory           (createDirectoryIfMissing, copyFile)
import           System.FilePath            ((<.>), (</>), takeFileName, takeBaseName)
import           System.IO                  (hPutStrLn, stderr)

data InitOptions = InitOptions
    { _initGenoData :: GenotypeDataSpec
    , _initPacPath :: FilePath
    , _initPacName :: Maybe String
    , _initMinimal :: Bool
    }

runInit :: InitOptions -> IO ()
runInit (InitOptions genotypeData@(GenotypeDataSpec _ genoFile_ _ snpFile_ _ indFile_ _ _) outPath maybeOutName minimal) = do
    -- create new directory
    hPutStrLn stderr $ "Creating new package directory: " ++ outPath
    createDirectoryIfMissing True outPath
    -- compile genotype data structure
    let outInd = takeFileName indFile_
        outSnp = takeFileName snpFile_
        outGeno = takeFileName genoFile_
    -- genotype data
    hPutStrLn stderr "Copying genotype data"
    copyFile indFile_ $ outPath </> outInd
    copyFile snpFile_ $ outPath </> outSnp
    copyFile genoFile_ $ outPath </> outGeno
    -- create new package
    hPutStrLn stderr "Creating new package entity"
    let outName = case maybeOutName of -- take basename of outPath, if name is not provided
            Just x -> x
            Nothing -> takeBaseName outPath
    when (outName == "") $ throwM PoseidonEmptyOutPacNameException
    inds <- loadIndividuals outPath genotypeData
    pac <- if minimal
           then return $ newMinimalPackageTemplate outPath outName genotypeData
           else newPackageTemplate outPath outName genotypeData (Just (Left inds)) [dummyBibEntry]
    -- POSEIDON.yml
    hPutStrLn stderr "Creating POSEIDON.yml"
    writePoseidonPackage pac
    unless minimal $ do
        -- janno
        hPutStrLn stderr "Creating minimal .janno file"
        writeJannoFile (outPath </> outName <.> "janno") $ posPacJanno pac
        -- bib
        hPutStrLn stderr "Creating dummy .bib file"
        writeBibTeXFile (outPath </> outName <.> "bib") $ posPacBib pac
