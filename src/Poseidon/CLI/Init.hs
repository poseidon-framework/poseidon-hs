module Poseidon.CLI.Init where

import           Poseidon.BibFile           (writeBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..), 
                                             loadIndividuals,
                                             SNPSetSpec (..)
                                             )
import           Poseidon.Janno             (createMinimalJanno, 
                                             writeJannoFile)
import           Poseidon.Package           (PoseidonPackage (..),
                                             newPackageTemplate,
                                             getIndividuals, writePoseidonPackage)

import           Data.Yaml.Pretty.Extras    (encodeFilePretty)
import           System.Directory           (createDirectory, copyFile)
import           System.FilePath            ((<.>), (</>), takeFileName)
import           System.IO                  (hPutStrLn, stderr)

data InitOptions = InitOptions
    { _inGenoFormat :: GenotypeFormatSpec
    , _inGenoFile :: FilePath
    , _inSnpFile :: FilePath
    , _inIndFile :: FilePath
    , _outPacPath :: FilePath
    , _outPacName :: String
    }

runInit :: InitOptions -> IO ()
runInit (InitOptions format genoFile snpFile indFile outPath outName) = do
    -- create new directory
    hPutStrLn stderr $ "Creating new package directory: " ++ outPath
    createDirectory outPath
    -- compile genotype data structure
    let outInd = takeFileName indFile
        outSnp = takeFileName snpFile
        outGeno = takeFileName genoFile
        genotypeData = GenotypeDataSpec format outGeno Nothing outSnp Nothing outInd Nothing SNPSetOther
    -- genotype data
    hPutStrLn stderr "Copying genotype data"
    copyFile indFile $ outPath </> outInd
    copyFile snpFile $ outPath </> outSnp
    copyFile genoFile $ outPath </> outGeno
    -- create new package
    hPutStrLn stderr "Creating new package entity"
    inds <- loadIndividuals outPath genotypeData
    pac <- newPackageTemplate outPath outName genotypeData (Just inds) Nothing Nothing
    -- POSEIDON.yml
    hPutStrLn stderr "Creating POSEIDON.yml"
    writePoseidonPackage pac
    -- janno
    hPutStrLn stderr "Creating empty .janno file"
    writeJannoFile (outPath </> outName <.> "janno") $ posPacJanno pac
    -- bib
    hPutStrLn stderr "Creating empty .bib file"
    writeBibTeXFile (outPath </> outName <.> "bib") $ posPacBib pac

