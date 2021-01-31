module Poseidon.CLI.Init where

import           Poseidon.BibFile           (writeBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), 
                                             GenotypeFormatSpec (..))
import           Poseidon.Janno             (createMinimalJanno, 
                                             writeJannoFile)
import           Poseidon.Package           (PoseidonPackage (..),
                                             newPackageTemplate,
                                             getIndividuals, writePoseidonPackage)

import           Data.Yaml.Pretty.Extras    (encodeFilePretty)
import           System.Directory           (createDirectory, copyFile)
import           System.FilePath            ((<.>), (</>), takeFileName)

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
    putStrLn $ "Creating new package directory: " ++ outPath
    createDirectory outPath
    -- compile file names
    let outInd = takeFileName indFile
        outSnp = takeFileName snpFile
        outGeno = takeFileName genoFile
        genotypeData = GenotypeDataSpec format outGeno Nothing outSnp Nothing outInd Nothing
    -- genotype data
    putStrLn "Copying genotype data"
    copyFile indFile $ outPath </> outInd
    copyFile snpFile $ outPath </> outSnp
    copyFile genoFile $ outPath </> outGeno
    -- create new package
    putStrLn "Creating new package entity"
    pac <- newPackageTemplate outPath outName genotypeData
    -- POSEIDON.yml
    putStrLn "Creating POSEIDON.yml"
    writePoseidonPackage pac
    -- janno
    putStrLn "Creating empty .janno file"
    writeJannoFile (outPath </> outName <.> "janno") $ posPacJannoFile pac
    -- bib
    putStrLn "Creating empty .bib file"
    writeBibTeXFile (outPath </> outName <.> "bib") $ posPacBibFile pac

