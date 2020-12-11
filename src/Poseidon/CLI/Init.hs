module Poseidon.CLI.Init (
    runInit, InitOptions (..),
    ) where

import           Poseidon.BibFile           (writeBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), 
                                             GenotypeFormatSpec (..))
import           Poseidon.Janno             (createMinimalSamplesList, 
                                             writeJannoFile)
import           Poseidon.Package           (newPackageTemplate,
                                             getIndividuals)

import           Data.Yaml                  (encodeFile)
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
    createDirectory outPath
    -- compile file names
    let outInd = takeFileName indFile
        outSnp = takeFileName snpFile
        outGeno = takeFileName genoFile
        genotypeData = GenotypeDataSpec format outGeno outSnp outInd
        outJanno = outName <.> "janno"
        outBib = outName <.> "bib"
    -- POSEIDON.yml
    pac <- newPackageTemplate outName genotypeData outJanno outBib
    encodeFile (outPath </> "POSEIDON.yml") pac
    -- genotype data
    copyFile indFile $ outPath </> outInd
    copyFile snpFile $ outPath </> outSnp
    copyFile genoFile $ outPath </> outGeno
    -- janno (needs the genotype files!)
    indEntries <- getIndividuals pac
    let jannoRows = createMinimalSamplesList indEntries
    writeJannoFile (outPath </> outJanno) jannoRows
    -- bib
    writeBibTeXFile (outPath </> outBib) []