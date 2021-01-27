module Poseidon.CLI.Init (
    runInit, InitOptions (..),
    ) where

import           Poseidon.BibFile           (writeBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), 
                                             GenotypeFormatSpec (..))
import           Poseidon.Janno             (createMinimalSamplesList, 
                                             writeJannoFile)
import           Poseidon.Package           (newPackageTemplate,
                                             getIndividuals,
                                             decodePoseidonYml)

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
    putStrLn $ "Creating new package directory: " ++ outPath
    createDirectory outPath
    -- compile file names
    let outInd = takeFileName indFile
        outSnp = takeFileName snpFile
        outGeno = takeFileName genoFile
        genotypeData = GenotypeDataSpec format outGeno outSnp outInd
        outJanno = outName <.> "janno"
        outBib = outName <.> "bib"
    -- POSEIDON.yml
    putStrLn "Compiling POSEIDON.yml"
    pac <- newPackageTemplate outName genotypeData outJanno outBib
    encodeFile (outPath </> "POSEIDON.yml") pac
    -- genotype data
    putStrLn "Copying genotype data"
    copyFile indFile $ outPath </> outInd
    copyFile snpFile $ outPath </> outSnp
    copyFile genoFile $ outPath </> outGeno
    -- janno (needs the genotype files!)
    putStrLn "Creating empty .janno file"
    new_package <- decodePoseidonYml $ outPath </> "POSEIDON.yml"
    indEntries <- getIndividuals new_package
    let jannoRows = createMinimalSamplesList indEntries
    writeJannoFile (outPath </> outJanno) jannoRows
    -- bib
    putStrLn "Creating empty .bib file"
    writeBibTeXFile (outPath </> outBib) []
