module Poseidon.CLI.Init (
    runInit, InitOptions (..),
    ) where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..), 
                                             GenotypeFormatSpec (..),
                                             determineGenotypeFormat)
import           Poseidon.Package           (newPackageTemplate)

import           Data.Yaml                  (encodeFile)
import           System.Directory           (createDirectory, copyFile)
import           System.FilePath            ((<.>), (</>), takeFileName)

data InitOptions = InitOptions
    { _inGenoFile :: FilePath
    , _inSnpFile :: FilePath
    , _inIndFile :: FilePath
    , _outPacPath :: FilePath
    , _outPacName :: String
    }

runInit :: InitOptions -> IO ()
runInit (InitOptions genoFile snpFile indFile outPath outName) = do
    -- create new directory
    createDirectory outPath
    -- compile new paths
    let outInd = outPath </> takeFileName genoFile
        outSnp = outPath </> takeFileName snpFile
        outGeno = outPath </> takeFileName indFile
        outPosYml = outPath </> "POSEIDON.yml"
        outJanno = outPath </> (outName <.> "janno")
        outBib = outPath <.> "sources.bib"
    -- POSEIDON.yml
    let genotypeFormat = determineGenotypeFormat outGeno
        genotypeData = GenotypeDataSpec genotypeFormat outGeno outSnp outInd
    pac <- newPackageTemplate outName genotypeData outJanno
    encodeFile outPosYml pac
    -- copy genotype files --
    copyFile genoFile outInd
    copyFile snpFile outSnp
    copyFile indFile outGeno