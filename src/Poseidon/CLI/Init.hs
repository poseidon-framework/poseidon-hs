module Poseidon.CLI.Init (
    runInit, InitOptions (..),
    ) where

import           Poseidon.GenotypeData      (GenotypeDataSpec (..), 
                                             GenotypeFormatSpec (..))
import           Poseidon.Package           (newPackageTemplate)

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
    -- compile new paths
    let outInd = takeFileName genoFile
        outSnp = takeFileName snpFile
        outGeno = takeFileName indFile
        genotypeData = GenotypeDataSpec format outGeno outSnp outInd
        outJanno = outName <.> "janno"
        outBib = outName <.> "bib"
    -- POSEIDON.yml
    pac <- newPackageTemplate outName genotypeData outJanno outBib
    encodeFile (outPath </> "POSEIDON.yml") pac
    -- janno
    
    -- bib
    -- copy genotype files --
    copyFile genoFile $ outPath </> outInd
    copyFile snpFile $ outPath </> outSnp
    copyFile indFile $ outPath </> outGeno