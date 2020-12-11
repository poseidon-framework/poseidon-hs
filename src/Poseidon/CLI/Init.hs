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
        outPosYml = "POSEIDON.yml"
        outJanno = outName <.> "janno"
        outBib = "sources.bib"
    -- POSEIDON.yml
    let genotypeData = GenotypeDataSpec format outGeno outSnp outInd
    pac <- newPackageTemplate outName genotypeData outJanno
    encodeFile outPosYml pac
    -- copy genotype files --
    copyFile genoFile $ outPath </> outInd
    copyFile snpFile $ outPath </> outSnp
    copyFile indFile $ outPath </> outGeno