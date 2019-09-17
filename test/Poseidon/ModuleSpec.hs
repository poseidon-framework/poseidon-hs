{-# LANGUAGE QuasiQuotes #-}
module Poseidon.ModuleSpec (spec) where

import Data.ByteString (ByteString)
import Text.RawString.QQ

spec = do
    testPoseidonFromJSON

json1 :: ByteString
json1 = [r|{
    "moduleName": "myTestModule1",
    "genotypeData": {
        "format": "EIGENSTRAT",
        "genoFile": "geno.txt",
        "snpFile": "snp.txt",
        "indFile": "ind.txt"
    },
    "metaDataFile": "annot.txt",
    "notes": "hello world",
    "maintainer": "Stephan Schiffels",
    "maintainerEmail": "schiffels@shh.mpg.de",
    "lastUpdate": "2019-07-10",
    "version": "1.0.0"
}|]

jsonDat :: PoseidonModuleJSON
jsonDat1 = PoseidonModuleJSON {
    moduleName = "myTestModule1",
    genotypeData = GenotypDataSpecJSON {
        format = "EIGENSTRAT",
        genoFile = "geno.txt",
        snpFile = "snp.txt",
        indFile = "ind.txt"
    },
    metaDataFile = "annot.txt",
    notes = "hello world",
    maintainer = "Stephan Schiffels",
    maintainerEmail = "schiffels@shh.mpg.de",
    lastUpdate = "2019-07-10",
    version = "1.0.0"
}

testPoseidonFromJSON :: Spec
testPoseidonFromJSON = describe "PoseidonModule.fromJSON" $ do
    it "should parse correct JSON data" $
        decode json1 `shouldBe` jsonDat1

