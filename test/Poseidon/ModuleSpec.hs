{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Poseidon.ModuleSpec (spec) where

import           Poseidon.Module       (GenotypeDataSpecJSON (..),
                                        PoseidonModuleJSON (..))

import           Data.Aeson            (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B
import           Data.Time             (defaultTimeLocale, parseTimeOrError)
import           Data.Version          (Version (..))
import           Test.Hspec
import           Text.RawString.QQ

spec = do
    testPoseidonFromJSON

json :: B.ByteString
json = [r|{
    "moduleName": "myTestModule1",
    "genotypeData": {
        "format": "EIGENSTRAT",
        "genoFile": "geno.txt",
        "snpFile": "snp.txt",
        "indFile": "ind.txt"
    },
    "metaData": "metaData.txt",
    "notes": "hello world",
    "maintainer": "Stephan Schiffels",
    "maintainerEmail": "schiffels@shh.mpg.de",
    "lastUpdate": "2019-07-10",
    "version": "1.0.0"
}|]

replace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replace from to s =
    let (h, t) = B.breakSubstring from s
    in  B.concat [h, to, B.drop (B.length from) t]

jsonDat :: PoseidonModuleJSON
jsonDat = PoseidonModuleJSON {
    moduleName = "myTestModule1",
    genotypeData = GenotypeDataSpecJSON {
        format = "EIGENSTRAT",
        genoFile = "geno.txt",
        snpFile = "snp.txt",
        indFile = "ind.txt"
    },
    metaData = Just "metaData.txt",
    notes = Just "hello world",
    maintainer = "Stephan Schiffels",
    maintainerEmail = "schiffels@shh.mpg.de",
    lastUpdate = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" "2019-07-10",
    version = Version [1, 0, 0] []
}

testPoseidonFromJSON :: Spec
testPoseidonFromJSON = describe "PoseidonModule.fromJSON" $ do
    it "should parse correct JSON data" $
        eitherDecodeStrict json `shouldBe` Right jsonDat
    it "should give error with incorrect date string" $ do
        let json2 = replace "2019-07-10" "2019-07-10sss" json
        eitherDecodeStrict json2 `shouldBe` (Left "Error in $: could not parse lastUpdate date string 2019-07-10sss" :: Either String PoseidonModuleJSON)
    it "should give error with incorrect version string" $ do
        let json2 = replace "1.0.0" "1.0.0sss" json
        eitherDecodeStrict json2 `shouldBe` (Left "Error in $: could not parse version string 1.0.0sss" :: Either String PoseidonModuleJSON)
    it "should give Nothing for missing metaData" $ do
        let json2 = replace "\"metaData\": \"metaData.txt\"," "" json
        eitherDecodeStrict json2 `shouldBe` Right (jsonDat {metaData = Nothing})
    it "should give Nothing for missing notes" $ do
        let json2 = replace "\"notes\": \"hello world\"," "" json
        eitherDecodeStrict json2 `shouldBe` Right (jsonDat {notes = Nothing})
    it "should fail with moduleName missing" $ do
        let json2 = replace "\"moduleName\": \"myTestModule1\"," "" json
        eitherDecodeStrict json2 `shouldBe` (Left "Error in $: key \"moduleName\" not present" :: Either String PoseidonModuleJSON)
    it "should fail with version missing" $ do
        let json2 = replace ",\n    \"version\": \"1.0.0\"" "" json
        eitherDecodeStrict json2 `shouldBe` (Left "Error in $: key \"version\" not present" :: Either String PoseidonModuleJSON)
    it "should fail with lastUpdate missing" $ do
        let json2 = replace "\"lastUpdate\": \"2019-07-10\"," "" json
        eitherDecodeStrict json2 `shouldBe` (Left "Error in $: key \"lastUpdate\" not present" :: Either String PoseidonModuleJSON)

testReadPoseidonModule :: Spec
testReadPoseidonModule = describe "readPoseidonModule" $ do
    it "should read correct file content" $
        pending