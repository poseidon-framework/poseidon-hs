module Poseidon.ForgeSpec (spec) where

import           Poseidon.CLI.Forge
import           Poseidon.Package           (PoseidonPackage (..),
                                             loadPoseidonPackages)
import           Poseidon.ForgeRecipe       (ForgeEntity (..), 
                                             ForgeRecipe (..))
import           Test.Hspec
import           Text.RawString.QQ

spec = do
    testFindNonExistentEntities
    testFilterPackages
    testFilterJannoFiles
    testFilterBibEntries
    testExtractEntityIndices

testBaseDir :: [FilePath]
testBaseDir = ["test/testDat/testModules/ancient"]

goodEntities :: ForgeRecipe
goodEntities = [
        ForgePac "Schiffels_2016",
        ForgeGroup "POP1",
        ForgeInd "SAMPLE3"
    ]

badEntities :: ForgeRecipe
badEntities = [
        ForgePac "Schiffels_2015",
        ForgeGroup "foo",
        ForgeInd "bar"
    ]

testFindNonExistentEntities :: Spec
testFindNonExistentEntities = 
    describe "Poseidon.CLI.Forge.findNonExistentEntities" $ do
    it "should ignore good entities" $ do
        ps <- loadPoseidonPackages testBaseDir
        ents <- findNonExistentEntities goodEntities ps  
        ents `shouldBe` []
    it "should find bad entities" $ do
        ps <- loadPoseidonPackages testBaseDir
        ents <- findNonExistentEntities badEntities ps  
        ents `shouldMatchList` badEntities

testFilterPackages :: Spec
testFilterPackages = 
    describe "Poseidon.CLI.Forge.filterPackages" $ do
    it "should select all relevant packages" $ do
        ps <- loadPoseidonPackages testBaseDir
        pacs <- filterPackages goodEntities ps  
        map posPacTitle pacs `shouldMatchList` ["Schiffels_2016", "Wang_Plink_test_2020", "Lamnidis_2018"]
    it "should drop all irrelevant packages" $ do
        ps <- loadPoseidonPackages testBaseDir
        pacs <- filterPackages badEntities ps
        pacs `shouldBe` []

testFilterJannoFiles :: Spec
testFilterJannoFiles = 
    describe "Poseidon.CLI.Forge.filterJannoFiles" $ do
    it "should " $ do
        1 `shouldBe` 1

testFilterBibEntries :: Spec
testFilterBibEntries = 
    describe "Poseidon.CLI.Forge.filterBibEntries" $ do
    it "should " $ do
        1 `shouldBe` 1

testExtractEntityIndices :: Spec
testExtractEntityIndices = 
    describe "Poseidon.CLI.Forge.extractEntityIndices" $ do
    it "should " $ do
        1 `shouldBe` 1