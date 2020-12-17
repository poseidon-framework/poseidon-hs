module Poseidon.ForgeSpec (spec) where

import           Poseidon.CLI.Forge
import           Test.Hspec
import           Text.RawString.QQ

spec = do
    testFindNonExistentEntities
    testFilterPackages
    testFilterJannoFiles
    testFilterBibEntries
    testExtractEntityIndices

testFindNonExistentEntities :: Spec
testFindNonExistentEntities = 
    describe "Poseidon.CLI.Forge.findNonExistentEntities" $ do
    it "should " $ do
        1 `shouldBe` 1

testFilterPackages :: Spec
testFilterPackages = 
    describe "Poseidon.CLI.Forge.filterPackages" $ do
    it "should " $ do
        1 `shouldBe` 1

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