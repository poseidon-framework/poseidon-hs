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

withBadEntities :: ForgeRecipe
withBadEntities = [
        ForgePac "Schiffels_2015",
        ForgeGroup "foo",
        ForgeInd "bar"
    ]

testFindNonExistentEntities :: Spec
testFindNonExistentEntities = 
    describe "Poseidon.CLI.Forge.findNonExistentEntities" $ do
        it "should ignore good entities" $ do
            ps <- loadPoseidonPackages testBaseDir
            g <- findNonExistentEntities goodEntities ps  
            g `shouldBe` []
        it "should find bad entities" $ do
            ps <- loadPoseidonPackages testBaseDir
            g <- findNonExistentEntities withBadEntities ps  
            g `shouldMatchList` withBadEntities

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