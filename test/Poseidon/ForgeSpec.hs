module Poseidon.ForgeSpec (spec) where

import           Poseidon.CLI.Forge
import           Poseidon.ForgeRecipe       (ForgeEntity (..), 
                                             ForgeRecipe (..))
import           Poseidon.Janno             (jannoToSimpleMaybeList,
                                             PoseidonSample (..),
                                             createMinimalJanno)
import           Poseidon.Package           (PoseidonPackage (..),
                                             readAllPoseidonPackages,
                                             maybereadJannoFiles)

import           Data.Maybe                 (catMaybes)
import           Text.CSL                   (Reference (..) )
import           Test.Hspec
import           Text.RawString.QQ

spec = do
    testFindNonExistentEntities
    testFilterPackages
    testFilterJannoFiles
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
        ps <- readAllPoseidonPackages testBaseDir
        ents <- findNonExistentEntities goodEntities ps  
        ents `shouldBe` []
    it "should find bad entities" $ do
        ps <- readAllPoseidonPackages testBaseDir
        ents <- findNonExistentEntities badEntities ps  
        ents `shouldMatchList` badEntities

testFilterPackages :: Spec
testFilterPackages = 
    describe "Poseidon.CLI.Forge.filterPackages" $ do
    it "should select all relevant packages" $ do
        ps <- readAllPoseidonPackages testBaseDir
        pacs <- filterPackages goodEntities ps  
        map posPacTitle pacs `shouldMatchList` ["Schiffels_2016", "Wang_Plink_test_2020", "Lamnidis_2018"]
    it "should drop all irrelevant packages" $ do
        ps <- readAllPoseidonPackages testBaseDir
        pacs <- filterPackages badEntities ps
        pacs `shouldBe` []

testFilterJannoFiles :: Spec
testFilterJannoFiles = 
    describe "Poseidon.CLI.Forge.filterJannoFiles" $ do
    it "should select all relevant individuals" $ do
        ps <- readAllPoseidonPackages testBaseDir
        rps <- filterPackages goodEntities ps
        let namesPs = map posPacTitle rps
        jFs <- maybereadJannoFiles rps
        let gJrs = catMaybes $ jannoToSimpleMaybeList jFs
        let jRs = filterJannoFiles goodEntities $ zip namesPs gJrs
        map posSamIndividualID jRs `shouldMatchList` [
                -- Schiffels 2016
                "XXX001", "XXX002", "XXX003", "XXX004", "XXX005",
                "XXX006", "XXX007", "XXX008", "XXX009", "XXX010",
                -- Lamnidis 2018
                "XXX011", "XXX013", "XXX017", "XXX019",
                -- Wang 2020
                "SAMPLE3"
            ]
    it "should drop all irrelevant individuals" $ do
        ps <- readAllPoseidonPackages testBaseDir
        rps <- filterPackages badEntities ps
        let namesPs = map posPacTitle rps
        jFs <- maybereadJannoFiles rps
        let gJrs = catMaybes $ jannoToSimpleMaybeList jFs
        let jRs = filterJannoFiles badEntities $ zip namesPs gJrs
        jRs `shouldBe` []

testExtractEntityIndices :: Spec
testExtractEntityIndices = 
    describe "Poseidon.CLI.Forge.extractEntityIndices" $ do
    it "should select all relevant individuals" $ do
        ps <- readAllPoseidonPackages testBaseDir
        indInts <- extractEntityIndices goodEntities ps  
        indInts `shouldMatchList` [0, 2, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 23]
    it "should drop all irrelevant individuals" $ do
        ps <- readAllPoseidonPackages testBaseDir
        indInts <- extractEntityIndices badEntities ps
        indInts `shouldBe` []
