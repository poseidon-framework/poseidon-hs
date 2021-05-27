module Poseidon.ForgeSpec (spec) where

import           Poseidon.CLI.Forge
import           Poseidon.EntitiesList       (PoseidonEntity (..), 
                                             EntitiesList)
import           Poseidon.Janno             (JannoRow (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             readPoseidonPackageCollection,
                                             PackageReadOptions (..), defaultPackageReadOptions)

import           Test.Hspec

spec :: Spec
spec = do
    testFindNonExistentEntities
    testFilterPackages
    testFilterJannoFiles
    testExtractEntityIndices

testPacReadOpts :: PackageReadOptions
testPacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    }

testBaseDir :: [FilePath]
testBaseDir = ["test/testDat/testModules/ancient"]

goodEntities :: EntitiesList
goodEntities = [
        Pac "Schiffels_2016",
        Group "POP1",
        Ind "SAMPLE3"
    ]

badEntities :: EntitiesList
badEntities = [
        Pac "Schiffels_2015",
        Group "foo",
        Ind "bar"
    ]

testFindNonExistentEntities :: Spec
testFindNonExistentEntities = 
    describe "Poseidon.CLI.Forge.findNonExistentEntities" $ do
    it "should ignore good entities" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        ents <- findNonExistentEntities goodEntities ps  
        ents `shouldBe` []
    it "should find bad entities" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        ents <- findNonExistentEntities badEntities ps  
        ents `shouldMatchList` badEntities

testFilterPackages :: Spec
testFilterPackages = 
    describe "Poseidon.CLI.Forge.filterPackages" $ do
    it "should select all relevant packages" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        pacs <- filterPackages goodEntities ps  
        map posPacTitle pacs `shouldMatchList` ["Schiffels_2016", "Wang_Plink_test_2020", "Lamnidis_2018"]
    it "should drop all irrelevant packages" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        pacs <- filterPackages badEntities ps
        pacs `shouldBe` []

testFilterJannoFiles :: Spec
testFilterJannoFiles = 
    describe "Poseidon.CLI.Forge.filterJannoFiles" $ do
    it "should select all relevant individuals" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        rps <- filterPackages goodEntities ps
        let pacNames = map posPacTitle rps
        let jannos = map posPacJanno rps
        let filteredJannos = filterJannoFiles goodEntities $ zip pacNames jannos
        map jIndividualID filteredJannos `shouldMatchList` [
                -- Schiffels 2016
                "XXX001", "XXX002", "XXX003", "XXX004", "XXX005",
                "XXX006", "XXX007", "XXX008", "XXX009", "XXX010",
                -- Lamnidis 2018
                "XXX011", "XXX013", "XXX017", "XXX019",
                -- Wang 2020
                "SAMPLE3"
            ]
    it "should drop all irrelevant individuals" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        rps <- filterPackages badEntities ps
        let pacNames = map posPacTitle rps
        let jannos = map posPacJanno rps
        let filteredJannos = filterJannoFiles goodEntities $ zip pacNames jannos
        filteredJannos `shouldBe` []

testExtractEntityIndices :: Spec
testExtractEntityIndices = 
    describe "Poseidon.CLI.Forge.extractEntityIndices" $ do
    it "should select all relevant individuals" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        indInts <- extractEntityIndices goodEntities ps  
        indInts `shouldMatchList` [0, 2, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 23]
    it "should drop all irrelevant individuals" $ do
        ps <- readPoseidonPackageCollection testPacReadOpts testBaseDir
        indInts <- extractEntityIndices badEntities ps
        indInts `shouldBe` []
