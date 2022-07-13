{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntitiesListSpec (spec) where

import           Poseidon.EntitiesList
import           Poseidon.Package        (PackageReadOptions (..),
                                          PoseidonPackage (..),
                                          defaultPackageReadOptions,
                                          getJointIndividualInfo,
                                          readPoseidonPackageCollection)
import           Poseidon.SecondaryTypes (IndividualInfo (..))
import           Poseidon.Utils          (LogMode (..), PoseidonException,
                                          usePoseidonLogger)

import           Data.Aeson              (decode, encode)
import           Data.Either             (fromRight, isLeft)
import           Test.Hspec

spec :: Spec
spec = do
    testReadPoseidonEntitiesString
    testReadEntitiesFromFile
    testFindNonExistentEntities
    testFilterPackages
    testExtractEntityIndices
    testJSON


testReadPoseidonEntitiesString :: Spec
testReadPoseidonEntitiesString =
    describe "Poseidon.EntitiesList.readPoseidonEntitiesString" $ do
    it "should parse single entity lists correctly" $ do
        fromRight [] (readEntitiesFromString "<a>") `shouldBe` [Include $ Ind "a"]
        fromRight [] (readEntitiesFromString "b") `shouldBe` [Include $ Group "b"]
        fromRight [] (readEntitiesFromString "*c*") `shouldBe` [Include $ Pac "c"]
    it "should parse longer entity lists correctly" $ do
        fromRight [] (readEntitiesFromString "<a>,b,*c*") `shouldBe`
            map Include [Ind "a", Group "b", Pac "c"]
        fromRight [] (readEntitiesFromString "<a1>,b1,<a2>,*c*,b2") `shouldBe`
            map Include [Ind "a1", Group "b1", Ind "a2", Pac "c", Group "b2"]
    it "should parse unsigned entity lists correctly" $ do
        fromRight [] (readEntitiesFromString "<a>,b,*c*") `shouldBe`
            [Ind "a", Group "b", Pac "c"]
        fromRight [] (readEntitiesFromString "<a1>,b1,<a2>,*c*,b2") `shouldBe`
            [Ind "a1", Group "b1", Ind "a2", Pac "c", Group "b2"]
    it "should ignore spaces after commas" $ do
        fromRight [] (readEntitiesFromString "<a>, b, *c*") `shouldBe`
            map Include [Ind "a", Group "b", Pac "c"]
        fromRight [] (readEntitiesFromString "*c*,  b") `shouldBe`
            map Include [Pac "c", Group "b"]
    it "should parse exclusion entities correctly" $ do
        fromRight [] (readEntitiesFromString "-<a>") `shouldBe` [Exclude $ Ind "a"]
        fromRight [] (readEntitiesFromString "-<a1>, <a2>, -b1,b2,-*c1*, *c2*") `shouldBe`
            [Exclude $ Ind "a1", Include $ Ind "a2",
             Exclude $ Group "b1", Include $ Group "b2",
             Exclude $ Pac "c1", Include $ Pac "c2"]
    it "should fail with any other spaces" $ do
        -- the following type annotations - annoyingly - are required because readEntitiesFromString is polymorphic,
        -- and even though it all returns Left, the compiler complains about ambiguous types.
        (readEntitiesFromString "<a> ,b,*c*"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString " <a>,b,*c*"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a >,b,*c*"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "< a>,b,*c*"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b,*c* "   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b d,*c*"  :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>>,b,*c*"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b,*c*c*"  :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "-<a>,b,*c*c*" :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b,*c*-"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "-a>,b,*c*"    :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft

testReadEntitiesFromFile :: Spec
testReadEntitiesFromFile =
    describe "Poseidon.EntitiesList.readEntitiesFromFile" $ do
    let g1 = "test/testDat/testEntityFiles/goodEntities1.txt"
        g2 = "test/testDat/testEntityFiles/goodEntities2.txt"
        g3 = "test/testDat/testEntityFiles/goodEntities3.txt"
        g4 = "test/testDat/testEntityFiles/goodEntities4.txt"
        b1 = "test/testDat/testEntityFiles/badEntities1.txt"
    it "should parse good, single-value-per-line files correctly" $ do
        g1res <- readEntitiesFromFile g1
        g1res `shouldBe`
            map Include [Ind "a", Group "b", Pac "c"]
    it "should parse good, multi-value-per-line files correctly" $ do
        g2res <- readEntitiesFromFile g2
        g2res `shouldBe`
            map Include [Ind "a1", Ind "a2", Group "b1", Pac "c1", Pac "c2", Group "b2", Group "b3"]
    it "should handle empty lines and #-comments correctly" $ do
        g3res <- readEntitiesFromFile g3
        g3res `shouldBe`
            map Include [Ind "a1", Ind "a2", Group "b1", Group "b2", Group "b3"]
    it "should handle exclusion correctly" $ do
        g4res <- readEntitiesFromFile g4
        g4res `shouldBe`
            [Include $ Ind "a1", Exclude $ Ind "a2",
             Exclude $ Group "b1", Include $ Group "b1",
             Exclude $ Pac "c2"]
    it "should fail to parse bad files and throw an exception" $ do
        (readEntitiesFromFile b1 :: IO EntitiesList) `shouldThrow` anyException -- wrong space

testPacReadOpts :: PackageReadOptions
testPacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    }

testBaseDir :: [FilePath]
testBaseDir = ["test/testDat/testPackages/ancient"]

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
    describe "Poseidon.EntitiesList.findNonExistentEntities" $ do
    it "should ignore good entities" $ do
        ps <- usePoseidonLogger NoLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let ents = findNonExistentEntities goodEntities (getJointIndividualInfo ps)
        ents `shouldBe` []
    it "should find bad entities" $ do
        ps <- usePoseidonLogger NoLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let ents = findNonExistentEntities badEntities (getJointIndividualInfo ps)
        ents `shouldMatchList` badEntities

testFilterPackages :: Spec
testFilterPackages =
    describe "Poseidon.EntitiesList.filterPackages" $ do
    it "should select all relevant packages" $ do
        ps <- usePoseidonLogger NoLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let pacs = filterRelevantPackages goodEntities ps
        map posPacTitle pacs `shouldMatchList` ["Schiffels_2016", "Wang_Plink_test_2020", "Lamnidis_2018"]
    it "should drop all irrelevant packages" $ do
        ps <- usePoseidonLogger NoLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let pacs = filterRelevantPackages badEntities ps
        pacs `shouldBe` []

testExtractEntityIndices :: Spec
testExtractEntityIndices =
    describe "Poseidon.EntitiesList.extractEntityIndices" $ do
    it "should select all relevant individuals" $ do
        ps <- usePoseidonLogger NoLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let indInts = conformingEntityIndices goodEntities (getJointIndividualInfo ps)
        indInts `shouldMatchList` [0, 2, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 23]
    it "should drop all irrelevant individuals" $ do
        ps <- usePoseidonLogger NoLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let indInts = conformingEntityIndices badEntities (getJointIndividualInfo ps)
        indInts `shouldBe` []
    it "should correctly extract indices with ordered signed entities" $ do
        let indInfo = [
                IndividualInfo "Ind1" ["Pop1", "PopB"] "Pac1",
                IndividualInfo "Ind2" ["Pop1", "PopB"] "Pac1",
                IndividualInfo "Ind3" ["Pop2", "PopB"] "Pac1",
                IndividualInfo "Ind4" ["Pop2", "PopB"] "Pac1",
                IndividualInfo "Ind5" ["Pop3", "PopC"] "Pac2",
                IndividualInfo "Ind6" ["Pop3", "PopC"] "Pac2",
                IndividualInfo "Ind7" ["Pop4", "PopC"] "Pac2",
                IndividualInfo "Ind8" ["Pop4", "PopC"] "Pac2"]
        conformingEntityIndices [Include (Pac "Pac1"), Exclude (Group "Pop2"), Include (Ind "Ind3")] indInfo `shouldBe` [0, 1, 2]
        conformingEntityIndices [Include (Pac "Pac1")] indInfo `shouldBe` [0, 1, 2, 3]

testJSON :: Spec
testJSON =
    describe "Poseidon.EntitiesList.ToJSON" $ do
        it "should encode entities correctly to JSON" $ do
            encode (Ind "Ind1")                `shouldBe` "\"<Ind1>\""
            encode (Group "Group1")            `shouldBe` "\"Group1\""
            encode (Pac "Pac1")                `shouldBe` "\"*Pac1*\""
            encode (Exclude (Ind "Ind1"))      `shouldBe` "\"-<Ind1>\""
            encode (Exclude (Group "Group1"))  `shouldBe` "\"-Group1\""
            encode (Exclude (Pac "Pac1"))      `shouldBe` "\"-*Pac1*\""
        it "should decode entities correctly from JSON" $ do
            decode "\"<Ind1>\""  `shouldBe` Just (Ind "Ind1")
            decode "\"Group1\""  `shouldBe` Just (Group "Group1")
            decode "\"*Pac1*\""  `shouldBe` Just (Pac "Pac1")
            decode "\"-<Ind1>\"" `shouldBe` Just (Exclude (Ind "Ind1"))
            decode "\"-Group1\"" `shouldBe` Just (Exclude (Group "Group1"))
            decode "\"-*Pac1*\"" `shouldBe` Just (Exclude (Pac "Pac1"))
