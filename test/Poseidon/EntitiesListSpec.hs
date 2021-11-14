module Poseidon.EntitiesListSpec (spec) where

import           Poseidon.EntitiesList

import           Test.Hspec
import           Data.Either (fromRight, isLeft)

spec :: Spec
spec = do
    testReadPoseidonEntitiesString
    testReadEntitiesFromFile

testReadPoseidonEntitiesString :: Spec
testReadPoseidonEntitiesString = 
    describe "Poseidon.EntitiesList.readPoseidonEntitiesString" $ do
    it "should parse single entity lists correctly" $ do
        fromRight [] (readPoseidonEntitiesString "<a>") `shouldBe` [Include $ Ind "a"]
        fromRight [] (readPoseidonEntitiesString "b") `shouldBe` [Include $ Group "b"]
        fromRight [] (readPoseidonEntitiesString "*c*") `shouldBe` [Include $ Pac "c"]
    it "should parse longer entity lists correctly" $ do
        fromRight [] (readPoseidonEntitiesString "<a>,b,*c*") `shouldBe` 
            map Include [Ind "a", Group "b", Pac "c"]
        fromRight [] (readPoseidonEntitiesString "<a1>,b1,<a2>,*c*,b2") `shouldBe` 
            map Include [Ind "a1", Group "b1", Ind "a2", Pac "c", Group "b2"]
    it "should ignore spaces after commas" $ do
        fromRight [] (readPoseidonEntitiesString "<a>, b, *c*") `shouldBe` 
            map Include [Ind "a", Group "b", Pac "c"]
        fromRight [] (readPoseidonEntitiesString "*c*,  b") `shouldBe` 
            map Include [Pac "c", Group "b"]
    it "should parse exclusion entities correctly" $ do
        fromRight [] (readPoseidonEntitiesString "-<a>") `shouldBe` [Exclude $ Ind "a"]
        fromRight [] (readPoseidonEntitiesString "-<a1>, <a2>, -b1,b2,-*c1*, *c2*") `shouldBe` 
            [Exclude $ Ind "a1", Include $ Ind "a2", 
             Exclude $ Group "b1", Include $ Group "b2", 
             Exclude $ Pac "c1", Include $ Pac "c2"]
    it "should fail with any other spaces" $ do
        readPoseidonEntitiesString "<a> ,b,*c*"   `shouldSatisfy` isLeft
        readPoseidonEntitiesString " <a>,b,*c*"   `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a >,b,*c*"   `shouldSatisfy` isLeft
        readPoseidonEntitiesString "< a>,b,*c*"   `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a>,b,*c* "   `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a>,b d,*c*"  `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a>>,b,*c*"   `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a>,b,*c*c*"  `shouldSatisfy` isLeft
        readPoseidonEntitiesString "-<a>,b,*c*c*" `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a>,b,*c*-"   `shouldSatisfy` isLeft
        readPoseidonEntitiesString "-a>,b,*c*"    `shouldSatisfy` isLeft

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
        readEntitiesFromFile b1 `shouldThrow` anyException -- wrong space
