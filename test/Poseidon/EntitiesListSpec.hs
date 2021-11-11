module Poseidon.EntitiesListSpec (spec) where

import           Poseidon.EntitiesList

import           Test.Hspec
import           Data.Either (fromRight, isLeft)

spec :: Spec
spec = do
    testReadPoseidonEntitiesString

testReadPoseidonEntitiesString :: Spec
testReadPoseidonEntitiesString = 
    describe "Poseidon.EntitiesList.readPoseidonEntitiesString" $ do
    it "should parse single entity lists correctly" $ do
        fromRight [] (readPoseidonEntitiesString "<a>") `shouldBe` [Ind "a"]
        fromRight [] (readPoseidonEntitiesString "b") `shouldBe` [Group "b"]
        fromRight [] (readPoseidonEntitiesString "*c*") `shouldBe` [Pac "c"]
    it "should parse longer entity lists correctly" $ do
        fromRight [] (readPoseidonEntitiesString "<a>,b,*c*") `shouldBe` [Ind "a", Group "b", Pac "c"]
        fromRight [] (readPoseidonEntitiesString "<a1>,b1,<a2>,*c*,b2") `shouldBe` [Ind "a1", Group "b1", Ind "a2", Pac "c", Group "b2"]
    it "should ignore spaces after commas" $ do
        fromRight [] (readPoseidonEntitiesString "<a>, b, *c*") `shouldBe` [Ind "a", Group "b", Pac "c"]
        fromRight [] (readPoseidonEntitiesString "*c*, b") `shouldBe` [Pac "c", Group "b"]
    it "should fail with any other spaces" $ do
        readPoseidonEntitiesString "<a> ,b,*c*"  `shouldSatisfy` isLeft
        readPoseidonEntitiesString " <a>,b,*c*"  `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a >,b,*c*"  `shouldSatisfy` isLeft
        readPoseidonEntitiesString "< a>,b,*c*"  `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a>,b,*c* "  `shouldSatisfy` isLeft
        readPoseidonEntitiesString "<a>,b d,*c*" `shouldSatisfy` isLeft


