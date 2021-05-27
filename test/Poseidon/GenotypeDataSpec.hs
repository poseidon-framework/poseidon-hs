module Poseidon.GenotypeDataSpec (spec) where

import           Poseidon.GenotypeData

import           Test.Hspec

spec :: Spec
spec = do
    testSNPSetMergeList

testSNPSetMergeList :: Spec
testSNPSetMergeList = 
    describe "Poseidon.GenotypeData.snpSetMergeList" $ do
    it "should merge a list of length 1 correctly" $ do
        snpSetMergeList [SNPSet1240K] True `shouldBe` SNPSet1240K
    it "should merge a list with identical values correctly" $ do
        snpSetMergeList [SNPSet1240K, SNPSet1240K, SNPSet1240K] True `shouldBe` SNPSet1240K
    it "should merge with union merge correctly" $ do
        snpSetMergeList [SNPSet1240K, SNPSetHumanOrigins] False `shouldBe` SNPSet1240K
    it "should merge with intersect merge correctly" $ do
        snpSetMergeList [SNPSet1240K, SNPSetHumanOrigins] True `shouldBe` SNPSetHumanOrigins
    it "should merge with the snp set Other correctly" $ do
        snpSetMergeList [SNPSetOther, SNPSetHumanOrigins, SNPSet1240K] True `shouldBe` SNPSetOther
