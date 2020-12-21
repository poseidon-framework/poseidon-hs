module Poseidon.MathHelpersSpec (spec) where

import           Poseidon.MathHelpers

import           Test.Hspec

spec = do
    testAvg

testAvg :: Spec
testAvg = 
    describe "Poseidon.MathHelpers.avg" $ do
    it "should calculate the mean correctly for single element lists" $ do
        avg [1] `shouldBe` 1
    it "should calculate the mean correctly for normal lists" $ do
        avg [1,2,3,4] `shouldBe` 2.5
    it "should calculate the mean correctly for lists with negative values" $ do
        avg [-1,1] `shouldBe` 0

