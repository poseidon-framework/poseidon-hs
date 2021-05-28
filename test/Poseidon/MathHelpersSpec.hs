module Poseidon.MathHelpersSpec (spec) where

import           Poseidon.MathHelpers

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    testAvg
    testRoundTo
    testStdev

testAvg :: Spec
testAvg = 
    describe "Poseidon.MathHelpers.avg" $ do
    it "should calculate the mean correctly for single element lists" $ do
        avg [1] `shouldBe` 1
    it "should calculate the mean correctly for normal lists" $ do
        avg [1,2,3,4] `shouldBe` 2.5
    it "should calculate the mean correctly for lists with negative values" $ do
        avg [-1,1] `shouldBe` 0
    it "should always be within the range of the input list" $ property test
    where
        test :: [Double] -> Bool
        test [] = True 
        test x = minimum x <= avg x && maximum x >= avg x

genSmallCounts :: Gen Int
genSmallCounts = choose (0, 10)

testRoundTo :: Spec
testRoundTo = 
    describe "Poseidon.MathHelpers.roundTo" $ do
    it "should round down for .4" $ do
        roundTo 0 1.4 `shouldBe` 1
    it "should round up for .5" $ do
        roundTo 0 1.5 `shouldBe` 2
    it "should always round to the desired precision" $ 
        property $ forAll genSmallCounts test
    where
        test :: Int -> Double -> Bool
        test i x = abs (x - roundTo i x) <= (10**(-(fromIntegral i)))

testStdev :: Spec
testStdev = 
    describe "Poseidon.MathHelpers.stdev" $ do
    it "should calculate the sd correctly for single element lists" $ do
        stdev [1] `shouldBe` 0.0
    it "should calculate the sd correctly for normal lists" $ do
        roundTo 2 (stdev [1,2,3,4]) `shouldBe` 1.12
    it "should calculate the sd correctly for lists with negative values" $ do
        stdev [-1,1] `shouldBe` 1.0



