module Poseidon.SurveySpec (spec) where

import Poseidon.CLI.Survey ()

import           Test.Hspec

spec = do
    testRenderPackageWithCompleteness
    testRenderJannoCompleteness
    testAllNothing

testRenderPackageWithCompleteness :: Spec
testRenderPackageWithCompleteness = 
    describe "Poseidon.CLI.Survey.renderPackageWithCompleteness" $ do
    it "should" $ do
        1 `shouldBe` 1

testRenderJannoCompleteness :: Spec
testRenderJannoCompleteness = 
    describe "Poseidon.CLI.Survey.renderJannoCompleteness" $ do
    it "should" $ do
        1 `shouldBe` 1

testAllNothing :: Spec
testAllNothing = 
    describe "Poseidon.CLI.Survey.allNothing" $ do
    it "should" $ do
        1 `shouldBe` 1
