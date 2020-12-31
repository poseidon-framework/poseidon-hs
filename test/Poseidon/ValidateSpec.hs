module Poseidon.ValidateSpec (spec) where

import Poseidon.CLI.Validate

import           Test.Hspec

spec = do
    testRenderMismatch
    testZipWithPadding

testRenderMismatch :: Spec
testRenderMismatch = 
    describe "Poseidon.CLI.Validate.renderMismatch" $ do
    it "should" $ do
        1 `shouldBe` 1

testZipWithPadding :: Spec
testZipWithPadding = 
    describe "Poseidon.CLI.Validate.zipWithPadding" $ do
    it "should" $ do
        1 `shouldBe` 1
