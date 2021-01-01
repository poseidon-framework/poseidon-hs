module Poseidon.ValidateSpec (spec) where

import Poseidon.CLI.Validate

import           Test.Hspec

spec = do
    testRenderMismatch
    testZipWithPadding

testRenderMismatch :: Spec
testRenderMismatch = 
    describe "Poseidon.CLI.Validate.renderMismatch" $ do
    it "should not find mismatch for equal one-element lists" $ do
        renderMismatch ["a"] ["a"] `shouldBe` ""
    it "should find mismatch for non-equal one-element lists" $ do
        renderMismatch ["a"] ["b"] `shouldBe` "(a = b)"
    it "should not find mismatch for equal two-element lists" $ do
        renderMismatch ["a", "b"] ["a", "b"] `shouldBe` ""
    it "should find mismatch for non-equal two-element lists" $ do
        renderMismatch ["a", "b"] ["a", "c"] `shouldBe` "(b = c)"
    it "should stop printing at ten mismatches" $ do
        renderMismatch 
            ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"] 
            ["b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"] 
            `shouldBe` 
            "(a = b)\n(b = c)\n(c = d)\n(d = e)\n(e = f)\n(f = g)\
            \\n(g = h)\n(h = i)\n(i = j)\n(j = k)\n..."

testZipWithPadding :: Spec
testZipWithPadding = 
    describe "Poseidon.CLI.Validate.zipWithPadding" $ do
    it "should" $ do
        1 `shouldBe` 1
