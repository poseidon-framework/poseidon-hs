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
    it "should fill missing values with ?" $ do
        renderMismatch 
            ["a", "b", "c"] 
            ["d"] 
            `shouldBe`
            "(a = d)\n(b = ?)\n(c = ?)"

testZipWithPadding :: Spec
testZipWithPadding = 
    describe "Poseidon.CLI.Validate.zipWithPadding" $ do
    it "should zip normally for lists of equal length" $ do
        zipWithPadding "?" "!" ["a", "b"] ["c", "d"] `shouldBe` [("a", "c"), ("b", "d")]
    it "should fill for empty lists" $ do
        zipWithPadding "?" "!" ["a"] [] `shouldBe` [("a", "!")]
    it "should fill empty elements right" $ do
        zipWithPadding "?" "!" ["a", "b"] ["c"] `shouldBe` [("a", "c"), ("b", "!")]
    it "should fill empty elements left" $ do
        zipWithPadding "?" "!" ["a"] ["b", "c"] `shouldBe` [("a", "b"), ("?", "c")]
