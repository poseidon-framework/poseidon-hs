module Poseidon.SummariseSpec (spec) where

import Poseidon.CLI.Summarise

import           Test.Hspec

spec = do
    testPasteFirstN
    testFrequency
    testPrintFrequency
    testPrintFrequencyMaybe
    testMaybeShow

testPasteFirstN :: Spec
testPasteFirstN = 
    describe "Poseidon.CLI.Summarise.pasteFirstN" $ do
    it "should concat 1/1 correctly" $ do
        pasteFirstN 1 ["a"] `shouldBe` "a"
    it "should concat 2/1 correctly" $ do
        pasteFirstN 2 ["a"] `shouldBe` "a"
    it "should concat 1/2 correctly" $ do
        pasteFirstN 1 ["a", "b"] `shouldBe` "a, ..."
    it "should concat 2/2 correctly" $ do
        pasteFirstN 2 ["a", "b"] `shouldBe` "a, b"

testFrequency :: Spec
testFrequency = 
    describe "Poseidon.CLI.Summarise.frequency" $ do
    it "should" $ do
        1 `shouldBe` 1

testPrintFrequency :: Spec
testPrintFrequency = 
    describe "Poseidon.CLI.Summarise.printFrequency" $ do
    it "should" $ do
        1 `shouldBe` 1

testPrintFrequencyMaybe :: Spec
testPrintFrequencyMaybe = 
    describe "Poseidon.CLI.Summarise.printFrequencyMaybe" $ do
    it "should" $ do
        1 `shouldBe` 1

testMaybeShow :: Spec
testMaybeShow = 
    describe "Poseidon.CLI.Summarise.maybeShow" $ do
    it "should" $ do
        1 `shouldBe` 1