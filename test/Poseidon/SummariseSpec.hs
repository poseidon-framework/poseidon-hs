module Poseidon.SummariseSpec (spec) where

import           Poseidon.CLI.Summarise

import           Test.Hspec

spec = do
    testPasteFirstN
    testFrequency
    testPrintFrequency
    testPrintFrequencyMaybe
    testMaybeShow

testPasteFirstN :: Spec
testPasteFirstN = 
    describe "Poseidon.CLI.Summarises.pasteFirstN" $ do
    it "should" $ do
        1 `shouldBe` 1

testFrequency :: Spec
testFrequency = 
    describe "Poseidon.CLI.Summarises.frequency" $ do
    it "should" $ do
        1 `shouldBe` 1

testPrintFrequency :: Spec
testPrintFrequency = 
    describe "Poseidon.CLI.Summarises.printFrequency" $ do
    it "should" $ do
        1 `shouldBe` 1

testPrintFrequencyMaybe :: Spec
testPrintFrequencyMaybe = 
    describe "Poseidon.CLI.Summarises.printFrequencyMaybe" $ do
    it "should" $ do
        1 `shouldBe` 1

testMaybeShow :: Spec
testMaybeShow = 
    describe "Poseidon.CLI.Summarises.maybeShow" $ do
    it "should" $ do
        1 `shouldBe` 1