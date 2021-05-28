module Poseidon.SummariseSpec (spec) where

import           Poseidon.CLI.Summarise

import           Test.Hspec

spec :: Spec
spec = do
    testPaste
    testFrequency
    testPrintFrequency
    testPrintFrequencyMaybe
    testMaybeShow

testPaste :: Spec
testPaste = 
    describe "Poseidon.CLI.Summarise.paste" $ do
    it "should deal with an empty list correctly" $ do
        paste [] `shouldBe` "no values"
    it "should display singular elements correctly" $ do
        paste ["a"] `shouldBe` "a"
    it "should concat multiple elements correctly" $ do
        paste ["a", "b", "c"] `shouldBe` "a, b, c"

testFrequency :: Spec
testFrequency = 
    describe "Poseidon.CLI.Summarise.frequency" $ do
    it "should calculate frequencies correctly for strings" $ do
        frequency ["ab", "bc", "cd", "cd", "ab"] `shouldBe` 
            [("ab", 2), ("cd", 2), ("bc", 1)]
    it "should calculate frequencies correctly for integers" $ do
        frequency [1 :: Int, 2, 3, 1, 1] `shouldBe` 
            [(1, 3), (2, 1), (3, 1)]

testPrintFrequency :: Spec
testPrintFrequency = 
    describe "Poseidon.CLI.Summarise.printFrequency" $ do
    it "should deal with an empty list correctly" $ do
        printFrequency ":-)" ([] :: [(Int, Int)]) `shouldBe` "no values"
    it "should display frequencies correctly for strings" $ do
        printFrequency ", " (frequency ["ab", "bc", "cd", "cd", "ab"]) `shouldBe` 
            "\"ab\": 2, \"cd\": 2, \"bc\": 1"
    it "should display frequencies correctly for integers" $ do
        printFrequency " | " (frequency [1 :: Int, 2, 3, 1, 1]) `shouldBe` 
            "1: 3 | 2: 1 | 3: 1"

testPrintFrequencyMaybe :: Spec
testPrintFrequencyMaybe = 
    describe "Poseidon.CLI.Summarise.printFrequencyMaybe" $ do
    it "should deal with an empty list correctly" $ do
        printFrequencyMaybe ":-)" ([] :: [(Maybe Int, Int)]) `shouldBe` "no values"
    it "should deal with an effectivly empty list correctly" $ do
        printFrequencyMaybe ":-)" ([(Nothing, 3)] :: [(Maybe Int, Int)]) `shouldBe` "n/a: 3"
    it "should display frequencies correctly for strings" $ do
        printFrequencyMaybe ", " (frequency [Just "ab", Just "bc", Nothing, Just "ab"]) `shouldBe` 
            "\"ab\": 2, n/a: 1, \"bc\": 1"
    it "should display frequencies correctly for integers" $ do
        printFrequencyMaybe " | " (frequency [Just (1 :: Int), Just 2, Nothing, Just 1, Nothing]) `shouldBe` 
            "n/a: 2 | 1: 2 | 2: 1"

testMaybeShow :: Spec
testMaybeShow = 
    describe "Poseidon.CLI.Summarise.maybeShow" $ do
    it "should show a in case of Maybe a" $ do
        maybeShow (Just "test") `shouldBe` "\"test\""
    it "should show \"n/a\" in case of Nothing" $ do
        maybeShow (Nothing :: Maybe Int) `shouldBe` "n/a"
