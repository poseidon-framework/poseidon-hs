module Poseidon.GoldenTestsValidateChecksumsSpec (spec) where

import           Poseidon.GoldenTestsRunCommands    (createDynamicCheckSumFile, 
                                                     staticCheckSumFile, 
                                                     dynamicCheckSumFile)

import           Test.Hspec

spec :: Spec
spec = do
    testCommandsAndValidateChecksums

testCommandsAndValidateChecksums :: Spec
testCommandsAndValidateChecksums = describe 
    "buhu" $ do
        it "huhu" $ do
            -- perform actions
            createDynamicCheckSumFile
            static <- readFile staticCheckSumFile
            dynamic <- readFile dynamicCheckSumFile
            -- test outcome
            dynamic `shouldBe` static