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
    "run a whole CLI pipeline" $ do
        it "should produce the expected output files" $ do
            -- perform actions
            createDynamicCheckSumFile
            static <- readFile staticCheckSumFile
            let static_list = lines static
            dynamic <- readFile dynamicCheckSumFile
            let dynamic_list = lines dynamic
            -- test outcome
            dynamic_list `shouldBe` static_list