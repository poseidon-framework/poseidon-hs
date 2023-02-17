module PoseidonGoldenTests.GoldenTestsValidateChecksumsSpec (spec) where

import           PoseidonGoldenTests.GoldenTestsRunCommands (createDynamicCheckSumFile,
                                                             dynamicCheckSumFile,
                                                             staticCheckSumFile)

import           Test.Hspec

spec :: Spec
spec = do
    testCommandsAndValidateChecksums

testCommandsAndValidateChecksums :: Spec
testCommandsAndValidateChecksums = describe
    "running a CLI pipeline for a frontend test" $ do
        it "should produce the expected output files" $ do
            -- perform actions
            createDynamicCheckSumFile
            static <- readFile staticCheckSumFile
            let static_list = lines static
            dynamic <- readFile dynamicCheckSumFile
            let dynamic_list = lines dynamic
            -- test outcome
            dynamic_list `shouldBe` static_list
