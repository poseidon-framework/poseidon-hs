module Poseidon.OptParseApplicativeParsersSpec (spec) where

import           Poseidon.CLI.OptparseApplicativeParsers (splitExtensionsOptGz)

import           Test.Hspec

spec :: Spec
spec = do
    testSplitExtensionsOptGz

testSplitExtensionsOptGz :: Spec
testSplitExtensionsOptGz = describe
    "Poseidon.OptparseApplicativeParsers.testSplitExtensionsOptGz" $ do
        it "should split withempty ending" $
            splitExtensionsOptGz "myFile_noEnding" `shouldBe` ("myFile_noEnding", "")
        it "should return an single extension if not gz" $
            splitExtensionsOptGz "myFile.txt" `shouldBe` ("myFile", ".txt")
        it "...even if there are more dots" $
            splitExtensionsOptGz "myFile.double.txt" `shouldBe` ("myFile.double", ".txt")
        it "should return only gz if that's the only ending" $
            splitExtensionsOptGz "myFile.gz" `shouldBe` ("myFile", ".gz")
        it "should return two endings if ending with gz" $
            splitExtensionsOptGz "myFile.txt.gz" `shouldBe` ("myFile", ".txt.gz")
        it "even if there are more" $
            splitExtensionsOptGz "myFile.double.txt.gz" `shouldBe` ("myFile.double", ".txt.gz")
