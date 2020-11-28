module Poseidon.JannoSpec (spec) where

import           Poseidon.Janno            (loadJannoFile)

import qualified Data.ByteString.Char8     as B
import           Test.Hspec

spec = do
    testPoseidonSampleFromJannoFile

testPoseidonSampleFromJannoFile :: Spec
testPoseidonSampleFromJannoFile = describe "Poseidon.Janno.loadJannoFile" $ do
    let minimalJannoPath = "test/testDat/testJannoFiles/minimal.janno"
    it "should read a minimal janno file correctly" $ do
        janno <- loadJannoFile minimalJannoPath
        length janno `shouldBe` 10
        