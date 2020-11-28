module Poseidon.JannoSpec (spec) where

import           Poseidon.Janno            (PoseidonSample (..), loadJannoFile)

import qualified Data.ByteString.Char8     as B
import           Data.Either               (isRight, rights)
import           Test.Hspec

spec = do
    testPoseidonSampleFromJannoFile

testPoseidonSampleFromJannoFile :: Spec
testPoseidonSampleFromJannoFile = describe "Poseidon.Janno.loadJannoFile" $ do
    let minimalJannoPath = "test/testDat/testJannoFiles/minimal.janno"
    let normalJannoPath = "test/testDat/testJannoFiles/normal.janno"
    it "should read a minimal janno file correctly" $ do
        janno <- loadJannoFile minimalJannoPath
        length janno `shouldBe` 3
        all isRight janno `shouldBe` True
        map posSamIndividualID (rights janno) `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map posSamCollectionID (rights janno) `shouldBe` [Nothing, Nothing, Nothing]
    it "should read a normal janno file correctly" $ do
        janno <- loadJannoFile normalJannoPath
        length janno `shouldBe` 3
        all isRight janno `shouldBe` True
        map posSamIndividualID (rights janno) `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map posSamCollectionID (rights janno) `shouldBe` [Just "xxx", Just "xxx", Just "xxx"]
