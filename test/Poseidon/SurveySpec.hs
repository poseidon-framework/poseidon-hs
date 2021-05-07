module Poseidon.SurveySpec (spec) where

import           Poseidon.CLI.Survey
import           Poseidon.Janno
import           Poseidon.Package

import           Data.Either         (rights)
import           Data.Maybe          (catMaybes)
import           Test.Hspec

spec = do
    testRenderJannoCompleteness

testJannoNormal :: FilePath
testJannoNormal = "test/testDat/testJannoFiles/normal_full.janno"

testJannoMinimal :: FilePath
testJannoMinimal = "test/testDat/testJannoFiles/minimal_full.janno"

testRenderJannoCompleteness :: Spec
testRenderJannoCompleteness = 
    describe "Poseidon.CLI.Survey.renderJannoCompleteness" $ do
    it "should work for a full janno file" $ do
        janno <- readJannoFile testJannoNormal
        renderJannoCompleteness janno 
            `shouldBe` 
            "M.XXXXXXXXXXXXXXXXMMXXXXXXXXXXXXXXXXX"
    it "should work for a minimum janno file" $ do
        janno <- readJannoFile testJannoMinimal
        renderJannoCompleteness janno
            `shouldBe`
            "M.................MM................."
