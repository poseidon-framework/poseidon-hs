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
testJannoNormal = "test/testDat/testJannoFiles/normal.janno"

testJannoMinimal :: FilePath
testJannoMinimal = "test/testDat/testJannoFiles/minimal.janno"

testRenderJannoCompleteness :: Spec
testRenderJannoCompleteness = 
    describe "Poseidon.CLI.Survey.renderJannoCompleteness" $ do
    it "should work for a full janno file" $ do
        eJRs <- readJannoFile testJannoNormal
        let jRs = rights eJRs
        renderJannoCompleteness jRs 
            `shouldBe` 
            "MXXXXXXXXXXXXXXXXXMMXXXXXXXXXXXXXXX"
    it "should work for a minimum janno file" $ do
        eJRs <- readJannoFile testJannoMinimal
        let jRs = rights eJRs
        renderJannoCompleteness jRs 
            `shouldBe`
            "M.................MM..............."
