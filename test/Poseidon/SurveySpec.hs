module Poseidon.SurveySpec (spec) where

import           Poseidon.CLI.Survey
import           Poseidon.Janno

import           Poseidon.Utils      (testLog)
import           Test.Hspec

spec :: Spec
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
        (JannoFile rows) <- testLog $ readJannoFile testJannoNormal
        renderJannoCompleteness rows
            `shouldBe`
            "\9608\9608\9608\9619\9608\9608\9619\9619.\9608\9608\9608\9608\9608\9608\9618\9618\9618\9608\9608\9608\9619\9608\9608\9608\9608\9608\9608\9608\9608\9608\9608\9608\9608\9608\9619\9619\9619\9618\9608\9608\9608\9619\9619"
    it "should work for a minimum janno file" $ do
        (JannoFile rows) <- testLog $ readJannoFile testJannoMinimal
        renderJannoCompleteness rows
            `shouldBe`
            "\9608\9608\9608........................................."
