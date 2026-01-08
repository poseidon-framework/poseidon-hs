module Poseidon.SurveySpec (spec) where

import           Poseidon.CLI.Survey
import           Poseidon.Janno

import           Poseidon.Utils      (testLog)
import           Test.Hspec

spec :: Spec
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
        janno <- testLog $ readJannoFile [] testJannoNormal
        renderJannoCompleteness janno
            `shouldBe` "███..▓.██▓..▒▒▒▒███████▒▒▒███.████▓█████████▓▓▓███▓▓"
    it "should work for a minimum janno file" $ do
        janno <- testLog $ readJannoFile [] testJannoMinimal
        renderJannoCompleteness janno
            `shouldBe`
            "███................................................."
