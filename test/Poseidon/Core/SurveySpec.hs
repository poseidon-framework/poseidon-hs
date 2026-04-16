module Poseidon.Core.SurveySpec (spec) where

import           Poseidon.CLI.Trident.Survey
import           Poseidon.Core.Janno
import           Poseidon.Core.PoseidonVersion

import           Poseidon.Core.Utils           (testLog)
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
    describe "Poseidon.CLI.Trident.Survey.renderJannoCompleteness" $ do
    it "should work for a full janno file" $ do
        janno <- testLog $ readJannoFile latestPoseidonVersion [] testJannoNormal
        renderJannoCompleteness janno
            `shouldBe` "███..▓.██▓..▒▒▒▒███████▒▒▒███.████▓█████████▓▓▓███▓▓"
    it "should work for a minimum janno file" $ do
        janno <- testLog $ readJannoFile latestPoseidonVersion [] testJannoMinimal
        renderJannoCompleteness janno
            `shouldBe`
            "███................................................."
