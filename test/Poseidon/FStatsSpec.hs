module Poseidon.FStatsSpec (spec) where

import           Poseidon.FStats (FStatSpec (..), PopSpec (..), fStatSpecParser,
                                  runParser)
import           Test.Hspec

spec = do
    testParseStatSpec

testParseStatSpec :: Spec
testParseStatSpec = describe "Poseidon.FStatsSpec.fStatSpecParser" $ do
    it "should parse F4 correctly" $
        let f = F4Spec (PopSpecGroup "aa") (PopSpecInd "bb") (PopSpecGroup "cc") (PopSpecGroup "dd")
        in  runParser fStatSpecParser () "" "F4(aa,<bb>,cc,dd)" `shouldBe` Right f
    it "should parse F3 correctly" $
        let f = F3Spec (PopSpecGroup "aa") (PopSpecInd "bb") (PopSpecGroup "cc")
        in  runParser fStatSpecParser () "" "F3(aa,<bb>,cc)" `shouldBe` Right f
    it "should parse F2 correctly" $
        let f = F2Spec (PopSpecGroup "aa") (PopSpecInd "bb")
        in  runParser fStatSpecParser () "" "F2(aa,<bb>)" `shouldBe` Right f
    it "should parse PWM correctly" $
        let f = PairwiseMismatchSpec (PopSpecGroup "aa") (PopSpecInd "bb")
        in  runParser fStatSpecParser () "" "PWM(aa,<bb>)" `shouldBe` Right f


